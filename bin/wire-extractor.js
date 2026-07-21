#!/usr/bin/env node
/**
 * wire-extractor — extract Protocol.elm, generate Wire3 proofs, run them.
 * Default: full pipeline (extract + write + prove).
 */

const { spawnSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const TOOL_ROOT = path.resolve(__dirname, "..");
const REVIEW_CONFIG = path.join(TOOL_ROOT, "review");

function usage() {
  console.log(`wire-extractor — Lamdera ToBackend/ToFrontend wire freeze + Wire3 proof

  wire-extractor [--project DIR] [--tests-dir tests]
      Extract Protocol.elm + ProtocolWireProof.elm and run property/exhaustive
      Wire3 identity tests (exit non-zero if any fail).

  wire-extractor extract-only [--project DIR] [-o Protocol.elm] [--json]
      Write Protocol.elm only (no tests).

Needs: elm-review, elm-test-rs, lamdera
`);
}

function parseArgs(argv) {
  const args = {
    mode: "run", // run | extract-only | help
    project: process.cwd(),
    output: "Protocol.elm",
    testsDir: "tests",
    json: false,
    allowUnresolved: false,
  };
  const rest = [...argv];
  if (rest[0] === "-h" || rest[0] === "--help") {
    args.mode = "help";
    return args;
  }
  if (rest[0] === "extract-only" || rest[0] === "extract") {
    args.mode = "extract-only";
    rest.shift();
  } else if (rest[0] === "prove" || rest[0] === "run") {
    // aliases for default full pipeline
    args.mode = "run";
    rest.shift();
  }
  while (rest.length) {
    const a = rest.shift();
    if (a === "--project") args.project = path.resolve(rest.shift());
    else if (a === "-o" || a === "--output") args.output = rest.shift();
    else if (a === "--tests-dir") args.testsDir = rest.shift();
    else if (a === "--json") args.json = true;
    else if (a === "--allow-unresolved") args.allowUnresolved = true;
    else if (a === "-h" || a === "--help") args.mode = "help";
    else {
      console.error("Unknown argument:", a);
      process.exit(2);
    }
  }
  return args;
}

function findBin(name) {
  const candidates = [
    path.join(process.cwd(), "node_modules", ".bin", name),
    path.join(TOOL_ROOT, "node_modules", ".bin", name),
    name,
  ];
  for (const c of candidates) {
    if (c === name || fs.existsSync(c)) return c;
  }
  return name;
}

function resolveOut(projectDir, filePath) {
  return path.isAbsolute(filePath) ? filePath : path.join(projectDir, filePath);
}

function runExtract(projectDir) {
  if (!fs.existsSync(path.join(projectDir, "elm.json"))) {
    return { ok: false, error: "No elm.json in " + projectDir };
  }
  const result = spawnSync(
    findBin("elm-review"),
    [
      "--config",
      REVIEW_CONFIG,
      "--report=json",
      "--extract",
      "--rules",
      "ExtractWireProtocol",
    ],
    {
      cwd: projectDir,
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
      env: process.env,
    }
  );
  if (result.error) {
    return {
      ok: false,
      error:
        "Failed to run elm-review: " +
        result.error.message +
        " (install: npm i -g elm-review)",
    };
  }
  let report;
  try {
    report = JSON.parse(result.stdout || "");
  } catch (e) {
    return {
      ok: false,
      error:
        "elm-review did not return JSON.\n" +
        (result.stderr || result.stdout || "").slice(0, 2000),
    };
  }
  const data = report.extracts && report.extracts.ExtractWireProtocol;
  if (!data) {
    return {
      ok: false,
      error:
        "No ExtractWireProtocol extract.\n" +
        JSON.stringify(report.errors || [], null, 2).slice(0, 2000),
    };
  }
  return data;
}

function ensureTestDeps(projectDir) {
  const elmJsonPath = path.join(projectDir, "elm.json");
  const elmJson = JSON.parse(fs.readFileSync(elmJsonPath, "utf8"));
  let dirty = false;
  if (!Array.isArray(elmJson["source-directories"])) {
    elmJson["source-directories"] = ["src"];
  }
  if (!elmJson["source-directories"].includes("tests")) {
    elmJson["source-directories"].push("tests");
    dirty = true;
  }
  if (!elmJson["test-dependencies"]) {
    elmJson["test-dependencies"] = { direct: {}, indirect: {} };
  }
  if (!elmJson["test-dependencies"].direct) {
    elmJson["test-dependencies"].direct = {};
  }
  if (!elmJson["test-dependencies"].direct["elm-explorations/test"]) {
    elmJson["test-dependencies"].direct["elm-explorations/test"] = "2.2.0";
    dirty = true;
  }
  if (dirty) {
    fs.writeFileSync(elmJsonPath, JSON.stringify(elmJson, null, 4) + "\n");
  }
}

function cmdExtractOnly(args) {
  const data = runExtract(args.project);
  if (args.json) {
    process.stdout.write(JSON.stringify(data, null, 2) + "\n");
  }
  if (!data.elmSource) {
    console.error("Extract failed:", data.error || "no elmSource");
    process.exit(1);
  }
  if (!args.json) {
    const out = resolveOut(args.project, args.output);
    fs.writeFileSync(out, data.elmSource);
    console.log("Wrote", out);
    console.log("  roots:", data.rootToBackend, "/", data.rootToFrontend);
    console.log("  included:", data.includedCount, "types");
  }
  if (!data.ok && !args.allowUnresolved) {
    console.error("ERROR:", data.error || "extract ok=false");
    process.exit(1);
  }
}

/** Full pipeline: extract → write Protocol + proof → elm-test-rs */
function cmdRun(args) {
  console.log("1/3 extract");
  const data = runExtract(args.project);
  if (!data.ok || !data.elmSource || !data.proofElmSource) {
    console.error("Extract failed:", data.error || "incomplete extract");
    process.exit(1);
  }

  console.log("2/3 write Protocol.elm + ProtocolWireProof.elm");
  const testsDir = resolveOut(args.project, args.testsDir);
  fs.mkdirSync(testsDir, { recursive: true });
  const protocolPath = path.join(testsDir, "Protocol.elm");
  const proofPath = path.join(testsDir, "ProtocolWireProof.elm");
  fs.writeFileSync(protocolPath, data.elmSource);
  fs.writeFileSync(proofPath, data.proofElmSource);
  console.log("  ", protocolPath);
  console.log("  ", proofPath);
  console.log(
    "  roots:",
    data.rootToBackend,
    "/",
    data.rootToFrontend,
    " types:",
    data.includedCount
  );
  ensureTestDeps(args.project);

  console.log("3/3 prove (elm-test-rs --compiler lamdera)");
  const relProof = path.relative(args.project, proofPath) || proofPath;
  const result = spawnSync(
    findBin("elm-test-rs"),
    ["--compiler", "lamdera", relProof],
    {
      cwd: args.project,
      encoding: "utf8",
      maxBuffer: 64 * 1024 * 1024,
      env: process.env,
      stdio: "inherit",
    }
  );
  if (result.error) {
    console.error(
      "Failed to run elm-test-rs:",
      result.error.message,
      "\nInstall: cargo install elm-test-rs  (and lamdera compiler)"
    );
    process.exit(1);
  }
  if (result.status !== 0) {
    console.error("Wire proof FAILED");
    process.exit(result.status || 1);
  }
  console.log("Wire proof PASSED");
}

function main() {
  const args = parseArgs(process.argv.slice(2));
  if (args.mode === "help") {
    usage();
    process.exit(0);
  }
  if (args.mode === "extract-only") cmdExtractOnly(args);
  else cmdRun(args);
}

main();
