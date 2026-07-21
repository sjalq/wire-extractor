#!/usr/bin/env node
/**
 * lamdera-protocol — freeze ToBackend/ToFrontend as one Protocol.elm module
 * and prove Wire3 identity with property-based tests.
 *
 * Commands:
 *   extract   Write Protocol.elm (+ optional proof module)
 *   prove     Extract, write Protocol + ProtocolWireProof, run elm-test-rs
 */

const { spawnSync } = require("child_process");
const fs = require("fs");
const path = require("path");

const TOOL_ROOT = path.resolve(__dirname, "..");
const REVIEW_CONFIG = path.join(TOOL_ROOT, "review");

function usage() {
  console.log(`lamdera-protocol — Wire3 protocol extract + proof for Lamdera apps

Usage:
  lamdera-protocol extract [--project DIR] [-o Protocol.elm] [--proof FILE] [--json]
  lamdera-protocol prove   [--project DIR] [--tests-dir tests]

extract
  Runs pure Elm (elm-review + elm-syntax) over the target app and writes
  Protocol.elm: ToBackend, ToFrontend, and transitive wire payload types only.

prove
  Writes Protocol.elm and ProtocolWireProof.elm into --tests-dir, ensures
  elm-explorations/test is available, then runs:
    elm-test-rs --compiler lamdera <tests-dir>/ProtocolWireProof.elm

  Proofs (generated in Elm):
    • exhaustive minimal sample per root constructor
    • property-based fuzz for constructors with fuzzable kernel args
    • path: protocol-encode → app-decode → app-encode → protocol-decode
    • Wire3 byte lists must match; codecs are real Lamdera w3_* functions

Options:
  --project DIR     Lamdera app root (default: cwd)
  -o, --output FILE Protocol.elm path (extract; default: Protocol.elm)
  --proof FILE      Also write ProtocolWireProof.elm (extract)
  --tests-dir DIR   Where prove writes modules (default: tests)
  --json            Print extract JSON to stdout (extract)
  --allow-unresolved  extract: exit 0 even if some types unresolved
  -h, --help
`);
}

function parseArgs(argv) {
  const args = {
    command: null,
    project: process.cwd(),
    output: "Protocol.elm",
    proof: null,
    testsDir: "tests",
    json: false,
    allowUnresolved: false,
  };
  const rest = [...argv];
  if (!rest.length || rest[0] === "-h" || rest[0] === "--help") {
    args.command = "help";
    return args;
  }
  args.command = rest.shift();
  while (rest.length) {
    const a = rest.shift();
    if (a === "--project") args.project = path.resolve(rest.shift());
    else if (a === "-o" || a === "--output") args.output = rest.shift();
    else if (a === "--proof") args.proof = rest.shift();
    else if (a === "--tests-dir") args.testsDir = rest.shift();
    else if (a === "--json") args.json = true;
    else if (a === "--allow-unresolved") args.allowUnresolved = true;
    else if (a === "-h" || a === "--help") args.command = "help";
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

function resolveOut(projectDir, filePath) {
  return path.isAbsolute(filePath) ? filePath : path.join(projectDir, filePath);
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

function cmdExtract(args) {
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
    if (data.unresolved && data.unresolved.length) {
      console.log("  unresolved:", data.unresolved.join(", "));
    }
  }
  if (args.proof) {
    if (!data.proofElmSource) {
      console.error("No proofElmSource in extract");
      process.exit(1);
    }
    const proofOut = resolveOut(args.project, args.proof);
    fs.writeFileSync(proofOut, data.proofElmSource);
    console.log("Wrote", proofOut);
  }
  if (!data.ok && !args.allowUnresolved) {
    console.error("ERROR:", data.error || "extract ok=false");
    process.exit(1);
  }
}

function cmdProve(args) {
  const data = runExtract(args.project);
  if (!data.ok || !data.elmSource || !data.proofElmSource) {
    console.error("Extract failed:", data.error || "incomplete extract");
    process.exit(1);
  }
  const testsDir = resolveOut(args.project, args.testsDir);
  fs.mkdirSync(testsDir, { recursive: true });
  const protocolPath = path.join(testsDir, "Protocol.elm");
  const proofPath = path.join(testsDir, "ProtocolWireProof.elm");
  fs.writeFileSync(protocolPath, data.elmSource);
  fs.writeFileSync(proofPath, data.proofElmSource);
  console.log("Wrote", protocolPath);
  console.log("Wrote", proofPath);
  ensureTestDeps(args.project);

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
  process.exit(result.status === 0 ? 0 : result.status || 1);
}

function main() {
  const args = parseArgs(process.argv.slice(2));
  if (args.command === "help" || !args.command) {
    usage();
    process.exit(args.command === "help" ? 0 : 2);
  }
  if (args.command === "extract") cmdExtract(args);
  else if (args.command === "prove") cmdProve(args);
  else {
    console.error("Unknown command:", args.command);
    usage();
    process.exit(2);
  }
}

main();
