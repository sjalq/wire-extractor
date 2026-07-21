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

function runExtractOnce(projectDir) {
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
      _retryable: true,
    };
  }
  // elm-review unexpected crashes surface as type:error without extracts
  if (report && report.type === "error") {
    return {
      ok: false,
      error:
        "elm-review error: " +
        (report.title || "") +
        "\n" +
        JSON.stringify(report.message || report).slice(0, 1500),
      _retryable: true,
    };
  }
  const data = report.extracts && report.extracts.ExtractWireProtocol;
  if (!data) {
    return {
      ok: false,
      error:
        "No ExtractWireProtocol extract.\n" +
        JSON.stringify(report.errors || [], null, 2).slice(0, 2000),
      _retryable: true,
    };
  }
  // Treat sample invent failures as retryable (docs index occasionally incomplete mid-rebuild)
  if (
    data.ok === false &&
    typeof data.error === "string" &&
    data.error.indexOf("unconstructable") !== -1
  ) {
    data._retryable = true;
  }
  return data;
}

function runExtract(projectDir) {
  if (!fs.existsSync(path.join(projectDir, "elm.json"))) {
    return { ok: false, error: "No elm.json in " + projectDir };
  }
  let last = null;
  for (let attempt = 1; attempt <= 3; attempt++) {
    last = runExtractOnce(projectDir);
    if (last.ok && last.elmSource && last.proofElmSource) {
      return last;
    }
    if (!last._retryable || attempt === 3) {
      return last;
    }
    // Brief pause + drop generated review app so the next attempt rebuilds cleanly
    try {
      const gen = path.join(
        projectDir,
        "elm-stuff",
        "generated-code",
        "jfmengels"
      );
      if (fs.existsSync(gen)) {
        fs.rmSync(gen, { recursive: true, force: true });
      }
    } catch {
      /* ignore */
    }
  }
  return last;
}

/**
 * Ensure the app can host Protocol in tests/ (source-directories only).
 * Never bump/remove the app's test packages — that fights avh4/elm-program-test
 * and other pins on elm-explorations/test 1.x.
 */
function ensureTestsSourceDir(projectDir) {
  const elmJsonPath = path.join(projectDir, "elm.json");
  const elmJson = JSON.parse(fs.readFileSync(elmJsonPath, "utf8"));
  let dirty = false;
  if (!Array.isArray(elmJson["source-directories"])) {
    elmJson["source-directories"] = ["src"];
    dirty = true;
  }
  if (!elmJson["source-directories"].includes("tests")) {
    elmJson["source-directories"].push("tests");
    dirty = true;
  }
  if (dirty) {
    fs.writeFileSync(elmJsonPath, JSON.stringify(elmJson, null, 4) + "\n");
  }
  return elmJson;
}

/**
 * Isolated prove sandbox: same app sources + only elm-explorations/test 2.x.
 * Avoids dep conflicts with avh4/elm-program-test (test 1.x) in the host app.
 */
function prepareProveSandbox(projectDir, testsDirName) {
  const parentElm = JSON.parse(
    fs.readFileSync(path.join(projectDir, "elm.json"), "utf8")
  );
  const sandboxDir = path.join(projectDir, ".wire-extractor-prove");
  fs.mkdirSync(sandboxDir, { recursive: true });

  const srcDirs = Array.isArray(parentElm["source-directories"])
    ? parentElm["source-directories"]
    : ["src"];
  // Parent source dirs + tests, all relative to sandbox via ../
  const sandboxSrc = [];
  for (const d of srcDirs) {
    if (d === "tests" || d === testsDirName) continue;
    sandboxSrc.push(path.posix.join("..", d.split(path.sep).join("/")));
  }
  sandboxSrc.push(path.posix.join("..", testsDirName.split(path.sep).join("/")));

  const deps = parentElm.dependencies || { direct: {}, indirect: {} };
  // Ensure elm/random available (test package indirect); never pull test 1.x pins.
  const indirect = { ...(deps.indirect || {}) };
  if (!indirect["elm/random"]) indirect["elm/random"] = "1.0.0";

  const sandboxElm = {
    type: "application",
    "source-directories": sandboxSrc,
    "elm-version": parentElm["elm-version"] || "0.19.1",
    dependencies: {
      direct: { ...(deps.direct || {}) },
      indirect,
    },
    "test-dependencies": {
      direct: {
        "elm-explorations/test": "2.2.0",
      },
      indirect: {},
    },
  };

  fs.writeFileSync(
    path.join(sandboxDir, "elm.json"),
    JSON.stringify(sandboxElm, null, 4) + "\n"
  );
  return sandboxDir;
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
  ensureTestsSourceDir(args.project);

  console.log("3/3 prove (elm-test-rs --compiler lamdera, isolated sandbox)");
  const sandboxDir = prepareProveSandbox(args.project, args.testsDir);
  // Proof path relative to sandbox: ../tests/ProtocolWireProof.elm
  const relProof = path
    .relative(sandboxDir, proofPath)
    .split(path.sep)
    .join("/");

  // Class C: under elm-test-rs, Lamdera specializes Lamdera.sendToBackend to
  // Types.ToBackend -> Cmd, which fails to unify with Bridge.ToBackend even when
  // Types.ToBackend is an alias. Frontend entry compiles fine; test entry does not.
  // Temporarily make sendToBackend polymorphic for the prove compile only.
  const bridgePatches = patchSendToBackendForProve(args.project);
  let result;
  try {
    result = spawnSync(
      findBin("elm-test-rs"),
      ["--compiler", "lamdera", relProof],
      {
        cwd: sandboxDir,
        encoding: "utf8",
        maxBuffer: 64 * 1024 * 1024,
        env: process.env,
        stdio: "inherit",
      }
    );
  } finally {
    restorePatches(bridgePatches);
  }
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

/**
 * Find Bridge-like modules that bind sendToBackend = Lamdera.sendToBackend and
 * temporarily widen the type so elm-test-rs can compile the app graph.
 * Returns [{ path, original }] for restorePatches.
 */
function patchSendToBackendForProve(projectDir) {
  const patches = [];
  const srcRoot = path.join(projectDir, "src");
  if (!fs.existsSync(srcRoot)) return patches;

  const walk = (dir) => {
    for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
      const p = path.join(dir, ent.name);
      if (ent.isDirectory()) {
        if (ent.name === "Evergreen" || ent.name === "elm-stuff") continue;
        walk(p);
      } else if (ent.name.endsWith(".elm")) {
        const original = fs.readFileSync(p, "utf8");
        if (
          !/sendToBackend\s*=/.test(original) ||
          !/Lamdera\.sendToBackend/.test(original)
        ) {
          continue;
        }
        // Replace `sendToBackend = Lamdera.sendToBackend` (with optional type ann)
        const patched = original.replace(
          /(?:sendToBackend\s*:\s*[^\n]+\n)?sendToBackend\s*=\s*\n\s*Lamdera\.sendToBackend/g,
          "sendToBackend : a -> Cmd msg\nsendToBackend _ =\n    Cmd.none"
        );
        if (patched !== original) {
          patches.push({ path: p, original });
          fs.writeFileSync(p, patched);
        }
      }
    }
  };
  try {
    walk(srcRoot);
  } catch {
    /* ignore */
  }
  return patches;
}

function restorePatches(patches) {
  for (const { path: p, original } of patches) {
    try {
      fs.writeFileSync(p, original);
    } catch {
      /* ignore */
    }
  }
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
