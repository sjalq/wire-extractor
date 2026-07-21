#!/usr/bin/env node
/**
 * Regression checks for Class A (ctor collisions) and Class B (opaque samples).
 * Drives the real CLI; no reimplementation of invent logic.
 *
 * Usage:
 *   node scripts/verify-class-ab.js
 *   node scripts/verify-class-ab.js --git-root ~/git
 */
const { spawnSync } = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const TOOL = path.resolve(__dirname, "..");
const CLI = path.join(TOOL, "bin", "wire-extractor.js");

function parseArgs(argv) {
  let gitRoot = path.join(os.homedir(), "git");
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--git-root") gitRoot = path.resolve(argv[++i]);
  }
  return { gitRoot };
}

function run(project) {
  const r = spawnSync(process.execPath, [CLI, "--project", project], {
    cwd: TOOL,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  const out = (r.stdout || "") + (r.stderr || "");
  return {
    status: r.status,
    out,
    passed: /Wire proof PASSED/i.test(out),
  };
}

function extractOnly(project) {
  const r = spawnSync(
    process.execPath,
    [CLI, "extract-only", "--project", project, "--json"],
    { cwd: TOOL, encoding: "utf8", maxBuffer: 64 * 1024 * 1024 }
  );
  const raw = (r.stdout || "") + (r.stderr || "");
  const start = raw.indexOf("{");
  if (start < 0) return { ok: false, error: raw.slice(0, 400) };
  try {
    // first JSON object only
    let depth = 0;
    let end = start;
    for (let i = start; i < raw.length; i++) {
      if (raw[i] === "{") depth++;
      if (raw[i] === "}") {
        depth--;
        if (depth === 0) {
          end = i + 1;
          break;
        }
      }
    }
    return JSON.parse(raw.slice(start, end));
  } catch (e) {
    return { ok: false, error: e.message + "\n" + raw.slice(0, 400) };
  }
}

function main() {
  const { gitRoot } = parseArgs(process.argv.slice(2));
  const apps = [
    {
      name: "class-a-justice",
      dir: path.join(gitRoot, "cause-for-justice-donations"),
      classA: true,
    },
    {
      name: "class-a-ernst",
      dir: path.join(gitRoot, "ernst-donate"),
      classA: true,
    },
    {
      name: "class-b-dashboard",
      dir: path.join(gitRoot, "lamdera-infra", "dashboard-sanitised"),
      classB: true,
    },
  ];

  let failed = 0;
  for (const app of apps) {
    if (!fs.existsSync(path.join(app.dir, "elm.json"))) {
      console.error("SKIP missing", app.dir);
      continue;
    }
    console.log("extract", app.name);
    const ex = extractOnly(app.dir);
    if (app.classA) {
      const err = ex.error || "";
      if (/Constructor name collision/i.test(err)) {
        console.error("FAIL Class A still reports collision:", err);
        failed++;
        continue;
      }
    }
    if (ex.ok === false && !ex.elmSource) {
      console.error("FAIL extract", app.name, ex.error);
      failed++;
      continue;
    }
    // proof file after full run
    console.log("prove", app.name);
    const pr = run(app.dir);
    if (!pr.passed || pr.status !== 0) {
      console.error("FAIL prove", app.name, pr.out.slice(-400));
      failed++;
      continue;
    }
    const proof = path.join(app.dir, "tests", "ProtocolWireProof.elm");
    if (fs.existsSync(proof)) {
      const text = fs.readFileSync(proof, "utf8");
      if (text.includes("Debug.todo")) {
        console.error("FAIL Debug.todo in", proof);
        failed++;
        continue;
      }
      if (app.classB && !/sessionIdFromString|clientIdFromString/.test(text)) {
        // dashboard should invent SessionId via docs
        if (text.includes("SessionId") || text.includes("sessionId")) {
          console.error("FAIL Class B missing docs sample for SessionId");
          failed++;
          continue;
        }
      }
    }
    console.log("OK", app.name);
  }

  // no hardwires
  const srcs = [
    path.join(TOOL, "review", "src"),
    path.join(TOOL, "bin"),
  ];
  for (const root of srcs) {
    const r = spawnSync(
      "rg",
      ["-n", "cause-for-justice|ernst-donate|dashboard-sanitised", root],
      { encoding: "utf8" }
    );
    if (r.stdout && r.stdout.trim()) {
      console.error("FAIL hardwires:\n", r.stdout);
      failed++;
    }
  }

  if (failed) {
    console.error("FAILED", failed);
    process.exit(1);
  }
  console.log("All Class A/B checks passed");
}

main();
