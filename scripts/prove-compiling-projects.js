#!/usr/bin/env node
/**
 * Multi-app smoke harness (NOT the main CLI).
 *
 * Finds compiling Lamdera apps under a git root (default: ~/git), runs the
 * full wire-extractor pipeline on each, prints a summary.
 *
 * Usage:
 *   node scripts/prove-compiling-projects.js
 *   node scripts/prove-compiling-projects.js --git-root ~/git
 *   node scripts/prove-compiling-projects.js --only starter-project,telebot
 *   node scripts/prove-compiling-projects.js --skip-compile-check   # use cached inventory or all candidates
 */

const { spawnSync } = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const TOOL_ROOT = path.resolve(__dirname, "..");
const CLI = path.join(TOOL_ROOT, "bin", "wire-extractor.js");
const RUNS_DIR = path.join(__dirname, ".runs");

function parseArgs(argv) {
  const args = {
    gitRoot: path.join(os.homedir(), "git"),
    only: null,
    skipCompileCheck: false,
  };
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--git-root") args.gitRoot = path.resolve(argv[++i]);
    else if (argv[i] === "--only")
      args.only = new Set(argv[++i].split(",").map((s) => s.trim()));
    else if (argv[i] === "--skip-compile-check") args.skipCompileCheck = true;
    else if (argv[i] === "-h" || argv[i] === "--help") {
      console.log(`prove-compiling-projects.js — multi-app harness for wire-extractor

  node scripts/prove-compiling-projects.js [--git-root DIR] [--only a,b] [--skip-compile-check]

Scans DIR for Lamdera apps with ToBackend, optionally checks they compile,
then runs: node bin/wire-extractor.js --project <app>

Results under scripts/.runs/
`);
      process.exit(0);
    }
  }
  return args;
}

function run(cmd, argv, opts = {}) {
  return spawnSync(cmd, argv, {
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
    ...opts,
  });
}

function hasToBackend(projectDir) {
  const src = path.join(projectDir, "src");
  if (!fs.existsSync(src)) return false;
  const walk = (dir) => {
    for (const ent of fs.readdirSync(dir, { withFileTypes: true })) {
      const p = path.join(dir, ent.name);
      if (ent.isDirectory()) {
        if (ent.name === "Evergreen" || ent.name === "elm-stuff") continue;
        if (walk(p)) return true;
      } else if (ent.name.endsWith(".elm")) {
        const t = fs.readFileSync(p, "utf8");
        if (/\btype\s+ToBackend\b/.test(t)) return true;
      }
    }
    return false;
  };
  try {
    return walk(src);
  } catch {
    return false;
  }
}

function looksLikeLamdera(projectDir) {
  const ej = path.join(projectDir, "elm.json");
  try {
    if (!fs.existsSync(ej) || !fs.statSync(ej).isFile()) return false;
    const raw = fs.readFileSync(ej, "utf8");
    // Real Lamdera apps ship lamdera/core and/or codecs (needed for Wire3 proofs).
    return (
      (raw.includes("lamdera/core") || raw.includes("lamdera/codecs")) &&
      hasToBackend(projectDir)
    );
  } catch {
    return false;
  }
}

function compileOk(projectDir) {
  let entry = null;
  for (const c of ["src/Backend.elm", "src/Frontend.elm", "src/Types.elm"]) {
    if (fs.existsSync(path.join(projectDir, c))) {
      entry = c;
      break;
    }
  }
  if (!entry) return { ok: false, reason: "no entry module" };
  const compiler = run("which", ["lamdera"]).status === 0 ? "lamdera" : "elm";
  const r = run(compiler, ["make", entry, "--output=/dev/null"], {
    cwd: projectDir,
  });
  return {
    ok: r.status === 0,
    reason:
      r.status === 0
        ? null
        : ((r.stderr || r.stdout || "").split("\n").slice(0, 3).join(" ") ||
            "compile failed"
          ).slice(0, 200),
  };
}

function listCandidates(gitRoot, maxDepth = 5) {
  if (!fs.existsSync(gitRoot)) return [];
  const out = [];
  const seen = new Set();
  const skipDirNames = new Set([
    "auth",
    "node_modules",
    "elm-stuff",
    ".git",
    "vendor",
    "review",
    "tests",
    "dist",
    "build",
    "target",
    ".stack-work",
    "coverage",
    "__pycache__",
    ".runs",
  ]);

  const walk = (dir, rel, depth) => {
    if (depth > maxDepth) return;
    let ents;
    try {
      ents = fs.readdirSync(dir, { withFileTypes: true });
    } catch {
      return;
    }

    // Prefer the current dir as a Lamdera app when it has elm.json;
    // still walk children for nested apps (e.g. monorepos).
    if (fs.existsSync(path.join(dir, "elm.json"))) {
      if (
        !seen.has(dir) &&
        looksLikeLamdera(dir) &&
        hasToBackend(dir)
      ) {
        seen.add(dir);
        out.push({ name: rel || path.basename(dir), dir });
      }
    }

    for (const ent of ents) {
      if (!ent.isDirectory()) continue;
      if (skipDirNames.has(ent.name) || ent.name.startsWith(".")) continue;
      walk(
        path.join(dir, ent.name),
        rel ? `${rel}/${ent.name}` : ent.name,
        depth + 1
      );
    }
  };

  walk(gitRoot, "", 0);
  out.sort((a, b) => a.name.localeCompare(b.name));
  return out;
}

function proveProject(projectDir) {
  const r = run(process.execPath, [CLI, "--project", projectDir], {
    cwd: TOOL_ROOT,
    env: process.env,
  });
  const out = (r.stdout || "") + "\n" + (r.stderr || "");
  const passed = /Wire proof PASSED/i.test(out) || /TEST RUN PASSED/i.test(out);
  const m = out.match(/Passed:\s*(\d+)/);
  return {
    status: r.status,
    passed,
    tests: m ? Number(m[1]) : null,
    out,
  };
}

function main() {
  const args = parseArgs(process.argv.slice(2));
  fs.mkdirSync(RUNS_DIR, { recursive: true });
  const consoleLogPath = path.join(RUNS_DIR, "console.log");
  const lines = [];
  const log = (s) => {
    console.log(s);
    lines.push(s);
  };

  let candidates = listCandidates(args.gitRoot);
  if (args.only) {
    candidates = candidates.filter((c) => args.only.has(c.name));
  }

  log(`git root: ${args.gitRoot}`);
  log(`candidates with ToBackend: ${candidates.length}`);

  const compiling = [];
  for (const c of candidates) {
    if (args.skipCompileCheck) {
      compiling.push(c);
      continue;
    }
    process.stdout.write(`compile-check ${c.name} ... `);
    const { ok, reason } = compileOk(c.dir);
    if (ok) {
      console.log("OK");
      compiling.push(c);
      lines.push(`COMPILE_OK ${c.name}`);
    } else {
      console.log("FAIL");
      lines.push(`COMPILE_FAIL ${c.name} :: ${reason}`);
    }
  }

  log(`\nProving ${compiling.length} compiling projects\n`);

  const results = [];
  for (const c of compiling) {
    log(`======== ${c.name} ========`);
    const runDir = path.join(RUNS_DIR, c.name);
    fs.mkdirSync(runDir, { recursive: true });
    try {
      const res = proveProject(c.dir);
      fs.writeFileSync(path.join(runDir, "wire-extractor.log"), res.out);
      if (res.passed && res.status === 0) {
        log(`PROOF_OK tests=${res.tests ?? "?"}`);
        results.push({
          name: c.name,
          status: "proof_ok",
          tests: res.tests,
        });
      } else {
        const tail = res.out.slice(-800).replace(/\n/g, " ");
        log(`PROOF_FAIL ${tail.slice(0, 300)}`);
        results.push({
          name: c.name,
          status: "proof_fail",
          reason: tail.slice(0, 400),
        });
      }
    } catch (e) {
      log(`ERROR ${e.message}`);
      results.push({ name: c.name, status: "error", reason: e.message });
    }
  }

  log("\n======== SUMMARY ========");
  const counts = {};
  for (const r of results) {
    counts[r.status] = (counts[r.status] || 0) + 1;
    log(
      `${r.status.padEnd(12)} ${r.name}${
        r.tests != null ? ` tests=${r.tests}` : ""
      }${r.reason ? " :: " + String(r.reason).slice(0, 100) : ""}`
    );
  }
  log("\nCounts: " + JSON.stringify(counts));

  fs.writeFileSync(path.join(RUNS_DIR, "summary.log"), lines.join("\n") + "\n");
  fs.writeFileSync(
    path.join(RUNS_DIR, "results.json"),
    JSON.stringify(results, null, 2)
  );

  const fails = results.filter((r) => r.status !== "proof_ok");
  process.exit(fails.length ? 1 : 0);
}

main();
