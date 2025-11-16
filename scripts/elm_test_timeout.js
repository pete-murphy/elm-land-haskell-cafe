#!/usr/bin/env bun
// Runs elm-test-rs with a timeout. Requires Bun (Node compatibility APIs available).
const [, , timeoutArg, ...rest] = process.argv;
if (!timeoutArg) {
  console.error("usage: elm_test_timeout.js <timeout-seconds> [elm-test-rs args...]");
  process.exit(2);
}
const timeoutSeconds = Number(timeoutArg);
if (!Number.isFinite(timeoutSeconds)) {
  console.error("First argument must be an integer timeout (seconds)");
  process.exit(2);
}

const { spawn } = await import("node:child_process");

const args = ["elm-test-rs", ...rest];
const child = spawn(args[0], args.slice(1), { stdio: "inherit" });

const timer = setTimeout(() => {
  console.error(`elm-test-rs timed out after ${timeoutSeconds}s`);
  try {
    child.kill("SIGTERM");
    setTimeout(() => child.kill("SIGKILL"), 2000);
  } catch {}
  process.exit(124);
}, timeoutSeconds * 1000);

child.on("exit", (code, signal) => {
  clearTimeout(timer);
  if (signal) {
    // If killed by signal return 128 + signal?
    process.exit(code ?? 1);
  } else {
    process.exit(code ?? 1);
  }
});


