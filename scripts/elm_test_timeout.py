#!/usr/bin/env python3
import os
import signal
import subprocess
import sys
from typing import List

def main() -> int:
    if len(sys.argv) < 2:
        print("usage: elm_test_timeout.py <timeout-seconds> [elm-test-rs args...]", flush=True)
        return 2
    try:
        timeout_seconds = int(sys.argv[1])
    except ValueError:
        print("First argument must be an integer timeout (seconds)", flush=True)
        return 2

    args: List[str] = ["elm-test-rs"] + sys.argv[2:]
    # Start in a new process group so we can kill the whole tree on timeout
    proc = subprocess.Popen(
        args,
        preexec_fn=os.setsid if hasattr(os, "setsid") else None,  # type: ignore
    )
    try:
        return proc.wait(timeout=timeout_seconds)
    except subprocess.TimeoutExpired:
        print(f"elm-test-rs timed out after {timeout_seconds}s", flush=True)
        try:
            if hasattr(os, "getpgid") and proc.pid:
                os.killpg(os.getpgid(proc.pid), signal.SIGTERM)  # graceful
        except Exception:
            pass
        try:
            proc.terminate()
        except Exception:
            pass
        try:
            proc.wait(timeout=2)
        except Exception:
            try:
                if hasattr(os, "getpgid") and proc.pid:
                    os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
            except Exception:
                pass
            try:
                proc.kill()
            except Exception:
                pass
        return 124

if __name__ == "__main__":
    sys.exit(main())


