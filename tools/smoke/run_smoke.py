#!/usr/bin/env python3
"""Smoke test runner for Lyra designs.

Compiles and runs each design in the manifest, verifying clean termination.
This is a correctness check, not a performance benchmark.

Usage:
    python3 tools/smoke/run_smoke.py [--lyra PATH] [--timeout SECS]
                                     [--memory-mb MB]
"""

import argparse
import functools
import resource
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent

# Explicit manifest of designs to smoke-test. Each entry is a path relative
# to REPO_ROOT containing a lyra.toml.
DESIGNS = [
    "examples/hello",
    "examples/riscv-cpu",
    "tools/bench/fixtures/stress-array",
    "tools/bench/fixtures/pipeline",
]

DEFAULT_TIMEOUT_SECS = 60
DEFAULT_MEMORY_MB = 2048
TAIL_LINES = 10


def find_lyra_binary(explicit_path: str | None) -> Path:
    if explicit_path:
        p = Path(explicit_path)
        if not p.exists():
            print(f"ERROR: lyra binary not found at {p}", file=sys.stderr)
            sys.exit(1)
        return p

    candidates = [
        REPO_ROOT / "bazel-bin" / "lyra",
    ]
    for c in candidates:
        if c.exists():
            return c

    print(
        "ERROR: lyra binary not found. Build with: bazel build //:lyra",
        file=sys.stderr,
    )
    sys.exit(1)


def set_memory_limit(memory_mb: int) -> None:
    """Set virtual memory limit for child processes."""
    limit_bytes = memory_mb * 1024 * 1024
    resource.setrlimit(resource.RLIMIT_AS, (limit_bytes, limit_bytes))


def format_failure(result: subprocess.CompletedProcess[str]) -> str:
    """Format failure output showing stderr and stdout tails."""
    parts = [f"exit code {result.returncode}"]

    stderr_lines = result.stderr.strip().split("\n") if result.stderr.strip() else []
    stdout_lines = result.stdout.strip().split("\n") if result.stdout.strip() else []

    if stderr_lines:
        tail = stderr_lines[-TAIL_LINES:]
        parts.append("stderr:")
        parts.extend(f"  {line}" for line in tail)

    if stdout_lines and len(stderr_lines) < 3:
        tail = stdout_lines[-TAIL_LINES:]
        parts.append("stdout:")
        parts.extend(f"  {line}" for line in tail)

    return "\n".join(parts)


def run_design(
    lyra: Path,
    design_path: Path,
    timeout_secs: int,
    memory_mb: int,
) -> tuple[bool, str]:
    """Run a single design. Returns (success, message)."""
    if not (design_path / "lyra.toml").exists():
        return False, f"lyra.toml not found in {design_path}"

    cmd = [str(lyra), "-C", str(design_path), "run"]

    try:
        result = subprocess.run(
            cmd,
            timeout=timeout_secs,
            capture_output=True,
            text=True,
            preexec_fn=functools.partial(set_memory_limit, memory_mb),
        )
    except subprocess.TimeoutExpired:
        cmd_str = " ".join(cmd)
        return False, f"TIMEOUT after {timeout_secs}s\n  command: {cmd_str}"
    except OSError as e:
        return False, f"OS error: {e}"

    if result.returncode != 0:
        cmd_str = " ".join(cmd)
        return False, f"{format_failure(result)}\n  command: {cmd_str}"

    return True, "ok"


def main() -> None:
    parser = argparse.ArgumentParser(description="Lyra design smoke tests")
    parser.add_argument("--lyra", help="Path to lyra binary")
    parser.add_argument(
        "--timeout",
        type=int,
        default=DEFAULT_TIMEOUT_SECS,
        help=f"Timeout per design in seconds (default: {DEFAULT_TIMEOUT_SECS})",
    )
    parser.add_argument(
        "--memory-mb",
        type=int,
        default=DEFAULT_MEMORY_MB,
        help=f"Memory limit per design in MB (default: {DEFAULT_MEMORY_MB})",
    )
    args = parser.parse_args()

    lyra = find_lyra_binary(args.lyra)
    print(f"Using lyra: {lyra}")
    print(f"Timeout: {args.timeout}s, Memory limit: {args.memory_mb}MB")
    print()

    failures = []
    for design_rel in DESIGNS:
        design_path = REPO_ROOT / design_rel
        name = design_rel
        sys.stdout.write(f"  {name} ... ")
        sys.stdout.flush()

        ok, msg = run_design(lyra, design_path, args.timeout, args.memory_mb)
        if ok:
            print("PASS")
        else:
            print(f"FAIL: {msg}")
            failures.append((name, msg))

    print()
    if failures:
        print(f"FAILED: {len(failures)}/{len(DESIGNS)} designs")
        for name, msg in failures:
            print(f"  {name}: {msg}")
        sys.exit(1)
    else:
        print(f"All {len(DESIGNS)} designs passed.")


if __name__ == "__main__":
    main()
