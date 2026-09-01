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
import signal
import subprocess
import sys
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib  # type: ignore[no-redef]

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
CRASH_ARTIFACT_DIR = REPO_ROOT / "crash-artifacts"

PROJECT_FILE = "lyra.toml"

# `lyra compile` writes this beside the emitted sources and the script that
# builds them, so the output directory holds more than one executable file and
# the simulation binary is named rather than searched for.
BINARY_NAME = "program"

# What a project file may say that this runner knows how to put on a command
# line. A design naming anything else -- include paths, macro definitions, a
# compilation policy -- is refused, because honouring the rest of it and
# dropping those would compile something the file did not describe.
KNOWN_KEYS = {
    "package": {"name", "top"},
    "sources": {"files"},
}

# Whole designs, not benchmark cases. A benchmark case is given whatever amount
# of work a measurement needs, so borrowing one here would make this run's
# duration a function of what the benchmark was tuned to, and one sized to
# expose a slow path would make it fail.
DESIGNS = [
    "examples/hello",
    "examples/riscv-cpu",
]

DEFAULT_TIMEOUT_SECS = 60
DEFAULT_MEMORY_MB = 2048
TAIL_LINES = 10


@dataclass(frozen=True)
class Project:
    """A design's top module and the sources that make it up."""
    top: str
    files: list[Path]


def reject_unknown(path: Path, data: dict) -> None:
    """Raise ValueError naming anything in the file this cannot carry out."""
    for section, contents in data.items():
        if section not in KNOWN_KEYS:
            raise ValueError(
                f"{path}: [{section}] is not something this runner can put on "
                f"a command line")
        if not isinstance(contents, dict):
            raise ValueError(f"{path}: [{section}] must be a table")
        unknown = set(contents) - KNOWN_KEYS[section]
        if unknown:
            named = ", ".join(f"{section}.{k}" for k in sorted(unknown))
            raise ValueError(
                f"{path}: {named} is not something this runner can put on a "
                f"command line")


def read_project(directory: Path) -> Project:
    """Read a project directory, or raise ValueError saying what stopped it.

    Reading it here stands in for the compiler, which declines to look the file
    up itself. A stand-in that reads part of a file is worse than one that reads
    none of it, because what it drops it drops silently, so this refuses
    anything it cannot carry out rather than proceeding without it.
    """
    path = directory / PROJECT_FILE
    if not path.exists():
        raise ValueError(f"no {PROJECT_FILE} in {directory}")

    with open(path, "rb") as f:
        data = tomllib.load(f)

    reject_unknown(path, data)

    top = data.get("package", {}).get("top")
    files = data.get("sources", {}).get("files")

    if not isinstance(top, str) or not top:
        raise ValueError(f"{path}: missing package.top")
    if not isinstance(files, list) or not files:
        raise ValueError(f"{path}: missing sources.files")

    return Project(top=top, files=[directory / str(f) for f in files])


def lyra_argv(
    lyra: str | Path, command: str, project: Project,
    options: Sequence[str] = (),
) -> list[str]:
    """Compose a Lyra command line that builds or runs one project.

    The command word is positional and comes first; everything after it is one
    command line shared with the front end, and the sources come last. Lyra is
    told the top and the sources explicitly because looking them up from the
    project file is not wired.
    """
    return [
        str(lyra), command,
        *options,
        "--top", project.top,
        *(str(f) for f in project.files),
    ]


def signal_death(returncode: int) -> int:
    """The signal a subprocess died from, or zero if it exited on its own."""
    return -returncode if returncode < 0 else 0


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


def diagnose_aot_failure(
    lyra: Path,
    project: Project,
    design_rel: str,
    timeout_secs: int,
) -> None:
    """Rebuild the design and run its binary directly, to see the raw signal.

    Running through the driver reports the child's failure in the driver's own
    terms, so a signal death arrives as an exit code with the signal already
    interpreted. Executing the binary itself is what puts the signal back. The
    emitted project is left in place beside the output, which is everything a
    post-mortem needs to reproduce the build.
    """
    artifact_dir = CRASH_ARTIFACT_DIR / design_rel.replace("/", "_")
    artifact_dir.mkdir(parents=True, exist_ok=True)

    output_dir = artifact_dir / "aot_out"
    output_dir.mkdir(exist_ok=True)

    compile_cmd = lyra_argv(
        lyra, "compile", project, ["-o", str(output_dir)])
    try:
        compile_result = subprocess.run(
            compile_cmd, capture_output=True, text=True, timeout=timeout_secs,
        )
    except (subprocess.TimeoutExpired, OSError):
        return

    if compile_result.returncode != 0:
        (artifact_dir / "compile_stderr.txt").write_text(compile_result.stderr)
        return

    binary = output_dir / BINARY_NAME
    if not binary.is_file():
        return

    try:
        run_result = subprocess.run(
            [str(binary)], capture_output=True, text=True, timeout=timeout_secs,
        )
    except subprocess.TimeoutExpired:
        return

    (artifact_dir / "run_stderr.txt").write_text(run_result.stderr)
    (artifact_dir / "run_stdout.txt").write_text(run_result.stdout)
    (artifact_dir / "exit_code.txt").write_text(str(run_result.returncode))

    died_from = signal_death(run_result.returncode)
    if died_from:
        name = signal.Signals(died_from).name
        print(
            f"    killed by {name} when run directly", file=sys.stderr)

    print(
        f"    artifacts saved to {artifact_dir.relative_to(REPO_ROOT)}",
        file=sys.stderr)


def run_design(
    lyra: Path,
    design_path: Path,
    design_rel: str,
    timeout_secs: int,
    memory_mb: int,
) -> tuple[bool, str]:
    """Run a single design. Returns (success, message)."""
    try:
        project = read_project(design_path)
    except ValueError as e:
        return False, str(e)

    cmd = lyra_argv(lyra, "run", project)

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
        # Only do the expensive AOT diagnostic rerun for crash-like failures
        # (signal death or empty stderr suggesting sudden termination).
        looks_like_crash = (
            result.returncode < 0
            or (result.returncode != 0 and len(result.stderr.strip()) == 0)
        )
        if looks_like_crash:
            diagnose_aot_failure(lyra, project, design_rel, timeout_secs)
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

        ok, msg = run_design(
            lyra, design_path, design_rel, args.timeout, args.memory_mb)
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
