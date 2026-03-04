#!/usr/bin/env python3
"""Lyra performance benchmark runner.

Runs lyra against a set of designs, collects compile-time metrics from
structured JSON output (--stats-out), and prints a Markdown report table.

RSS measurement: Reports RUSAGE_CHILDREN ru_maxrss (monotonic high-water mark
across all child processes in the session).

Usage:
    python3 tools/bench/run_benchmarks.py [--lyra PATH] [--json PATH]
           [--trials N] [--tier pr|nightly|full] [--ci]
"""

import argparse
import json
import os
import resource
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, asdict
from pathlib import Path


TIMEOUT_SECONDS = 120

REPO_ROOT = Path(__file__).resolve().parent.parent.parent

DESIGNS = {
    "hello": REPO_ROOT / "examples" / "hello",
    "riscv-cpu": REPO_ROOT / "examples" / "riscv-cpu",
    "stress-array": REPO_ROOT / "tools" / "bench" / "fixtures" / "stress-array",
}

TIER_CONFIG = {
    "pr": {
        "designs": ["hello", "riscv-cpu"],
        "backends": ["aot"],
        "default_trials": 1,
    },
    "nightly": {
        "designs": ["hello", "riscv-cpu", "stress-array"],
        "backends": ["aot", "jit"],
        "default_trials": 3,
    },
    "full": {
        "designs": ["hello", "riscv-cpu", "stress-array"],
        "backends": ["aot", "jit"],
        "default_trials": 3,
    },
}


@dataclass
class BenchResult:
    design: str = ""
    backend: str = ""
    wall_s: float = 0.0
    parse_s: float = 0.0
    elaborate_s: float = 0.0
    lower_hir_s: float = 0.0
    lower_mir_s: float = 0.0
    lower_llvm_s: float = 0.0
    llvm_insts: int = 0
    mir_stmts: int = 0
    rss_max_mb: float = 0.0
    binary_kb: int = 0
    error: str = ""


def read_stats_json(path: str) -> dict:
    """Read the --stats-out JSON file."""
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return {}


def populate_from_stats(result: BenchResult, stats: dict) -> None:
    """Populate a BenchResult from --stats-out JSON."""
    phases = stats.get("phases", {})
    result.parse_s = phases.get("parse", 0.0)
    result.elaborate_s = phases.get("elaborate", 0.0)
    result.lower_hir_s = phases.get("lower_hir", 0.0)
    result.lower_mir_s = phases.get("lower_mir", 0.0)
    result.lower_llvm_s = phases.get("lower_llvm", 0.0)

    llvm = stats.get("llvm", {})
    result.llvm_insts = llvm.get("instructions", 0)

    mir = stats.get("mir", {})
    result.mir_stmts = mir.get("mir_stmts", 0)


def get_rss_children_kb() -> int:
    """Get current RUSAGE_CHILDREN max RSS in KB."""
    ru = resource.getrusage(resource.RUSAGE_CHILDREN)
    return int(ru.ru_maxrss)


def run_one(
    lyra: str, project_dir: str, design: str, backend: str,
    stats_json_path: str, output_dir: str | None = None,
) -> BenchResult:
    """Run a single benchmark trial."""
    result = BenchResult(design=design, backend=backend)

    if backend == "aot":
        cmd = [
            lyra, "-C", project_dir, "compile",
            "--stats-out", stats_json_path,
            "-o", output_dir,
        ]
    else:
        cmd = [
            lyra, "-C", project_dir, "run",
            "--backend=jit",
            "--stats-out", stats_json_path,
        ]

    try:
        t0 = time.monotonic()
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
        )
        result.wall_s = time.monotonic() - t0
        result.rss_max_mb = round(get_rss_children_kb() / 1024, 1)

        stats = read_stats_json(stats_json_path)

        if proc.returncode != 0:
            first_error = ""
            for line in (proc.stderr or "").splitlines():
                if "error" in line.lower():
                    first_error = line.strip()
                    break
            result.error = first_error or f"exit code {proc.returncode}"
        elif not stats:
            result.error = "missing/invalid stats-out JSON"
        else:
            populate_from_stats(result, stats)

    except subprocess.TimeoutExpired:
        result.error = "TIMEOUT"
    except Exception as e:
        result.error = str(e)

    return result


def find_binary_size(output_dir: str) -> int:
    """Find the compiled binary in output_dir/bin/ and return size in KB."""
    bin_dir = Path(output_dir) / "bin"
    if not bin_dir.exists():
        return 0
    regular_files = [
        f for f in bin_dir.iterdir()
        if f.is_file() and not f.is_symlink()
    ]
    if len(regular_files) != 1:
        return 0
    return round(regular_files[0].stat().st_size / 1024)


def pick_median_trial(trials: list[BenchResult]) -> BenchResult:
    """Pick the median trial by wall time (coherent: all data from one trial)."""
    valid = [t for t in trials if not t.error]
    if not valid:
        return trials[0]
    valid.sort(key=lambda t: t.wall_s)
    return valid[len(valid) // 2]


def run_design_backend(
    lyra: str, design: str, project_dir: str, backend: str,
    num_trials: int, tmpdir: str,
) -> BenchResult:
    """Run all trials for a single design+backend, return median."""
    trials = []
    for trial_idx in range(num_trials):
        stats_path = os.path.join(tmpdir, f"{design}-{backend}-{trial_idx}.json")
        aot_out = None
        if backend == "aot":
            aot_out = os.path.join(tmpdir, f"{design}-aot-{trial_idx}")
            os.makedirs(aot_out, exist_ok=True)

        r = run_one(lyra, str(project_dir), design, backend, stats_path, aot_out)

        if backend == "aot" and aot_out:
            r.binary_kb = find_binary_size(aot_out)

        trials.append(r)

    return pick_median_trial(trials)


def get_git_sha() -> str:
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True,
        ).stdout.strip()
    except Exception:
        return "unknown"


def fmt_val(val, suffix="") -> str:
    if isinstance(val, float):
        if val == 0.0:
            return "-"
        return f"{val}{suffix}"
    if isinstance(val, int):
        if val == 0:
            return "-"
        return f"{val}{suffix}"
    return str(val)


def print_markdown(results: list[BenchResult], num_trials: int) -> None:
    trial_note = f"{num_trials} (median)" if num_trials > 1 else "1"
    print("## Lyra Benchmark Report")
    print()
    print(f"git: {get_git_sha()} | trials: {trial_note}")
    print()
    print(
        "| Design | Backend | Wall (s) | Parse | Elab | HIR->MIR | MIR->LLVM "
        "| LLVM Insts | MIR Stmts | RSS max (MB) | Binary (KB) |"
    )
    print(
        "|--------|---------|----------|-------|------|----------|----------"
        "--|------------|-----------|--------------|-------------|"
    )

    for r in results:
        if r.error:
            print(f"| {r.design} | {r.backend} | FAIL | | | | | | | | |")
            print(f"| | | \\ {r.error} | | | | | | | | |")
            continue

        binary = fmt_val(r.binary_kb) if r.backend == "aot" else "-"
        print(
            f"| {r.design} | {r.backend} "
            f"| {r.wall_s:.2f} "
            f"| {fmt_val(r.parse_s)} "
            f"| {fmt_val(r.elaborate_s)} "
            f"| {fmt_val(r.lower_mir_s)} "
            f"| {fmt_val(r.lower_llvm_s)} "
            f"| {fmt_val(r.llvm_insts)} "
            f"| {fmt_val(r.mir_stmts)} "
            f"| {fmt_val(r.rss_max_mb)} "
            f"| {binary} |"
        )


def write_json(results: list[BenchResult], path: str) -> None:
    data = {
        "git": get_git_sha(),
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "results": [asdict(r) for r in results],
    }
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def main() -> None:
    parser = argparse.ArgumentParser(description="Lyra performance benchmarks")
    parser.add_argument(
        "--lyra", default="bazel-bin/lyra", help="Path to lyra binary")
    parser.add_argument(
        "--json", default=None, help="Write JSON results to this path")
    parser.add_argument(
        "--trials", type=int, default=None, help="Override number of trials")
    parser.add_argument(
        "--tier", choices=["pr", "nightly", "full"], default="full",
        help="Benchmark tier (default: full)")
    parser.add_argument(
        "--ci", action="store_true",
        help="CI mode: always exit 0, print FAIL rows")
    args = parser.parse_args()

    lyra = os.path.abspath(args.lyra)

    if not os.path.isfile(lyra):
        print(f"Error: lyra binary not found at {lyra}", file=sys.stderr)
        print("Run 'bazel build //:lyra' first.", file=sys.stderr)
        sys.exit(1)

    tier = TIER_CONFIG[args.tier]
    design_names = tier["designs"]
    backends = tier["backends"]
    num_trials = args.trials if args.trials is not None else tier["default_trials"]

    all_results: list[BenchResult] = []
    has_failure = False

    with tempfile.TemporaryDirectory(prefix="lyra-bench-") as tmpdir:
        for design_name in design_names:
            project_dir = DESIGNS[design_name]
            if not project_dir.exists():
                r = BenchResult(
                    design=design_name, backend="?",
                    error=f"project dir not found: {project_dir}")
                all_results.append(r)
                has_failure = True
                continue

            for backend in backends:
                r = run_design_backend(
                    lyra, design_name, str(project_dir), backend,
                    num_trials, tmpdir)
                all_results.append(r)
                if r.error:
                    has_failure = True

    print_markdown(all_results, num_trials)

    if args.json:
        write_json(all_results, args.json)
        print(f"\nJSON written to {args.json}", file=sys.stderr)

    if has_failure and not args.ci:
        sys.exit(1)


if __name__ == "__main__":
    main()
