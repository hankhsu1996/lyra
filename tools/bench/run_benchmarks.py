#!/usr/bin/env python3
"""Lyra performance benchmark runner.

Runs lyra (and optionally Verilator) against a set of designs, collects
compile-time and simulation-time metrics, and prints a Markdown report.

Usage:
    python3 tools/bench/run_benchmarks.py [--lyra PATH] [--json PATH]
           [--trials N] [--tier pr|nightly|full] [--ci]
"""

import argparse
import json
import os
import re
import resource
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass, asdict, field
from pathlib import Path


TIMEOUT_SECONDS = 120

REPO_ROOT = Path(__file__).resolve().parent.parent.parent

DESIGNS = {
    "hello": REPO_ROOT / "examples" / "hello",
    "riscv-cpu": REPO_ROOT / "examples" / "riscv-cpu",
    "stress-array": REPO_ROOT / "tools" / "bench" / "fixtures" / "stress-array",
    "pipeline": REPO_ROOT / "tools" / "bench" / "fixtures" / "pipeline",
}

TIER_CONFIG = {
    "pr": {
        "designs": ["hello", "riscv-cpu"],
        "backends": ["aot"],
        "default_trials": 1,
    },
    "nightly": {
        "designs": ["hello", "riscv-cpu", "stress-array", "pipeline"],
        "backends": ["aot", "jit", "verilator"],
        "default_trials": 3,
    },
    "full": {
        "designs": ["hello", "riscv-cpu", "stress-array", "pipeline"],
        "backends": ["aot", "jit", "verilator"],
        "default_trials": 3,
    },
}


@dataclass
class BenchResult:
    design: str = ""
    backend: str = ""
    compile_s: float = 0.0
    sim_s: float = 0.0
    wall_s: float = 0.0
    llvm_insts: int = 0
    mir_stmts: int = 0
    binary_kb: int = 0
    rss_max_mb: float = 0.0
    phases: dict = field(default_factory=dict)
    error: str = ""


def read_stats_json(path: str) -> dict:
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return {}


def compute_compile_time(stats: dict, backend: str) -> float:
    """Sum all compile phases from stats JSON."""
    phases = stats.get("phases", {})
    total = 0.0
    for key, val in phases.items():
        if key == "sim":
            continue
        total += val
    return total


def populate_from_stats(result: BenchResult, stats: dict) -> None:
    phases = stats.get("phases", {})
    result.phases = dict(phases)
    result.compile_s = compute_compile_time(stats, result.backend)

    llvm = stats.get("llvm", {})
    result.llvm_insts = llvm.get("instructions", 0)

    mir = stats.get("mir", {})
    result.mir_stmts = mir.get("mir_stmts", 0)


def get_rss_children_kb() -> int:
    ru = resource.getrusage(resource.RUSAGE_CHILDREN)
    return int(ru.ru_maxrss)


def find_binary(output_dir: str) -> str | None:
    """Find the compiled binary in output_dir/bin/."""
    bin_dir = Path(output_dir) / "bin"
    if not bin_dir.exists():
        return None
    regular_files = [
        f for f in bin_dir.iterdir()
        if f.is_file() and not f.is_symlink()
    ]
    if len(regular_files) != 1:
        return None
    return str(regular_files[0])


def time_binary(binary_path: str) -> float:
    """Run a binary and return wall-clock time in seconds."""
    t0 = time.monotonic()
    subprocess.run(
        [binary_path], capture_output=True, timeout=TIMEOUT_SECONDS,
    )
    return time.monotonic() - t0


def run_lyra_jit(
    lyra: str, project_dir: str, design: str, stats_path: str,
) -> BenchResult:
    result = BenchResult(design=design, backend="jit")
    cmd = [
        lyra, "-C", project_dir, "run",
        "--backend=jit",
        "--stats-out", stats_path,
    ]
    try:
        t0 = time.monotonic()
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
        )
        result.wall_s = time.monotonic() - t0
        result.rss_max_mb = round(get_rss_children_kb() / 1024, 1)

        if proc.returncode != 0:
            result.error = _first_error(proc.stderr) or f"exit code {proc.returncode}"
            return result

        stats = read_stats_json(stats_path)
        if not stats:
            result.error = "missing/invalid stats-out JSON"
            return result

        populate_from_stats(result, stats)
        result.sim_s = max(0.0, result.wall_s - result.compile_s)

    except subprocess.TimeoutExpired:
        result.error = "TIMEOUT"
    except Exception as e:
        result.error = str(e)

    return result


def run_lyra_aot(
    lyra: str, project_dir: str, design: str, stats_path: str,
    output_dir: str,
) -> BenchResult:
    result = BenchResult(design=design, backend="aot")

    # Step 1: compile
    cmd = [
        lyra, "-C", project_dir, "compile",
        "--stats-out", stats_path,
        "-o", output_dir,
    ]
    try:
        t0 = time.monotonic()
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
        )
        compile_wall = time.monotonic() - t0
        result.rss_max_mb = round(get_rss_children_kb() / 1024, 1)

        if proc.returncode != 0:
            result.error = _first_error(proc.stderr) or f"exit code {proc.returncode}"
            return result

        stats = read_stats_json(stats_path)
        if not stats:
            result.error = "missing/invalid stats-out JSON"
            return result

        populate_from_stats(result, stats)
        result.compile_s = compile_wall

        binary = find_binary(output_dir)
        if binary:
            result.binary_kb = round(Path(binary).stat().st_size / 1024)
            # Step 2: run the binary
            result.sim_s = time_binary(binary)

        result.wall_s = result.compile_s + result.sim_s

    except subprocess.TimeoutExpired:
        result.error = "TIMEOUT"
    except Exception as e:
        result.error = str(e)

    return result


def parse_lyra_toml(project_dir: str) -> tuple[str, list[str]]:
    """Parse lyra.toml for top module and source files."""
    toml_path = Path(project_dir) / "lyra.toml"
    if not toml_path.exists():
        return "", []

    content = toml_path.read_text()
    top = ""
    files = []

    top_match = re.search(r'top\s*=\s*"([^"]+)"', content)
    if top_match:
        top = top_match.group(1)

    files_match = re.search(r'files\s*=\s*\[(.*?)\]', content, re.DOTALL)
    if files_match:
        files = re.findall(r'"([^"]+)"', files_match.group(1))

    return top, files


def run_verilator(
    project_dir: str, design: str, work_dir: str,
) -> BenchResult:
    result = BenchResult(design=design, backend="verilator")

    verilator = shutil.which("verilator")
    if not verilator:
        result.error = "verilator not found"
        return result

    top, files = parse_lyra_toml(project_dir)
    if not top or not files:
        result.error = "cannot parse lyra.toml"
        return result

    sv_paths = [str(Path(project_dir) / f) for f in files]

    # Step 1: build
    build_cmd = [
        verilator, "--binary",
        "--top-module", top,
        "-Wno-WIDTHEXPAND", "-Wno-WIDTHTRUNC",
        "-Wno-UNUSEDSIGNAL", "-Wno-UNDRIVEN",
        "-Wno-UNUSEDPARAM", "-Wno-PINMISSING",
        "-Wno-CASEINCOMPLETE", "-Wno-IMPORTSTAR",
    ] + sv_paths

    try:
        t0 = time.monotonic()
        proc = subprocess.run(
            build_cmd, capture_output=True, text=True,
            timeout=TIMEOUT_SECONDS, cwd=work_dir,
        )
        result.compile_s = time.monotonic() - t0

        if proc.returncode != 0:
            result.error = "verilator build failed"
            return result

        # Step 2: run
        binary = os.path.join(work_dir, "obj_dir", f"V{top}")
        if not os.path.isfile(binary):
            result.error = f"binary not found: {binary}"
            return result

        result.binary_kb = round(Path(binary).stat().st_size / 1024)
        result.sim_s = time_binary(binary)
        result.wall_s = result.compile_s + result.sim_s
        result.rss_max_mb = round(get_rss_children_kb() / 1024, 1)

    except subprocess.TimeoutExpired:
        result.error = "TIMEOUT"
    except Exception as e:
        result.error = str(e)

    return result


def _first_error(stderr: str | None) -> str:
    for line in (stderr or "").splitlines():
        if "error" in line.lower():
            return line.strip()
    return ""


def run_one_trial(
    lyra: str, design: str, project_dir: str, backend: str, tmpdir: str,
    trial_idx: int,
) -> BenchResult:
    stats_path = os.path.join(tmpdir, f"{design}-{backend}-{trial_idx}.json")

    if backend == "jit":
        return run_lyra_jit(lyra, project_dir, design, stats_path)
    elif backend == "aot":
        aot_out = os.path.join(tmpdir, f"{design}-aot-{trial_idx}")
        os.makedirs(aot_out, exist_ok=True)
        return run_lyra_aot(lyra, project_dir, design, stats_path, aot_out)
    elif backend == "verilator":
        ver_dir = os.path.join(tmpdir, f"{design}-verilator-{trial_idx}")
        os.makedirs(ver_dir, exist_ok=True)
        return run_verilator(project_dir, design, ver_dir)
    else:
        return BenchResult(design=design, backend=backend, error=f"unknown backend: {backend}")


def pick_median_trial(trials: list[BenchResult]) -> BenchResult:
    valid = [t for t in trials if not t.error]
    if not valid:
        return trials[0]
    valid.sort(key=lambda t: t.wall_s)
    return valid[len(valid) // 2]


def run_design_backend(
    lyra: str, design: str, project_dir: str, backend: str,
    num_trials: int, tmpdir: str,
) -> BenchResult:
    trials = []
    for trial_idx in range(num_trials):
        r = run_one_trial(lyra, design, project_dir, backend, tmpdir, trial_idx)
        trials.append(r)
        # Skip remaining trials if first one errors
        if r.error:
            break
    return pick_median_trial(trials)


def get_git_sha() -> str:
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True,
        ).stdout.strip()
    except Exception:
        return "unknown"


def fmt_time(val: float) -> str:
    if val == 0.0:
        return "-"
    if val < 0.01:
        return f"{val:.4f}"
    return f"{val:.2f}"


def fmt_int(val: int) -> str:
    if val == 0:
        return "-"
    return str(val)


def compute_ratios(
    results: list[BenchResult],
) -> dict[str, float]:
    """Compute Verilator sim_s per design for ratio calculation."""
    verilator_sim: dict[str, float] = {}
    for r in results:
        if r.backend == "verilator" and not r.error and r.sim_s > 0:
            verilator_sim[r.design] = r.sim_s
    return verilator_sim


def print_markdown(results: list[BenchResult], num_trials: int) -> None:
    trial_note = f"{num_trials} (median)" if num_trials > 1 else "1"
    verilator_sim = compute_ratios(results)

    print()
    print("## Lyra Benchmark Report")
    print()
    print(f"git: {get_git_sha()} | trials: {trial_note}")
    print()
    print(
        "| Design | Backend | Compile (s) | Sim (s) | Total (s) "
        "| vs Verilator | LLVM Insts | MIR Stmts | Binary (KB) |"
    )
    print(
        "|--------|---------|-------------|---------|-----------|"
        "--------------|------------|-----------|-------------|"
    )

    for r in results:
        if r.error:
            print(
                f"| {r.design} | {r.backend} "
                f"| FAIL | | | | | | |"
            )
            print(
                f"| | | {r.error} | | | | | | |"
            )
            continue

        # Compute ratio vs Verilator
        ratio = "-"
        if r.sim_s > 0 and r.design in verilator_sim:
            ratio_val = r.sim_s / verilator_sim[r.design]
            if r.backend == "verilator":
                ratio = "1.0x"
            elif ratio_val >= 10:
                ratio = f"{ratio_val:.0f}x"
            else:
                ratio = f"{ratio_val:.1f}x"

        print(
            f"| {r.design} | {r.backend} "
            f"| {fmt_time(r.compile_s)} "
            f"| {fmt_time(r.sim_s)} "
            f"| {fmt_time(r.wall_s)} "
            f"| {ratio} "
            f"| {fmt_int(r.llvm_insts)} "
            f"| {fmt_int(r.mir_stmts)} "
            f"| {fmt_int(r.binary_kb)} |"
        )

    print()


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
                if r.error and r.error != "verilator not found":
                    has_failure = True

    print_markdown(all_results, num_trials)

    if args.json:
        write_json(all_results, args.json)
        print(f"JSON written to {args.json}", file=sys.stderr)

    if has_failure and not args.ci:
        sys.exit(1)


if __name__ == "__main__":
    main()
