#!/usr/bin/env python3
"""Lyra performance benchmark runner.

Runs lyra (and optionally Verilator) against a set of designs, collects
compile-time and simulation-time metrics, and prints a Markdown report.

Usage:
    python3 tools/bench/run_benchmarks.py [--json PATH] [--trials N]
           [--tier pr|nightly] [--ci]
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
    "stress-array": REPO_ROOT / "tools" / "bench" / "fixtures" / "stress-array",
    "pipeline": REPO_ROOT / "tools" / "bench" / "fixtures" / "pipeline",
}

TIER_CONFIG = {
    "pr": {
        "designs": ["stress-array", "pipeline"],
        "backends": ["aot", "jit", "verilator"],
        "default_trials": 1,
    },
    "nightly": {
        "designs": ["stress-array", "pipeline"],
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


def compute_compile_time(stats: dict) -> float:
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
    result.compile_s = compute_compile_time(stats)

    llvm = stats.get("llvm", {})
    result.llvm_insts = llvm.get("instructions", 0)

    mir = stats.get("mir", {})
    result.mir_stmts = mir.get("mir_stmts", 0)


def get_rss_children_kb() -> int:
    ru = resource.getrusage(resource.RUSAGE_CHILDREN)
    return int(ru.ru_maxrss)


def find_binary(output_dir: str) -> str | None:
    """Find the compiled binary in output_dir/."""
    out = Path(output_dir)
    if not out.exists():
        return None
    regular_files = [
        f for f in out.iterdir()
        if f.is_file() and not f.is_symlink() and os.access(f, os.X_OK)
    ]
    if len(regular_files) != 1:
        return None
    return str(regular_files[0])


def time_binary(binary_path: str) -> tuple[float, str]:
    """Run a binary and return (wall_seconds, error_string)."""
    t0 = time.monotonic()
    proc = subprocess.run(
        [binary_path], capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
    )
    elapsed = time.monotonic() - t0
    if proc.returncode != 0:
        err = _first_error(proc.stderr) or f"sim exit code {proc.returncode}"
        return elapsed, err
    return elapsed, ""


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
        if not binary:
            result.error = f"binary not found in {output_dir}"
            return result

        result.binary_kb = round(Path(binary).stat().st_size / 1024)

        # Step 2: run the binary
        sim_time, sim_err = time_binary(binary)
        result.sim_s = sim_time
        if sim_err:
            result.error = sim_err

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
        sim_time, sim_err = time_binary(binary)
        result.sim_s = sim_time
        if sim_err:
            result.error = sim_err
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


def group_by_design(
    results: list[BenchResult], designs: list[str],
) -> dict[str, dict[str, BenchResult]]:
    """Group results into {design: {backend: result}}."""
    grouped: dict[str, dict[str, BenchResult]] = {}
    for r in results:
        grouped.setdefault(r.design, {})[r.backend] = r
    return grouped


def print_markdown(
    results: list[BenchResult], num_trials: int, designs: list[str],
) -> None:
    trial_note = f"{num_trials} (median)" if num_trials > 1 else "1"
    grouped = group_by_design(results, designs)

    print()
    print("## Lyra Benchmark Report")
    print()
    print(f"> git: `{get_git_sha()}` | trials: {trial_note}")

    # Table 1: Simulation Performance
    print()
    print("### Simulation Performance")
    print()
    print("| Design | Lyra AOT (s) | Verilator (s) |")
    print("|--------|--------------|---------------|")
    for design in designs:
        backends = grouped.get(design, {})
        aot = backends.get("aot")
        ver = backends.get("verilator")

        aot_sim = fmt_time(aot.sim_s) if aot and not aot.error else "FAIL" if aot else "-"
        ver_sim = fmt_time(ver.sim_s) if ver and not ver.error else "FAIL" if ver else "-"

        print(f"| {design} | {aot_sim} | {ver_sim} |")

    # Table 2: Compile Time
    print()
    print("### Compile Time")
    print()
    print("| Design | AOT (s) | JIT (s) | Verilator (s) |")
    print("|--------|---------|---------|---------------|")
    for design in designs:
        backends = grouped.get(design, {})
        aot = backends.get("aot")
        jit = backends.get("jit")
        ver = backends.get("verilator")

        aot_c = fmt_time(aot.compile_s) if aot and not aot.error else "FAIL" if aot else "-"
        jit_c = fmt_time(jit.compile_s) if jit and not jit.error else "FAIL" if jit else "-"
        ver_c = fmt_time(ver.compile_s) if ver and not ver.error else "FAIL" if ver else "-"

        print(f"| {design} | {aot_c} | {jit_c} | {ver_c} |")

    # Errors
    errors = [r for r in results if r.error and r.error != "verilator not found"]
    if errors:
        print()
        print("### Errors")
        print()
        for r in errors:
            print(f"- **{r.design}/{r.backend}**: {r.error}")

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
        "--json", default=None, help="Write JSON results to this path")
    parser.add_argument(
        "--trials", type=int, default=None, help="Override number of trials")
    parser.add_argument(
        "--tier", choices=["pr", "nightly"], default="nightly",
        help="Benchmark tier (default: nightly)")
    parser.add_argument(
        "--ci", action="store_true",
        help="CI mode: always exit 0, print FAIL rows")
    args = parser.parse_args()

    # Expect a pre-built optimized binary. The caller (CI workflow or user)
    # is responsible for building with -c opt before running benchmarks.
    # See docs/profiling.md for why -c opt is required.
    lyra = str(REPO_ROOT / "bazel-bin" / "lyra")

    if not os.path.isfile(lyra):
        print(f"Error: lyra binary not found at {lyra}", file=sys.stderr)
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

    print_markdown(all_results, num_trials, design_names)

    if args.json:
        write_json(all_results, args.json)
        print(f"JSON written to {args.json}", file=sys.stderr)

    if has_failure and not args.ci:
        sys.exit(1)


if __name__ == "__main__":
    main()
