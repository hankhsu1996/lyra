#!/usr/bin/env python3
"""Lyra performance benchmark runner.

Discovers benchmark fixtures via bench.toml manifests, runs lyra (and
optionally Verilator) against each fixture, collects compile-time and
simulation-time metrics, and prints a family-grouped Markdown report.

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
from dataclasses import dataclass, field
from pathlib import Path

try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib  # type: ignore[no-redef]


TIMEOUT_SECONDS = 120

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
FIXTURES_ROOT = REPO_ROOT / "tools" / "bench" / "fixtures"

TIER_DEFAULTS = {
    "pr": {"default_trials": 1},
    "nightly": {"default_trials": 3},
}

BACKENDS = ["aot", "aot-two-state", "jit", "verilator"]

REQUIRED_BENCHMARK_FIELDS = [
    "name", "family", "focus", "primary", "regression_means",
]


@dataclass(frozen=True)
class Fixture:
    name: str
    family: str
    focus: str
    primary: str
    secondary: tuple[str, ...]
    regression_means: str
    path: Path
    tiers: dict[str, bool]
    default_params: dict[str, str | int | float | bool]
    tier_params: dict[str, dict[str, str | int | float | bool]]
    relevant_counters: tuple[str, ...]


@dataclass
class BenchResult:
    fixture: str = ""
    family: str = ""
    focus: str = ""
    primary: str = ""
    secondary: tuple[str, ...] = ()
    backend: str = ""
    compile_s: float = 0.0
    sim_s: float = 0.0
    wall_s: float = 0.0
    llvm_insts: int = 0
    mir_stmts: int = 0
    binary_kb: int = 0
    rss_max_mb: float = 0.0
    params: dict = field(default_factory=dict)
    phases: dict = field(default_factory=dict)
    counters: dict = field(default_factory=dict)
    error: str = ""


def parse_manifest(manifest_path: Path) -> dict:
    with open(manifest_path, "rb") as f:
        return tomllib.load(f)


def validate_manifest(data: dict, manifest_path: Path) -> None:
    bench = data.get("benchmark")
    if not bench:
        raise ValueError(f"{manifest_path}: missing [benchmark] section")

    for field_name in REQUIRED_BENCHMARK_FIELDS:
        if field_name not in bench:
            raise ValueError(
                f"{manifest_path}: missing benchmark.{field_name}")

    if "secondary" not in bench:
        raise ValueError(f"{manifest_path}: missing benchmark.secondary")

    if "tiers" not in data:
        raise ValueError(f"{manifest_path}: missing [tiers] section")

    if "counters" not in data:
        raise ValueError(f"{manifest_path}: missing [counters] section")
    if "relevant" not in data["counters"]:
        raise ValueError(
            f"{manifest_path}: missing counters.relevant")

    # Validate directory family matches manifest family
    family_dir = manifest_path.parent.parent.name
    if family_dir != bench["family"]:
        raise ValueError(
            f"{manifest_path}: directory family '{family_dir}' "
            f"does not match manifest family '{bench['family']}'")

    # Validate tier param blocks reference known tiers
    params = data.get("params", {})
    known_tiers = set(data["tiers"].keys())
    for key in params:
        if key == "defaults":
            continue
        if key not in known_tiers:
            raise ValueError(
                f"{manifest_path}: params.{key} references "
                f"unknown tier (known: {known_tiers})")


def discover_fixtures(fixtures_root: Path) -> list[Fixture]:
    """Discover, validate, and return fixtures sorted by (family, name)."""
    fixtures: list[Fixture] = []
    seen_names: dict[str, Path] = {}

    for manifest_path in sorted(fixtures_root.rglob("bench.toml")):
        data = parse_manifest(manifest_path)
        validate_manifest(data, manifest_path)

        bench = data["benchmark"]
        name = bench["name"]

        if name in seen_names:
            raise ValueError(
                f"Duplicate fixture name '{name}': "
                f"{manifest_path} and {seen_names[name]}")
        seen_names[name] = manifest_path

        params = data.get("params", {})
        default_params = dict(params.get("defaults", {}))
        tier_params = {}
        for key, val in params.items():
            if key != "defaults":
                tier_params[key] = dict(val)

        fixture = Fixture(
            name=name,
            family=bench["family"],
            focus=bench["focus"],
            primary=bench["primary"],
            secondary=tuple(bench.get("secondary", [])),
            regression_means=bench["regression_means"],
            path=manifest_path.parent,
            tiers=dict(data["tiers"]),
            default_params=default_params,
            tier_params=tier_params,
            relevant_counters=tuple(data["counters"].get("relevant", [])),
        )
        fixtures.append(fixture)

    fixtures.sort(key=lambda f: (f.family, f.name))
    return fixtures


def resolve_fixture_params(
    fixture: Fixture, tier: str,
) -> dict[str, str | int | float | bool]:
    params = dict(fixture.default_params)
    params.update(fixture.tier_params.get(tier, {}))
    return params


def read_stats_json(path: str) -> dict:
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return {}


def compute_compile_time(stats: dict) -> float:
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
    t0 = time.monotonic()
    proc = subprocess.run(
        [binary_path], capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
    )
    elapsed = time.monotonic() - t0
    if proc.returncode != 0:
        err = _first_error(proc.stderr) or f"sim exit code {proc.returncode}"
        return elapsed, err
    return elapsed, ""


def build_define_args(
    params: dict[str, str | int | float | bool],
) -> list[str]:
    """Build -D arguments for Lyra CLI from resolved params."""
    args = []
    for key, val in params.items():
        args.extend(["-D", f"{key}={val}"])
    return args


def build_verilator_define_args(
    params: dict[str, str | int | float | bool],
) -> list[str]:
    """Build -D arguments for Verilator from resolved params."""
    args = []
    for key, val in params.items():
        args.extend([f"-D{key}={val}"])
    return args


def run_lyra_jit(
    lyra: str, fixture: Fixture, params: dict,
    stats_path: str, two_state: bool = False,
) -> BenchResult:
    backend_name = "jit-two-state" if two_state else "jit"
    result = BenchResult(
        fixture=fixture.name, family=fixture.family,
        focus=fixture.focus, primary=fixture.primary,
        secondary=fixture.secondary, backend=backend_name,
        params=dict(params),
    )
    cmd = [
        lyra, "-C", str(fixture.path), "run",
        "--backend=jit",
        "--stats-out", stats_path,
    ]
    cmd.extend(build_define_args(params))
    if two_state:
        cmd.append("--two-state")
    try:
        t0 = time.monotonic()
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
        )
        result.wall_s = time.monotonic() - t0
        result.rss_max_mb = round(get_rss_children_kb() / 1024, 1)

        if proc.returncode != 0:
            result.error = (
                _first_error(proc.stderr)
                or f"exit code {proc.returncode}")
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
    lyra: str, fixture: Fixture, params: dict,
    stats_path: str, output_dir: str, two_state: bool = False,
) -> BenchResult:
    backend_name = "aot-two-state" if two_state else "aot"
    result = BenchResult(
        fixture=fixture.name, family=fixture.family,
        focus=fixture.focus, primary=fixture.primary,
        secondary=fixture.secondary, backend=backend_name,
        params=dict(params),
    )

    cmd = [
        lyra, "-C", str(fixture.path), "compile",
        "--stats-out", stats_path,
        "-o", output_dir,
    ]
    cmd.extend(build_define_args(params))
    if two_state:
        cmd.append("--two-state")
    try:
        t0 = time.monotonic()
        proc = subprocess.run(
            cmd, capture_output=True, text=True, timeout=TIMEOUT_SECONDS,
        )
        compile_wall = time.monotonic() - t0
        result.rss_max_mb = round(get_rss_children_kb() / 1024, 1)

        if proc.returncode != 0:
            result.error = (
                _first_error(proc.stderr)
                or f"exit code {proc.returncode}")
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
    fixture: Fixture, params: dict, work_dir: str,
) -> BenchResult:
    result = BenchResult(
        fixture=fixture.name, family=fixture.family,
        focus=fixture.focus, primary=fixture.primary,
        secondary=fixture.secondary, backend="verilator",
        params=dict(params),
    )

    verilator = shutil.which("verilator")
    if not verilator:
        result.error = "verilator not found"
        return result

    top, files = parse_lyra_toml(str(fixture.path))
    if not top or not files:
        result.error = "cannot parse lyra.toml"
        return result

    sv_paths = [str(fixture.path / f) for f in files]

    build_cmd = [
        verilator, "--binary",
        "--top-module", top,
        "-Wno-WIDTHEXPAND", "-Wno-WIDTHTRUNC",
        "-Wno-UNUSEDSIGNAL", "-Wno-UNDRIVEN",
        "-Wno-UNUSEDPARAM", "-Wno-PINMISSING",
        "-Wno-CASEINCOMPLETE", "-Wno-IMPORTSTAR",
    ]
    build_cmd.extend(build_verilator_define_args(params))
    build_cmd.extend(sv_paths)

    try:
        env = os.environ.copy()
        env["CCACHE_DISABLE"] = "1"

        t0 = time.monotonic()
        proc = subprocess.run(
            build_cmd, capture_output=True, text=True,
            timeout=TIMEOUT_SECONDS, cwd=work_dir, env=env,
        )
        result.compile_s = time.monotonic() - t0

        if proc.returncode != 0:
            result.error = "verilator build failed"
            return result

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
    lyra: str, fixture: Fixture, params: dict,
    backend: str, tmpdir: str, trial_idx: int,
) -> BenchResult:
    tag = f"{fixture.name}-{backend}-{trial_idx}"
    stats_path = os.path.join(tmpdir, f"{tag}.json")

    two_state = backend.endswith("-two-state")
    base_backend = backend.removesuffix("-two-state")

    if base_backend == "jit":
        return run_lyra_jit(
            lyra, fixture, params, stats_path, two_state)
    elif base_backend == "aot":
        aot_out = os.path.join(tmpdir, tag)
        os.makedirs(aot_out, exist_ok=True)
        return run_lyra_aot(
            lyra, fixture, params, stats_path, aot_out, two_state)
    elif backend == "verilator":
        ver_dir = os.path.join(tmpdir, tag)
        os.makedirs(ver_dir, exist_ok=True)
        return run_verilator(fixture, params, ver_dir)
    else:
        return BenchResult(
            fixture=fixture.name, family=fixture.family,
            backend=backend, error=f"unknown backend: {backend}")


def pick_median_trial(trials: list[BenchResult]) -> BenchResult:
    valid = [t for t in trials if not t.error]
    if not valid:
        return trials[0]
    valid.sort(key=lambda t: t.wall_s)
    return valid[len(valid) // 2]


def run_fixture_backend(
    lyra: str, fixture: Fixture, params: dict,
    backend: str, num_trials: int, tmpdir: str,
) -> BenchResult:
    trials = []
    for trial_idx in range(num_trials):
        r = run_one_trial(
            lyra, fixture, params, backend, tmpdir, trial_idx)
        trials.append(r)
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
    return f"{int(round(val * 1000)):,}"


def fmt_int(val: int) -> str:
    if val == 0:
        return "-"
    return f"{val:,}"


def fmt_float(val: float) -> str:
    if val == 0.0:
        return "-"
    return f"{val:.1f}"


def group_by_family(
    results: list[BenchResult],
) -> dict[str, list[BenchResult]]:
    """Group results into {family: [results]}."""
    grouped: dict[str, list[BenchResult]] = {}
    for r in results:
        grouped.setdefault(r.family, []).append(r)
    return grouped


def group_fixture_backends(
    results: list[BenchResult],
) -> dict[str, dict[str, BenchResult]]:
    """Group results into {fixture: {backend: result}}."""
    grouped: dict[str, dict[str, BenchResult]] = {}
    for r in results:
        grouped.setdefault(r.fixture, {})[r.backend] = r
    return grouped


def get_ordered_fixtures(
    results: list[BenchResult],
) -> list[str]:
    """Get fixture names in stable order (by family, then name)."""
    seen = set()
    ordered = []
    for r in sorted(results, key=lambda r: (r.family, r.fixture)):
        if r.fixture not in seen:
            seen.add(r.fixture)
            ordered.append(r.fixture)
    return ordered


def print_markdown(
    results: list[BenchResult], num_trials: int, tier: str,
) -> None:
    trial_note = f"{num_trials} (median)" if num_trials > 1 else "1"
    by_fixture = group_fixture_backends(results)
    ordered = get_ordered_fixtures(results)

    # Build family -> [fixture_name] map preserving order
    family_fixtures: dict[str, list[str]] = {}
    for r in sorted(results, key=lambda r: (r.family, r.fixture)):
        if r.family not in family_fixtures:
            family_fixtures[r.family] = []
        if r.fixture not in family_fixtures[r.family]:
            family_fixtures[r.family].append(r.fixture)

    print()
    print("## Lyra Benchmark Report")
    print()
    print(f"> git: `{get_git_sha()}` | tier: {tier} | trials: {trial_note}")

    # Table 1: Simulation Performance
    print()
    print("### Simulation Performance")
    print()
    print(
        "| Fixture | Lyra 4-state (ms) "
        "| Lyra 2-state (ms) | Verilator (ms) |")
    print(
        "|---------|------------------:"
        "|------------------:|---------------:|")

    for fixture_name in ordered:
        backends = by_fixture.get(fixture_name, {})
        aot = backends.get("aot")
        aot_2s = backends.get("aot-two-state")
        ver = backends.get("verilator")

        aot_sim = (
            fmt_time(aot.sim_s) if aot and not aot.error
            else "FAIL" if aot else "-")
        aot_2s_sim = (
            fmt_time(aot_2s.sim_s) if aot_2s and not aot_2s.error
            else "FAIL" if aot_2s else "-")
        ver_sim = (
            fmt_time(ver.sim_s) if ver and not ver.error
            else "FAIL" if ver else "-")

        print(
            f"| {fixture_name} "
            f"| {aot_sim} | {aot_2s_sim} | {ver_sim} |")

    # Table 2: Compile Time
    print()
    print("### Compile Time")
    print()
    print(
        "| Fixture | AOT (ms) "
        "| JIT (ms) | Verilator (ms) |")
    print(
        "|--------|---------:"
        "|---------:|---------------:|")

    for fixture_name in ordered:
        backends = by_fixture.get(fixture_name, {})
        aot = backends.get("aot")
        jit = backends.get("jit")
        ver = backends.get("verilator")

        aot_c = (
            fmt_time(aot.compile_s) if aot and not aot.error
            else "FAIL" if aot else "-")
        jit_c = (
            fmt_time(jit.compile_s) if jit and not jit.error
            else "FAIL" if jit else "-")
        ver_c = (
            fmt_time(ver.compile_s) if ver and not ver.error
            else "FAIL" if ver else "-")

        print(
            f"| {fixture_name} "
            f"| {aot_c} | {jit_c} | {ver_c} |")

    # Errors
    errors = [r for r in results if r.error and r.error != "verilator not found"]
    if errors:
        print()
        print("### Errors")
        print()
        for r in errors:
            print(f"- **{r.fixture}/{r.backend}**: {r.error}")

    print()


def result_to_dict(r: BenchResult) -> dict:
    return {
        "fixture": r.fixture,
        "family": r.family,
        "focus": r.focus,
        "primary": r.primary,
        "secondary": list(r.secondary),
        "backend": r.backend,
        "compile_s": r.compile_s,
        "sim_s": r.sim_s,
        "wall_s": r.wall_s,
        "llvm_insts": r.llvm_insts,
        "mir_stmts": r.mir_stmts,
        "binary_kb": r.binary_kb,
        "rss_max_mb": r.rss_max_mb,
        "params": r.params,
        "phases": r.phases,
        "counters": r.counters,
        "error": r.error,
    }


def write_json(
    results: list[BenchResult], path: str, tier: str,
) -> None:
    data = {
        "version": 2,
        "git": get_git_sha(),
        "tier": tier,
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "results": [result_to_dict(r) for r in results],
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

    lyra = str(REPO_ROOT / "bazel-bin" / "lyra")

    if not os.path.isfile(lyra):
        print(f"Error: lyra binary not found at {lyra}", file=sys.stderr)
        sys.exit(1)

    # Discover fixtures
    all_fixtures = discover_fixtures(FIXTURES_ROOT)
    if not all_fixtures:
        print("Error: no fixtures found", file=sys.stderr)
        sys.exit(1)

    # Filter by tier
    tier_name = args.tier
    tier_fixtures = [f for f in all_fixtures if f.tiers.get(tier_name, False)]
    if not tier_fixtures:
        print(
            f"Error: no fixtures enabled for tier '{tier_name}'",
            file=sys.stderr)
        sys.exit(1)

    tier_config = TIER_DEFAULTS.get(tier_name, {"default_trials": 1})
    num_trials = (
        args.trials if args.trials is not None
        else tier_config["default_trials"])

    all_results: list[BenchResult] = []
    has_failure = False

    with tempfile.TemporaryDirectory(prefix="lyra-bench-") as tmpdir:
        for fixture in tier_fixtures:
            params = resolve_fixture_params(fixture, tier_name)

            for backend in BACKENDS:
                r = run_fixture_backend(
                    lyra, fixture, params,
                    backend, num_trials, tmpdir)
                all_results.append(r)
                if r.error and r.error != "verilator not found":
                    has_failure = True

    print_markdown(all_results, num_trials, tier_name)

    if args.json:
        write_json(all_results, args.json, tier_name)
        print(f"JSON written to {args.json}", file=sys.stderr)

    if has_failure and not args.ci:
        sys.exit(1)


if __name__ == "__main__":
    main()
