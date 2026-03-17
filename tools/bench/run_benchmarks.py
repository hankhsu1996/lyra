#!/usr/bin/env python3
"""Lyra performance benchmark runner.

Discovers benchmark fixtures via bench.toml manifests, runs lyra (and
optionally Verilator) against each fixture, collects compile-time and
simulation-time metrics, and prints a Markdown report sorted by category.

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
CONFIG_PATH = REPO_ROOT / "tools" / "bench" / "config.toml"

BACKENDS = ["aot", "aot-two-state", "jit", "verilator"]


@dataclass(frozen=True)
class TierConfig:
    profile: str
    trials: int


@dataclass(frozen=True)
class Fixture:
    name: str
    category: str
    subcategory: str | None
    intent: str
    focus: str
    primary: str
    secondary: tuple[str, ...]
    path: Path
    scale: dict[str, dict[str, str | int | float | bool]]


@dataclass
class BenchResult:
    fixture: str = ""
    category: str = ""
    subcategory: str | None = None
    intent: str = ""
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


# Manifest schema: maps field name -> (type, required, non_empty).
# This is the single definition of the bench.toml [benchmark] shape.
_BENCHMARK_SCHEMA: dict[str, tuple[type, bool, bool]] = {
    "name":        (str,  True,  True),
    "category":    (str,  True,  True),
    "subcategory": (str,  False, False),
    "intent":      (str,  True,  True),
    "focus":       (str,  True,  True),
    "primary":     (str,  True,  True),
    "secondary":   (list, True,  False),
}

_ALLOWED_SECTIONS = {"benchmark", "scale"}


def load_config() -> dict[str, TierConfig]:
    """Load central tier configuration from config.toml."""
    if not CONFIG_PATH.exists():
        print(f"Error: config not found at {CONFIG_PATH}", file=sys.stderr)
        sys.exit(1)

    with open(CONFIG_PATH, "rb") as f:
        data = tomllib.load(f)

    tiers_data = data.get("tiers")
    if not tiers_data:
        print("Error: config.toml missing [tiers] section", file=sys.stderr)
        sys.exit(1)

    tier_configs: dict[str, TierConfig] = {}
    for tier_name, tier_data in tiers_data.items():
        if "profile" not in tier_data or "trials" not in tier_data:
            print(
                f"Error: config.toml [tiers.{tier_name}] missing "
                f"'profile' or 'trials'", file=sys.stderr)
            sys.exit(1)
        tier_configs[tier_name] = TierConfig(
            profile=tier_data["profile"],
            trials=tier_data["trials"],
        )

    return tier_configs


def parse_fixture_manifest(
    manifest_path: Path, fixtures_root: Path,
) -> Fixture:
    """Parse a bench.toml into a Fixture, or raise ValueError."""
    with open(manifest_path, "rb") as f:
        data = tomllib.load(f)

    ctx = str(manifest_path)

    # Reject unknown top-level sections
    unexpected = set(data.keys()) - _ALLOWED_SECTIONS
    if unexpected:
        raise ValueError(f"{ctx}: unexpected sections: {unexpected}")

    bench = data.get("benchmark")
    if not bench:
        raise ValueError(f"{ctx}: missing [benchmark] section")

    # Reject unknown benchmark fields
    unexpected_fields = set(bench.keys()) - set(_BENCHMARK_SCHEMA.keys())
    if unexpected_fields:
        raise ValueError(
            f"{ctx}: unexpected benchmark fields: {unexpected_fields}")

    # Validate each field against the schema
    for field_name, (expected_type, required, non_empty) in (
        _BENCHMARK_SCHEMA.items()
    ):
        if field_name not in bench:
            if required:
                raise ValueError(f"{ctx}: missing benchmark.{field_name}")
            continue
        val = bench[field_name]
        if not isinstance(val, expected_type):
            raise ValueError(
                f"{ctx}: benchmark.{field_name} must be "
                f"{expected_type.__name__}, got {type(val).__name__}")
        if non_empty and isinstance(val, str) and not val.strip():
            raise ValueError(
                f"{ctx}: benchmark.{field_name} must not be empty")

    # secondary items must be strings
    for i, item in enumerate(bench["secondary"]):
        if not isinstance(item, str):
            raise ValueError(
                f"{ctx}: benchmark.secondary[{i}] must be a string, "
                f"got {type(item).__name__}")

    # Parse [scale] profiles
    scale_raw = data.get("scale")
    if not scale_raw:
        raise ValueError(f"{ctx}: missing [scale] section")

    scale: dict[str, dict[str, str | int | float | bool]] = {}
    for profile_name, profile_params in scale_raw.items():
        if not isinstance(profile_params, dict) or not profile_params:
            raise ValueError(
                f"{ctx}: scale.{profile_name} must be a non-empty table")
        for k, v in profile_params.items():
            if not isinstance(v, (str, int, float, bool)):
                raise ValueError(
                    f"{ctx}: scale.{profile_name}.{k} must be a scalar, "
                    f"got {type(v).__name__}")
        scale[profile_name] = dict(profile_params)

    # Directory path must match category[/subcategory]/name
    name = bench["name"]
    category = bench["category"]
    subcategory = bench.get("subcategory")

    try:
        rel = (
            manifest_path.parent.resolve()
            .relative_to(fixtures_root.resolve())
        )
    except ValueError:
        raise ValueError(f"{ctx}: fixture not under {fixtures_root}")

    if subcategory:
        expected = (category, subcategory, name)
    else:
        expected = (category, name)

    if rel.parts != expected:
        raise ValueError(
            f"{ctx}: directory path '{'/'.join(rel.parts)}' does not match "
            f"manifest '{'/'.join(expected)}'")

    return Fixture(
        name=name,
        category=category,
        subcategory=subcategory,
        intent=bench["intent"],
        focus=bench["focus"],
        primary=bench["primary"],
        secondary=tuple(bench["secondary"]),
        path=manifest_path.parent,
        scale=scale,
    )


def discover_fixtures(fixtures_root: Path) -> list[Fixture]:
    """Discover, parse, and return fixtures sorted by (category, name)."""
    fixtures: list[Fixture] = []
    seen_names: dict[str, Path] = {}

    for manifest_path in sorted(fixtures_root.rglob("bench.toml")):
        fixture = parse_fixture_manifest(manifest_path, fixtures_root)

        if fixture.name in seen_names:
            raise ValueError(
                f"Duplicate fixture name '{fixture.name}': "
                f"{manifest_path} and {seen_names[fixture.name]}")
        seen_names[fixture.name] = manifest_path
        fixtures.append(fixture)

    fixtures.sort(key=lambda f: (f.category, f.subcategory or "", f.name))
    return fixtures


def resolve_fixture_params(
    fixture: Fixture, profile: str,
) -> dict[str, str | int | float | bool] | None:
    """Resolve parameters for a fixture given a scale profile.

    Returns None if the fixture does not have the requested profile.
    """
    params = fixture.scale.get(profile)
    if params is None:
        return None
    return dict(params)


def read_stats_json(path: str) -> dict:
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, json.JSONDecodeError):
        return {}


def populate_from_stats(result: BenchResult, stats: dict) -> None:
    """Populate phases, llvm_insts, and mir_stmts from stats JSON.

    Does NOT set compile_s -- callers set that explicitly because the source
    differs by backend: JIT derives it from phase timings, AOT uses wall time
    of the compile subprocess.
    """
    phases = stats.get("phases", {})
    result.phases = dict(phases)

    llvm = stats.get("llvm", {})
    result.llvm_insts = llvm.get("instructions", 0)

    mir = stats.get("mir", {})
    result.mir_stmts = mir.get("mir_stmts", 0)


def sum_compile_phases(stats: dict) -> float:
    """Sum all phase timings except 'sim' to get compile time."""
    total = 0.0
    for key, val in stats.get("phases", {}).items():
        if key == "sim":
            continue
        total += val
    return total


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


def make_result(fixture: Fixture, backend: str, params: dict) -> BenchResult:
    """Create a BenchResult pre-populated with fixture metadata."""
    return BenchResult(
        fixture=fixture.name,
        category=fixture.category,
        subcategory=fixture.subcategory,
        intent=fixture.intent,
        focus=fixture.focus,
        primary=fixture.primary,
        secondary=fixture.secondary,
        backend=backend,
        params=dict(params),
    )


def run_lyra_jit(
    lyra: str, fixture: Fixture, params: dict,
    stats_path: str,
) -> BenchResult:
    result = make_result(fixture, "jit", params)
    cmd = [
        lyra, "-C", str(fixture.path), "run",
        "--backend=jit",
        "--stats-out", stats_path,
    ]
    cmd.extend(build_define_args(params))
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
        result.compile_s = sum_compile_phases(stats)
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
    result = make_result(fixture, backend_name, params)

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
    result = make_result(fixture, "verilator", params)

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
        "-Wno-ALWCOMBORDER", "-Wno-UNOPTFLAT",
        "-Wno-MULTIDRIVEN", "-Wno-WIDTHCONCAT",
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

    if backend == "jit":
        return run_lyra_jit(lyra, fixture, params, stats_path)
    elif backend in ("aot", "aot-two-state"):
        two_state = backend == "aot-two-state"
        aot_out = os.path.join(tmpdir, tag)
        os.makedirs(aot_out, exist_ok=True)
        return run_lyra_aot(
            lyra, fixture, params, stats_path, aot_out, two_state)
    elif backend == "verilator":
        ver_dir = os.path.join(tmpdir, tag)
        os.makedirs(ver_dir, exist_ok=True)
        return run_verilator(fixture, params, ver_dir)
    else:
        result = make_result(fixture, backend, params)
        result.error = f"unknown backend: {backend}"
        return result


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


def group_fixture_backends(
    results: list[BenchResult],
) -> dict[str, dict[str, BenchResult]]:
    """Group results into {fixture: {backend: result}}."""
    grouped: dict[str, dict[str, BenchResult]] = {}
    for r in results:
        grouped.setdefault(r.fixture, {})[r.backend] = r
    return grouped


def print_runtime_table(
    fixture_names: list[str],
    by_fixture: dict[str, dict[str, BenchResult]],
) -> None:
    """Print a runtime sim_s table for a group of fixtures."""
    print(
        "| Fixture | Lyra 4s (ms) "
        "| Lyra 2s (ms) | Verilator (ms) |")
    print(
        "|---------|-------------:"
        "|-------------:|---------------:|")

    for name in fixture_names:
        backends = by_fixture.get(name, {})
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

        print(f"| {name} | {aot_sim} | {aot_2s_sim} | {ver_sim} |")


def print_compile_table(
    fixture_names: list[str],
    by_fixture: dict[str, dict[str, BenchResult]],
) -> None:
    """Print a compile-focused table for a group of fixtures."""
    print(
        "| Fixture | AOT (ms) "
        "| JIT (ms) | Verilator (ms) "
        "| LLVM insts | Binary (KB) |")
    print(
        "|---------|--------:"
        "|--------:|---------------:"
        "|-----------:|------------:|")

    for name in fixture_names:
        backends = by_fixture.get(name, {})
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
        llvm = (
            fmt_int(aot.llvm_insts) if aot and not aot.error
            else "-")
        binary = (
            fmt_int(aot.binary_kb) if aot and not aot.error
            else "-")

        print(
            f"| {name} | {aot_c} | {jit_c} | {ver_c} "
            f"| {llvm} | {binary} |")


def build_grouped_fixtures(
    results: list[BenchResult],
) -> dict[tuple[str, str | None], list[str]]:
    """Group fixture names by (category, subcategory) in stable order."""
    groups: dict[tuple[str, str | None], list[str]] = {}
    seen: set[str] = set()
    for r in sorted(
        results,
        key=lambda r: (r.category, r.subcategory or "", r.fixture),
    ):
        if r.fixture in seen:
            continue
        seen.add(r.fixture)
        key = (r.category, r.subcategory)
        groups.setdefault(key, []).append(r.fixture)
    return groups


def print_markdown(
    results: list[BenchResult], num_trials: int, tier: str, profile: str,
    num_discovered: int, num_runnable: int,
    skipped: list[tuple[str, str]],
) -> None:
    trial_note = f"{num_trials} (median)" if num_trials > 1 else "1"
    by_fixture = group_fixture_backends(results)

    _KNOWN_FOCUS = {"runtime", "compile"}
    unknown_focus = {r.focus for r in results} - _KNOWN_FOCUS
    if unknown_focus:
        print(
            f"ERROR: unknown focus values: {unknown_focus}",
            file=sys.stderr,
        )
        sys.exit(1)

    runtime_results = [r for r in results if r.focus == "runtime"]
    compile_results = [r for r in results if r.focus == "compile"]

    print()
    print("## Lyra Benchmark Report")
    print()
    print(
        f"> git: `{get_git_sha()}` | tier: {tier} | "
        f"profile: {profile} | trials: {trial_note}")
    print(
        f"> discovered: {num_discovered} | "
        f"runnable: {num_runnable} | "
        f"skipped: ", end="")
    if skipped:
        skip_items = [f"{name} ({reason})" for name, reason in skipped]
        print(", ".join(skip_items))
    else:
        print("none")

    if runtime_results:
        print()
        print("# Runtime Benchmarks")

        runtime_groups = build_grouped_fixtures(runtime_results)

        # Collect all fixtures per category into a flat list.
        cat_fixtures: dict[str, list[str]] = {}
        for (category, _subcategory), fixtures in runtime_groups.items():
            cat_fixtures.setdefault(category, []).extend(fixtures)

        for category in cat_fixtures:
            print()
            print(f"## {category}")
            print()
            print_runtime_table(cat_fixtures[category], by_fixture)

    if compile_results:
        print()
        print("# Compile Benchmarks")

        compile_groups = build_grouped_fixtures(compile_results)
        current_category = ""

        for (category, subcategory), fixtures in compile_groups.items():
            if category != current_category:
                print()
                print(f"## {category}")
                current_category = category

            if subcategory:
                print()
                print(f"### {subcategory}")

            print()
            print_compile_table(fixtures, by_fixture)

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
        "category": r.category,
        "subcategory": r.subcategory,
        "intent": r.intent,
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
    results: list[BenchResult], path: str, tier: str, profile: str,
) -> None:
    data = {
        "schema_version": 3,
        "git": get_git_sha(),
        "tier": tier,
        "profile": profile,
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "results": [result_to_dict(r) for r in results],
    }
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def main() -> None:
    tier_configs = load_config()

    parser = argparse.ArgumentParser(description="Lyra performance benchmarks")
    parser.add_argument(
        "--json", default=None, help="Write JSON results to this path")
    parser.add_argument(
        "--trials", type=int, default=None, help="Override number of trials")
    parser.add_argument(
        "--tier", choices=sorted(tier_configs.keys()), default="nightly",
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

    # Resolve tier config
    tier_name = args.tier
    tier_cfg = tier_configs[tier_name]
    profile = tier_cfg.profile
    num_trials = (
        args.trials if args.trials is not None
        else tier_cfg.trials)

    # Filter fixtures by profile availability
    runnable: list[tuple[Fixture, dict]] = []
    skipped: list[tuple[str, str]] = []

    for fixture in all_fixtures:
        params = resolve_fixture_params(fixture, profile)
        if params is not None:
            runnable.append((fixture, params))
        else:
            skipped.append((fixture.name, f"no '{profile}' profile"))

    num_discovered = len(all_fixtures)
    num_runnable = len(runnable)

    if not runnable:
        print(
            f"Error: no fixtures runnable for tier '{tier_name}' "
            f"(profile: {profile})", file=sys.stderr)
        sys.exit(1)

    # Print discovery summary
    print(
        f"Discovered {num_discovered} fixtures, "
        f"{num_runnable} runnable for tier '{tier_name}' "
        f"(profile: {profile})", file=sys.stderr)
    if skipped:
        skip_items = [f"{name} ({reason})" for name, reason in skipped]
        print(f"  Skipped: {', '.join(skip_items)}", file=sys.stderr)

    all_results: list[BenchResult] = []
    has_failure = False

    with tempfile.TemporaryDirectory(prefix="lyra-bench-") as tmpdir:
        for fixture, params in runnable:
            for backend in BACKENDS:
                r = run_fixture_backend(
                    lyra, fixture, params,
                    backend, num_trials, tmpdir)
                all_results.append(r)
                if r.error and r.error != "verilator not found":
                    has_failure = True

    print_markdown(
        all_results, num_trials, tier_name, profile,
        num_discovered, num_runnable, skipped)

    if args.json:
        write_json(all_results, args.json, tier_name, profile)
        print(f"JSON written to {args.json}", file=sys.stderr)

    if has_failure and not args.ci:
        sys.exit(1)


if __name__ == "__main__":
    main()
