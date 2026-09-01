#!/usr/bin/env python3
"""Lyra benchmark runner.

Runs every case under tests/benchmark/ against Lyra and a reference simulator,
choosing how much work each one does so that every measurement lands in the
same duration, and reports the rate each tool sustained.

Usage:
    python3 tools/bench/run_benchmarks.py [--json PATH] [--filter SUBSTRING]
                                          [--seconds N]
"""

import argparse
import json
import os
import shutil
import subprocess
import sys
import tempfile
import threading
import time
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
CORPUS_ROOT = REPO_ROOT / "tests" / "benchmark"

CASE_ENTRY = "main.sv"

# The top-level module every case elaborates, and the name it takes its work
# count under. Both are fixed rather than declared, so a case says only what a
# reader could not guess.
#
# A case that times a simulation reads the count from a plusarg, so one build
# serves every amount. A case that times a build has no such option -- its
# work is the design -- and takes the count as a top-level parameter override,
# which says it in the language rather than in the preprocessor and leaves the
# default written once, on the parameter itself.
CASE_TOP = "Top"
WORK_PARAM = "WORK"

# Long enough that process startup and timer resolution are a rounding error
# against a measurement, short enough that reaching it a few times per case is
# not what a run costs. A case whose single unit already overshoots it costs
# what that unit costs, whatever this says.
DEFAULT_TARGET_SECONDS = 1.0

# A build is far slower than a run, and most of a small one is the compiler
# starting rather than the design. The target has to clear that fixed part by
# enough that what varies with the design is the bulk of the reading.
BUILD_TARGET_SECONDS = 20.0

TIMEOUT_SECONDS = 900

# Below this, a duration is startup and scheduling rather than the work, so
# nothing can be extrapolated from it.
MIN_USEFUL_SECONDS = 0.002

# One probe may not multiply the amount by more than this. A first probe that
# lands near zero would otherwise ask for an amount that never finishes.
MAX_GROWTH = 1000

MAX_PROBES = 6

BINARY_NAME = "program"

UNSUPPORTED_MARKER = "lyra: unsupported:"

STATUS_OK = "ok"
STATUS_UNSUPPORTED = "unsupported"
STATUS_ERROR = "error"

MEASURE_BUILD = "build"
MEASURE_RUN = "run"

_DIRECTIVES = {"measure", "work"}


@dataclass(frozen=True)
class Case:
    family: str
    name: str
    measure: str
    work_unit: str
    sources: tuple[Path, ...]

    @property
    def path(self) -> str:
        return f"{self.family}/{self.name}"


@dataclass
class Result:
    case: str = ""
    family: str = ""
    measure: str = ""
    work_unit: str = ""
    tool: str = ""
    status: str = STATUS_OK
    work: int = 0
    seconds: float = 0.0
    rate: float = 0.0
    build_s: float = 0.0
    binary_kb: int = 0
    probes: int = 0
    detail: str = ""


@dataclass
class ProcessRun:
    """One subprocess and what running it cost."""
    elapsed_s: float
    returncode: int
    stdout: str
    stderr: str
    timed_out: bool


def run_process(
    cmd: list[str], cwd: str | None = None, env: dict | None = None,
) -> ProcessRun:
    """Run one command to completion and time it."""
    with (
        tempfile.TemporaryFile(mode="w+") as out,
        tempfile.TemporaryFile(mode="w+") as err,
    ):
        start = time.monotonic()
        proc = subprocess.Popen(
            cmd, stdout=out, stderr=err, cwd=cwd, env=env, text=True)

        killed: list[bool] = []

        def on_timeout() -> None:
            killed.append(True)
            proc.kill()

        watchdog = threading.Timer(TIMEOUT_SECONDS, on_timeout)
        watchdog.start()
        proc.wait()
        elapsed = time.monotonic() - start
        watchdog.cancel()

        out.seek(0)
        err.seek(0)
        return ProcessRun(
            elapsed_s=elapsed,
            returncode=proc.returncode,
            stdout=out.read(),
            stderr=err.read(),
            timed_out=bool(killed),
        )


def parse_directives(entry: Path) -> dict[str, str]:
    """Read the `// @key: value` lines that open a case's entry file.

    An unrecognized key is an error rather than a line quietly ignored, so a
    misspelled directive cannot look like a case that simply declares nothing.
    """
    directives: dict[str, str] = {}
    for line in entry.read_text().splitlines():
        stripped = line.strip()
        if not stripped.startswith("//"):
            break
        body = stripped[2:].strip()
        if not body.startswith("@"):
            break
        key, sep, value = body[1:].partition(":")
        if not sep:
            raise ValueError(f"{entry}: directive '{body}' has no value")
        key = key.strip()
        if key not in _DIRECTIVES:
            raise ValueError(
                f"{entry}: unknown directive '@{key}'; known are "
                f"{sorted(_DIRECTIVES)}")
        directives[key] = value.strip()
    return directives


def read_case(entry: Path, corpus_root: Path) -> Case:
    """Build a Case from its entry file, or raise ValueError saying why not."""
    directives = parse_directives(entry)

    measure = directives.get("measure")
    if measure not in (MEASURE_BUILD, MEASURE_RUN):
        raise ValueError(
            f"{entry}: @measure must be '{MEASURE_BUILD}' or '{MEASURE_RUN}'")

    work_unit = directives.get("work")
    if not work_unit:
        raise ValueError(
            f"{entry}: @work must name what one unit of this case's work is "
            f"called, so its duration can be read as a rate")

    relative = entry.parent.resolve().relative_to(corpus_root.resolve())
    if len(relative.parts) != 2:
        raise ValueError(
            f"{entry}: a case sits at <family>/<name>, not "
            f"'{'/'.join(relative.parts)}'")
    family, name = relative.parts

    # Companions are compiled before the entry, in name order, because a
    # reference reaches only what a compilation unit declared before it
    # (LRM 3.12.1).
    companions = sorted(
        p for p in entry.parent.glob("*.sv") if p.name != CASE_ENTRY)

    return Case(
        family=family,
        name=name,
        measure=measure,
        work_unit=work_unit,
        sources=(*companions, entry),
    )


def discover_cases(corpus_root: Path) -> list[Case]:
    cases = [
        read_case(entry, corpus_root)
        for entry in sorted(corpus_root.rglob(CASE_ENTRY))
    ]
    seen: dict[str, str] = {}
    for case in cases:
        if case.name in seen:
            raise ValueError(
                f"duplicate case name '{case.name}': "
                f"{case.path} and {seen[case.name]}")
        seen[case.name] = case.path
    cases.sort(key=lambda c: (c.family, c.name))
    return cases


def next_amount(
    history: list[tuple[int, float]], target: float,
) -> int:
    """The amount to probe next, from what the previous probes cost.

    A measurement is a fixed cost plus a cost per unit of work, and the two
    readings furthest apart separate them. Assuming proportionality instead
    would undershoot badly wherever the fixed part is large, which is every
    case that measures a build: the compiler's own startup is seconds before
    any of the design is read.

    One reading cannot separate them, so the first step scales proportionally
    and is corrected by the second. Every step is capped, because a reading at
    the resolution floor says nothing about how far the target is.
    """
    amount, elapsed = history[-1]
    if len(history) >= 2:
        first_amount, first_elapsed = history[0]
        per_unit = (elapsed - first_elapsed) / (amount - first_amount)
        fixed = elapsed - per_unit * amount
        if per_unit > 0.0 and target > fixed:
            grown = (target - fixed) / per_unit
            return max(amount + 1, min(int(grown), amount * MAX_GROWTH))
    grown = amount * target / max(elapsed, MIN_USEFUL_SECONDS)
    return max(amount + 1, min(int(grown), amount * MAX_GROWTH))


def converge(
    measure_at: Callable[[int], ProcessRun], target: float,
) -> tuple[int, ProcessRun, list[tuple[int, float]]]:
    """Raise the amount of work until one measurement reaches the target.

    Returns the amount that produced the final reading, that reading, and every
    (amount, duration) pair taken along the way -- those pairs are what separate
    what a measurement costs before any work from what it costs per unit. A case
    too slow to reach the target even at one unit stops there and reports what
    one unit cost, which is a rate like any other.
    """
    amount = 1
    run = measure_at(amount)
    history = [(amount, run.elapsed_s)]
    while (
        run.returncode == 0
        and not run.timed_out
        # A second reading is taken even when the first already reached the
        # target, because one reading cannot say which part of it was the work
        # and which was there before any: a case that has to fill an array
        # before reading it pays for the fill once, and charging that to the
        # single pass that followed reports the fill instead of the read.
        and (run.elapsed_s < target or len(history) < 2)
        and len(history) < MAX_PROBES
    ):
        candidate = next_amount(history, target)
        if candidate == amount:
            break
        amount = candidate
        run = measure_at(amount)
        history.append((amount, run.elapsed_s))
    return amount, run, history


def marginal_rate(history: list[tuple[int, float]]) -> float:
    """Units of work per second, with what a run costs before any of it removed.

    The fixed part is whatever a measurement pays regardless of how much work
    it covers -- a process starting, a compiler reading its prelude. It is a
    rounding error against a simulation and it is most of a small build, so
    leaving it in would make a build case's number move whenever the prelude
    cache did. Two readings separate it; with only one, there is nothing to
    separate and the whole duration is charged to the work.
    """
    amount, elapsed = history[-1]
    if len(history) >= 2:
        first_amount, first_elapsed = history[0]
        per_unit = (elapsed - first_elapsed) / (amount - first_amount)
        if per_unit > 0.0:
            return 1.0 / per_unit
    return amount / elapsed if elapsed > 0.0 else 0.0


def lyra_build_command(
    lyra: str, case: Case, out_dir: str, amount: int | None,
) -> list[str]:
    cmd = [lyra, "compile"]
    # A simulation is timed on the code a user would ship, so its translation
    # unit is optimized. A build is timed the way an edit loop builds, since
    # that is the cost it exists to protect and optimizing it would measure the
    # host compiler's optimizer instead.
    if case.measure == MEASURE_RUN:
        cmd.append("--release")
    cmd.extend(["--top", CASE_TOP])
    cmd.extend(["-o", out_dir])
    if amount is not None:
        cmd.extend(["-G", f"{WORK_PARAM}={amount}"])
    cmd.extend(str(s) for s in case.sources)
    return cmd


def verilator_build_command(
    verilator: str, case: Case, amount: int | None,
) -> list[str]:
    cmd = [
        verilator, "--binary", "--top-module", CASE_TOP,
        # Style opinions about a benchmark case are not what is being
        # measured, and listing the ones to silence is a list that grows
        # every time a case gets bigger. Errors still stop the build.
        "-Wno-fatal",
        # A case whose work is its own size is grown until building it takes
        # the target duration, and the faster tool is therefore handed the
        # larger design. Its default ceiling on unrolling a generate loop is
        # reached long before that, which would report the ceiling rather
        # than the speed.
        "--unroll-limit", "1000000",
    ]
    if amount is not None:
        cmd.append(f"-G{WORK_PARAM}={amount}")
    cmd.extend(str(s) for s in case.sources)
    return cmd


@dataclass(frozen=True)
class Tool:
    """One simulator, and how to build a case with it and find what it made."""
    name: str
    build: Callable[[Case, str, int | None], list[str]]
    binary: Callable[[str], Path]
    env: dict | None = None
    unsupported_marker: str = ""


def measure_run_case(
    case: Case, tool: Tool, result: Result, root: str, target: float,
) -> Result:
    """Build once, then raise the work count until a run reaches the target."""
    work = os.path.join(root, "build")
    os.makedirs(work, exist_ok=True)
    build = run_process(tool.build(case, work, None), cwd=work, env=tool.env)
    result.build_s = build.elapsed_s

    if not record_build_failure(result, build, tool):
        return result

    binary = tool.binary(work)
    if not binary.is_file():
        result.status = STATUS_ERROR
        result.detail = f"the build produced no {binary.name}"
        return result
    result.binary_kb = round(binary.stat().st_size / 1024)

    amount, run, history = converge(
        lambda n: run_process([str(binary), f"+work={n}"]), target)
    record_measurement(result, amount, run, history)
    return result


def measure_build_case(
    case: Case, tool: Tool, result: Result, root: str, target: float,
) -> Result:
    """Raise the design's size until building it reaches the target.

    A case whose subject is the build has no runtime amount to vary: its work
    is the design, so the amount is a parameter the design is elaborated with
    and every probe is a fresh build in a directory nothing has built in.
    """
    builds: list[str] = []

    def build_at(amount: int) -> ProcessRun:
        work = os.path.join(root, f"build-{len(builds)}")
        os.makedirs(work, exist_ok=True)
        builds.append(work)
        return run_process(
            tool.build(case, work, amount), cwd=work, env=tool.env)

    amount, build, history = converge(build_at, target)
    result.build_s = build.elapsed_s

    if not record_build_failure(result, build, tool):
        return result

    binary = tool.binary(builds[-1])
    if not binary.is_file():
        result.status = STATUS_ERROR
        result.detail = f"the build produced no {binary.name}"
        return result
    result.binary_kb = round(binary.stat().st_size / 1024)

    # The artifact is run once, not timed: a build case earns its number from
    # the build, and running it is what says the build produced something real.
    proof = run_process([str(binary), "+work=1"])
    if proof.returncode != 0:
        result.status = STATUS_ERROR
        result.detail = (
            first_error_line(proof) or f"exit code {proof.returncode}")
        return result

    record_measurement(result, amount, build, history)
    return result


def record_build_failure(
    result: Result, build: ProcessRun, tool: Tool,
) -> bool:
    """Record why a build failed. Returns True when it did not."""
    if build.timed_out:
        result.status = STATUS_ERROR
        result.detail = f"build timed out after {TIMEOUT_SECONDS}s"
        return False
    if build.returncode != 0:
        refused = (
            tool.unsupported_marker != ""
            and tool.unsupported_marker in build.stderr)
        result.status = STATUS_UNSUPPORTED if refused else STATUS_ERROR
        result.detail = (
            first_error_line(build) or f"build exit code {build.returncode}")
        return False
    return True


def record_measurement(
    result: Result, amount: int, run: ProcessRun,
    history: list[tuple[int, float]],
) -> None:
    result.probes = len(history)
    if run.timed_out:
        result.status = STATUS_ERROR
        result.detail = f"timed out after {TIMEOUT_SECONDS}s"
        return
    if run.returncode != 0:
        result.status = STATUS_ERROR
        result.detail = first_error_line(run) or f"exit code {run.returncode}"
        return
    result.work = amount
    result.seconds = run.elapsed_s
    result.rate = marginal_rate(history)


def first_error_line(run: ProcessRun) -> str:
    for text in (run.stderr, run.stdout):
        for line in text.splitlines():
            if "error" in line.lower() or "unsupported" in line.lower():
                return line.strip()
    return ""


def lyra_tool(lyra: str) -> Tool:
    return Tool(
        name="lyra",
        build=lambda case, out, amount: lyra_build_command(
            lyra, case, out, amount),
        binary=lambda out: Path(out) / BINARY_NAME,
        unsupported_marker=UNSUPPORTED_MARKER,
    )


def verilator_tool(verilator: str) -> Tool:
    # Verilator's own build is what is being timed, so the compiler cache is
    # kept out of it.
    env = os.environ.copy()
    env["CCACHE_DISABLE"] = "1"
    return Tool(
        name="verilator",
        build=lambda case, out, amount: verilator_build_command(
            verilator, case, amount),
        binary=lambda out: Path(out) / "obj_dir" / f"V{CASE_TOP}",
        env=env,
    )


def measure(case: Case, tool: Tool, tmpdir: str, target: float) -> Result:
    result = Result(
        case=case.name,
        family=case.family,
        measure=case.measure,
        work_unit=case.work_unit,
        tool=tool.name,
    )
    root = os.path.join(tmpdir, f"{case.name}-{tool.name}")
    os.makedirs(root, exist_ok=True)

    if case.measure == MEASURE_BUILD:
        return measure_build_case(
            case, tool, result, root, BUILD_TARGET_SECONDS)
    return measure_run_case(case, tool, result, root, target)


def fmt_rate(rate: float) -> str:
    if rate <= 0.0:
        return "-"
    if rate < 10.0:
        return f"{rate:.3g}"
    return f"{round(rate):,}"


def fmt_factor(factor: float) -> str:
    if factor >= 10.0:
        return f"{round(factor):,}x"
    return f"{factor:.1f}x"


def fmt_comparison(lyra_rate: float, other_rate: float) -> str:
    if lyra_rate <= 0.0 or other_rate <= 0.0:
        return "-"
    if lyra_rate >= other_rate * 1.2:
        return f"{fmt_factor(lyra_rate / other_rate)} faster"
    if other_rate >= lyra_rate * 1.2:
        return f"{fmt_factor(other_rate / lyra_rate)} slower"
    return "~1x"


def get_git_sha() -> str:
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True, cwd=REPO_ROOT,
        ).stdout.strip()
    except OSError:
        return "unknown"


def print_report(results: list[Result], target: float) -> None:
    by_case: dict[str, dict[str, Result]] = {}
    order: list[tuple[str, str]] = []
    for r in results:
        if r.case not in by_case:
            order.append((r.family, r.case))
        by_case.setdefault(r.case, {})[r.tool] = r

    print()
    print("## Lyra Benchmark Report")
    print()
    print(f"> git: `{get_git_sha()}` | target: {target:g}s per measurement")
    print(
        "> Each tool is given the amount of work it needs to reach that "
        "target, so a rate is comparable across tools, machines, and runs.")

    families: dict[str, list[str]] = {}
    for family, case in sorted(order):
        families.setdefault(family, []).append(case)

    for family, cases in families.items():
        print()
        print(f"## {family}")
        print()
        print(
            "| Case | Unit | Lyra /s | Verilator /s | vs Verilator "
            "| Lyra work | Binary (KB) |")
        print(
            "|------|------|--------:|-------------:|:-------------"
            "|----------:|------------:|")
        for name in cases:
            tools = by_case[name]
            lyra = tools.get("lyra")
            ver = tools.get("verilator")
            lyra_ok = lyra is not None and lyra.status == STATUS_OK
            ver_ok = ver is not None and ver.status == STATUS_OK
            lyra_rate = lyra.rate if lyra_ok else 0.0
            ver_rate = ver.rate if ver_ok else 0.0
            unit = lyra.work_unit if lyra else ""
            work = f"{lyra.work:,}" if lyra_ok else "-"
            binary = f"{lyra.binary_kb:,}" if lyra_ok else "-"
            print(
                f"| {name} | {unit} | {fmt_rate(lyra_rate)} "
                f"| {fmt_rate(ver_rate)} "
                f"| {fmt_comparison(lyra_rate, ver_rate)} "
                f"| {work} | {binary} |")

    for status, heading in (
        (STATUS_UNSUPPORTED, "Not measured"),
        (STATUS_ERROR, "Errors"),
    ):
        rows = [r for r in results if r.status == status]
        if not rows:
            continue
        print()
        print(f"### {heading}")
        print()
        for r in rows:
            print(f"- **{r.case}/{r.tool}**: {r.detail}")

    print()


def result_to_dict(r: Result) -> dict:
    return {
        "case": r.case,
        "family": r.family,
        "measure": r.measure,
        "work_unit": r.work_unit,
        "tool": r.tool,
        "status": r.status,
        "work": r.work,
        "seconds": r.seconds,
        "rate": r.rate,
        "build_s": r.build_s,
        "binary_kb": r.binary_kb,
        "probes": r.probes,
        "detail": r.detail,
    }


def write_json(results: list[Result], path: str, target: float) -> None:
    data = {
        "schema_version": 5,
        "git": get_git_sha(),
        "target_seconds": target,
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "results": [result_to_dict(r) for r in results],
    }
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def main() -> None:
    parser = argparse.ArgumentParser(description="Lyra benchmarks")
    parser.add_argument("--json", default=None, help="Write results here")
    parser.add_argument(
        "--filter", default=None,
        help="Run only cases whose name contains this substring")
    parser.add_argument(
        "--seconds", type=float, default=DEFAULT_TARGET_SECONDS,
        help="Target duration of one measurement")
    args = parser.parse_args()

    lyra = str(REPO_ROOT / "bazel-bin" / "lyra")
    if not os.path.isfile(lyra):
        print(f"Error: lyra binary not found at {lyra}", file=sys.stderr)
        sys.exit(1)

    cases = discover_cases(CORPUS_ROOT)
    if args.filter:
        cases = [c for c in cases if args.filter in c.name]
    if not cases:
        print("Error: no cases found", file=sys.stderr)
        sys.exit(1)

    tools = [lyra_tool(lyra)]
    verilator = shutil.which("verilator")
    if verilator:
        tools.append(verilator_tool(verilator))
    else:
        print("verilator not on PATH; Lyra only", file=sys.stderr)

    print(f"Running {len(cases)} cases", file=sys.stderr)

    results: list[Result] = []
    with tempfile.TemporaryDirectory(prefix="lyra-bench-") as tmpdir:
        for case in cases:
            for tool in tools:
                print(f"  {case.path}/{tool.name}", end="", flush=True,
                      file=sys.stderr)
                r = measure(case, tool, tmpdir, args.seconds)
                results.append(r)
                print(f" -> {r.status}", file=sys.stderr)

    print_report(results, args.seconds)

    if args.json:
        write_json(results, args.json, args.seconds)
        print(f"JSON written to {args.json}", file=sys.stderr)

    if any(r.status == STATUS_ERROR for r in results):
        sys.exit(1)


if __name__ == "__main__":
    main()
