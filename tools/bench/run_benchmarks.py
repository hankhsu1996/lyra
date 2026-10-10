#!/usr/bin/env python3
"""Lyra benchmark runner.

Runs every case under tests/benchmark/ against Lyra and a reference simulator,
choosing how much work each one does so that every measurement lands in the
same duration, and reports the rate each tool sustained.

A case that cannot be measured costs only itself: the run goes on, the report
carries every number that was obtained, and the case is named there with the
step that failed and how it ended. The exit status is 0 when every case was
measured and 1 when any was not, or when there was nothing to run.

Usage:
    python3 tools/bench/run_benchmarks.py [--json PATH] [--filter SUBSTRING]
                                          [--seconds N] [--lyra PATH]
"""

import argparse
import json
import math
import os
import shutil
import signal
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

# How much of what a failed step printed is kept. The reason is nearly always
# at the end, and a host compiler that fails prints thousands of lines first.
OUTPUT_TAIL_LINES = 20

# A reading whose work took less than this share of the target is mostly what
# a measurement costs before any work, so it says nothing about how far the
# target is.
MEASURED_SHARE = 0.1

# How far one probe may raise the amount past the last one: after a reading that
# says nothing, and after one that does. Nothing is known about how a case's
# cost grows with its amount -- a build's amount is the design, and a design's
# cost may be a square -- so a probe never reaches far past what was measured,
# and what bounds an overshoot is these factors raised to that unknown power.
BLIND_GROWTH = 4
MEASURED_GROWTH = 2

# No tool is asked for more work than this, and each tool's own ceiling on a
# generate loop is raised to it, so what a case reports is the tool's speed
# and never where its default ceiling happened to be.
MAX_AMOUNT = 1_000_000_000

BINARY_NAME = "program"

STATUS_OK = "ok"
STATUS_ERROR = "error"

# What a report says in place of a number, one word per way of having none, so
# that neither can be read as the other or as a rate of zero.
CELL_FAILED = "failed"
CELL_NOT_RUN = "not run"

# The tools a report has a column for.
LYRA = "lyra"
VERILATOR = "verilator"

MEASURE_BUILD = "build"
MEASURE_RUN = "run"

_DIRECTIVES = {"measure", "work", "work-max"}


@dataclass(frozen=True)
class Case:
    family: str
    name: str
    measure: str
    work_unit: str
    work_max: int
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
    readings: tuple[tuple[int, float], ...] = ()
    growth: float = 0.0
    detail: str = ""
    output: str = ""


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
    # What a step printed is only ever shown to a reader, and a program that
    # faults may have printed bytes that are no text at all.
    with (
        tempfile.TemporaryFile(mode="w+", errors="backslashreplace") as out,
        tempfile.TemporaryFile(mode="w+", errors="backslashreplace") as err,
    ):
        start = time.monotonic()
        # The command leads a process group of its own, so that stopping it
        # stops the host compiler it started too. One left running would be
        # taking the machine from the measurement after it.
        proc = subprocess.Popen(
            cmd, stdout=out, stderr=err, cwd=cwd, env=env, text=True,
            start_new_session=True)

        killed: list[bool] = []

        def stop() -> None:
            try:
                os.killpg(proc.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass

        def on_timeout() -> None:
            killed.append(True)
            stop()

        watchdog = threading.Timer(TIMEOUT_SECONDS, on_timeout)
        watchdog.start()
        try:
            proc.wait()
            elapsed = time.monotonic() - start
        finally:
            watchdog.cancel()
            if proc.poll() is None:
                stop()
                proc.wait()

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

    work_max = MAX_AMOUNT
    if "work-max" in directives:
        try:
            work_max = int(directives["work-max"])
        except ValueError:
            work_max = 0
        if not 2 <= work_max <= MAX_AMOUNT:
            raise ValueError(
                f"{entry}: @work-max must be a count from 2 to {MAX_AMOUNT}, "
                f"the most units of work this case can hold")

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
        work_max=work_max,
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
    history: list[tuple[int, float]], target: float, ceiling: int,
) -> int:
    """The amount to probe next, from what the previous probes cost.

    A measurement is a fixed cost plus the cost of the work, and the first
    reading stands for the fixed part: a build's is the compiler starting,
    seconds before any of the design is read. One reading cannot separate the
    two, so the first is read as all work. While the work is too small a share
    of a reading to be told from the fixed part, the amount is only multiplied.

    Once it shows, the line through the first reading and the last says where
    the target is, and it is believed no further than one step past the last
    amount. Suites that repeat one operation follow such a line much further,
    because repeating is proportional by construction; here the amount may be
    the size of a design, whose cost a line understates wherever it bends.
    """
    amount, elapsed = history[-1]
    origin_amount, origin_elapsed = (
        history[0] if len(history) >= 2 else (0, 0.0))
    work_s = elapsed - origin_elapsed
    if work_s < target * MEASURED_SHARE:
        return min(ceiling, amount * BLIND_GROWTH)
    per_unit = work_s / (amount - origin_amount)
    on_the_line = origin_amount + int((target - origin_elapsed) / per_unit)
    grown = min(on_the_line, amount * MEASURED_GROWTH)
    return min(ceiling, max(amount + 1, grown))


def converge(
    measure_at: Callable[[int], ProcessRun], target: float, ceiling: int,
) -> tuple[int, ProcessRun, list[tuple[int, float]]]:
    """Raise the amount of work until one measurement reaches the target.

    Returns the amount that produced the final reading, that reading, and every
    (amount, duration) pair taken along the way -- those pairs are what separate
    what a measurement costs before any work from what it costs per unit. A case
    too slow to reach the target even at one unit stops there and reports what
    one unit cost, which is a rate like any other; so does one that reaches the
    most work it can hold before it reaches the target.
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
    ):
        candidate = next_amount(history, target, ceiling)
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


def growth_exponent(history: list[tuple[int, float]], target: float) -> float:
    """The power of the amount the work's cost grew as, or zero when unseen.

    A rate is one number only where this is one: there, twice the work costs
    twice the time whatever the amount, so the amount drops out. Anywhere else
    the rate belongs to the amount it was taken at, and two tools given
    different amounts were not measured on the same thing.

    It is read off the last reading and the nearest one at most half its
    amount: two readings closer than that differ by less than the noise in
    either, and one further back says how the cost grew somewhere the search
    has since left. Where the work does not show in that reading, nothing is
    said.
    """
    first_amount, first_elapsed = history[0]
    far_amount, far_elapsed = history[-1]
    far_s = far_elapsed - first_elapsed
    for near_amount, near_elapsed in reversed(history[1:-1]):
        if far_amount < 2 * near_amount:
            continue
        near_s = near_elapsed - first_elapsed
        if near_s < target * MEASURED_SHARE or far_s <= near_s:
            return 0.0
        return (
            math.log(far_s / near_s)
            / math.log(
                (far_amount - first_amount) / (near_amount - first_amount)))
    return 0.0


def lyra_build_command(
    lyra: str, case: Case, out_dir: str, amount: int | None,
) -> list[str]:
    # Every build is a build: a program kept from an earlier probe of the same
    # amount would be copied rather than compiled, and time the copy.
    cmd = [lyra, "build", "--rebuild"]
    # A simulation is timed on the code a user would ship, so its translation
    # unit is optimized. A build is timed the way an edit loop builds, since
    # that is the cost it exists to protect and optimizing it would measure the
    # host compiler's optimizer instead.
    if case.measure == MEASURE_RUN:
        cmd.append("--release")
    cmd.extend(["--top", CASE_TOP])
    cmd.extend(["--max-generate-steps", str(MAX_AMOUNT)])
    cmd.extend(["-o", str(Path(out_dir) / BINARY_NAME)])
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
        "--unroll-limit", str(MAX_AMOUNT),
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


def measure_run_case(
    case: Case, tool: Tool, result: Result, root: str, target: float,
) -> Result:
    """Build once, then raise the work count until a run reaches the target."""
    work = os.path.join(root, "build")
    os.makedirs(work, exist_ok=True)
    build = run_process(tool.build(case, work, None), cwd=work, env=tool.env)
    result.build_s = build.elapsed_s

    if record_failure(result, "build", build):
        return result

    binary = tool.binary(work)
    if not binary.is_file():
        result.status = STATUS_ERROR
        result.detail = f"build produced no {binary.name}"
        return result
    result.binary_kb = round(binary.stat().st_size / 1024)

    amount, run, history = converge(
        lambda n: run_process([str(binary), f"+work={n}"]), target,
        case.work_max)
    result.readings = tuple(history)
    if record_failure(result, f"run at +work={amount}", run):
        return result
    record_measurement(result, amount, run, history, target)
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

    amount, build, history = converge(build_at, target, case.work_max)
    result.build_s = build.elapsed_s
    result.readings = tuple(history)

    if record_failure(result, f"build at {WORK_PARAM}={amount}", build):
        return result

    binary = tool.binary(builds[-1])
    if not binary.is_file():
        result.status = STATUS_ERROR
        result.detail = (
            f"build at {WORK_PARAM}={amount} produced no {binary.name}")
        return result
    result.binary_kb = round(binary.stat().st_size / 1024)

    # The artifact is run once, not timed: a build case earns its number from
    # the build, and running it is what says the build produced something real.
    proof = run_process([str(binary), "+work=1"])
    if record_failure(
            result, f"run of the program built at {WORK_PARAM}={amount}",
            proof):
        return result

    record_measurement(result, amount, build, history, target)
    return result


def how_it_ended(run: ProcessRun) -> str:
    """How a step that did not succeed ended, as the rest of a sentence."""
    if run.timed_out:
        return f"was stopped after {TIMEOUT_SECONDS}s"
    if run.returncode < 0:
        number = -run.returncode
        try:
            name = signal.Signals(number).name
        except ValueError:
            name = f"signal {number}"
        # The status is the one a shell reports for it, which is the number a
        # reader who ran the program by hand has seen.
        return f"was killed by {name} (status {128 + number})"
    return f"exited with status {run.returncode}"


def last_lines(run: ProcessRun) -> str:
    """The end of what a step printed, from the stream that says why."""
    for text in (run.stderr, run.stdout):
        lines = text.rstrip().splitlines()
        if lines:
            return "\n".join(lines[-OUTPUT_TAIL_LINES:])
    return ""


def record_failure(result: Result, step: str, run: ProcessRun) -> bool:
    """Record that a step did not succeed, and how. Returns True if so."""
    if not run.timed_out and run.returncode == 0:
        return False
    result.status = STATUS_ERROR
    result.detail = f"{step} {how_it_ended(run)}"
    result.output = last_lines(run)
    return True


def record_measurement(
    result: Result, amount: int, run: ProcessRun,
    history: list[tuple[int, float]], target: float,
) -> None:
    result.work = amount
    result.seconds = run.elapsed_s
    result.rate = marginal_rate(history)
    result.growth = growth_exponent(history, target)


def lyra_tool(lyra: str) -> Tool:
    return Tool(
        name=LYRA,
        build=lambda case, out, amount: lyra_build_command(
            lyra, case, out, amount),
        binary=lambda out: Path(out) / BINARY_NAME,
    )


def verilator_tool(verilator: str) -> Tool:
    # Verilator's own build is what is being timed, so the compiler cache is
    # kept out of it.
    env = os.environ.copy()
    env["CCACHE_DISABLE"] = "1"
    return Tool(
        name=VERILATOR,
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

    # A step the operating system would not start -- a program that is not one,
    # a directory that cannot be made -- is this case's failure like any other.
    try:
        if case.measure == MEASURE_BUILD:
            return measure_build_case(
                case, tool, result, root, BUILD_TARGET_SECONDS)
        return measure_run_case(case, tool, result, root, target)
    except OSError as e:
        result.status = STATUS_ERROR
        result.detail = f"a step could not be carried out ({e})"
        return result


def fmt_rate(rate: float) -> str:
    if rate <= 0.0:
        return "-"
    if rate < 10.0:
        return f"{rate:.3g}"
    return f"{round(rate):,}"


def fmt_cell(result: Result | None, measured: str) -> str:
    """What a table says of one tool's measurement of one case."""
    if result is None:
        return CELL_NOT_RUN
    if result.status != STATUS_OK:
        return CELL_FAILED
    return measured


def fmt_growth(exponent: float) -> str:
    if exponent <= 0.0:
        return "-"
    return f"n^{exponent:.1f}"


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


def print_report(
    cases: list[Case], tools_run: list[str], results: list[Result],
    target: float,
) -> None:
    """Print every number obtained, and name each measurement that has none.

    The table has a row for every case and a column for each tool whatever was
    reached, so a run that was cut short still says which cases it never got
    to.
    """
    by_case: dict[str, dict[str, Result]] = {}
    for r in results:
        by_case.setdefault(r.case, {})[r.tool] = r

    failed = [r for r in results if r.status == STATUS_ERROR]
    not_reached = [
        f"{case.name}/{tool}"
        for case in cases
        for tool in tools_run
        if tool not in by_case.get(case.name, {})
    ]
    verilator_ran = VERILATOR in tools_run
    not_run = len(not_reached) + (0 if verilator_ran else len(cases))

    print()
    print("## Lyra Benchmark Report")
    print()
    print(f"> git: `{get_git_sha()}` | target: {target:g}s per measurement")
    print(
        "> Each tool is given the amount of work it needs to reach that "
        "target, so a rate is comparable across tools, machines, and runs "
        "wherever the cost of the work grew as `n^1.0`. Where Lyra's did "
        "not, its rate holds only at the amount beside it.")
    if failed or not_run:
        absences = ((len(failed), CELL_FAILED), (not_run, CELL_NOT_RUN))
        counts = [f"{count} {word}" for count, word in absences if count]
        print(
            f"> **Not every measurement has a number: "
            f"{' and '.join(counts)}, of {len(results) + not_run}.** Each is "
            f"named under \"Not measured\" below. A cell reading "
            f"`{CELL_FAILED}` or `{CELL_NOT_RUN}` is never a rate of zero.")

    families: dict[str, list[Case]] = {}
    for case in cases:
        families.setdefault(case.family, []).append(case)

    for family, members in families.items():
        print()
        print(f"## {family}")
        print()
        print(
            "| Case | Unit | Lyra /s | Verilator /s | vs Verilator "
            "| Lyra work | Lyra growth | Binary (KB) |")
        print(
            "|------|------|--------:|-------------:|:-------------"
            "|----------:|:------------|------------:|")
        for case in members:
            tools = by_case.get(case.name, {})
            lyra = tools.get(LYRA)
            ver = tools.get(VERILATOR)
            lyra_ok = lyra is not None and lyra.status == STATUS_OK
            ver_ok = ver is not None and ver.status == STATUS_OK
            lyra_rate = lyra.rate if lyra_ok else 0.0
            ver_rate = ver.rate if ver_ok else 0.0
            work = f"{lyra.work:,}" if lyra_ok else "-"
            growth = fmt_growth(lyra.growth) if lyra_ok else "-"
            binary = f"{lyra.binary_kb:,}" if lyra_ok else "-"
            print(
                f"| {case.name} | {case.work_unit} "
                f"| {fmt_cell(lyra, fmt_rate(lyra_rate))} "
                f"| {fmt_cell(ver, fmt_rate(ver_rate))} "
                f"| {fmt_comparison(lyra_rate, ver_rate)} "
                f"| {work} | {growth} | {binary} |")

    if failed or not_run:
        print()
        print("### Not measured")
        print()
        for r in failed:
            print(f"- **{r.case}/{r.tool}**: {r.detail}")
            if r.output:
                print()
                print("  ```")
                for line in r.output.splitlines():
                    print(f"  {line}")
                print("  ```")
        if not verilator_ran:
            print(
                f"- **{VERILATOR}**: not on PATH, so no case was run with it")
        for name in not_reached:
            print(f"- **{name}**: the run ended before measuring it")

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
        "readings": [list(reading) for reading in r.readings],
        "growth": r.growth,
        "detail": r.detail,
        "output": r.output,
    }


def write_json(results: list[Result], path: str, target: float) -> None:
    data = {
        "schema_version": 6,
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
    parser.add_argument(
        "--lyra", default=str(REPO_ROOT / "bazel-bin" / "lyra"),
        help="The compiler to measure; this checkout's bazel-bin/lyra if "
             "not given")
    args = parser.parse_args()

    lyra = os.path.abspath(args.lyra)
    if not os.path.isfile(lyra):
        print(f"error: lyra binary not found at {lyra}", file=sys.stderr)
        sys.exit(1)

    try:
        cases = discover_cases(CORPUS_ROOT)
    except ValueError as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)
    if args.filter:
        cases = [c for c in cases if args.filter in c.name]
    if not cases:
        print("error: no cases found", file=sys.stderr)
        sys.exit(1)

    tools = [lyra_tool(lyra)]
    verilator = shutil.which("verilator")
    if verilator:
        tools.append(verilator_tool(verilator))
    else:
        print("note: verilator not on PATH; Lyra only", file=sys.stderr)

    print(f"Running {len(cases)} cases", file=sys.stderr)

    # A step runs in a process group of its own, so a signal sent to this one
    # does not reach it. Being told to stop is made an exit from here, which
    # unwinds through the step in flight and stops it on the way.
    def exit_on(signum: int, _frame: object) -> None:
        sys.exit(128 + signum)

    for stopping in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(stopping, exit_on)

    # Whatever ends the loop, what it obtained is reported: a run stopped part
    # way has still measured the cases before the one it stopped in. The result
    # file is rewritten as each result arrives for the same reason, since a run
    # that is killed outright prints no report at all.
    results: list[Result] = []
    try:
        with tempfile.TemporaryDirectory(prefix="lyra-bench-") as tmpdir:
            for case in cases:
                for tool in tools:
                    print(f"  {case.path}/{tool.name}", end="", flush=True,
                          file=sys.stderr)
                    r = measure(case, tool, tmpdir, args.seconds)
                    results.append(r)
                    if r.status == STATUS_OK:
                        print(
                            f" -> {r.status} "
                            f"({fmt_rate(r.rate)} {r.work_unit}/s)",
                            file=sys.stderr)
                    else:
                        print(f" -> {r.status}: {r.detail}", file=sys.stderr)
                    if args.json:
                        write_json(results, args.json, args.seconds)
    finally:
        print_report(cases, [t.name for t in tools], results, args.seconds)

    if args.json:
        print(f"JSON written to {args.json}", file=sys.stderr)

    if any(r.status == STATUS_ERROR for r in results):
        sys.exit(1)


if __name__ == "__main__":
    main()
