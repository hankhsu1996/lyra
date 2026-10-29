#!/usr/bin/env python3
"""Lyra benchmark runner.

Runs every case under tests/benchmark/ against Lyra, on each backend, and a
reference simulator, choosing how much work each one does so that every
measurement lands in the same duration, and reports the rate each sustained.

With --count it measures something else: how many instructions one unit of each
case's work costs Lyra, counted under callgrind. A count does not move with the
machine's load, so two compilers counted on one machine are told apart however
small the difference; it can move from one machine to another, so it is compared
only with a count taken beside it, and a rate is what compares Lyra with the
reference simulator.

With --count and --design it counts real designs instead of the corpus: each a
directory `declare_external_design` wrote a case of Verilator's benchmark suite
into, counted whole -- the instructions Lyra executes building it, and the
instructions its program executes running the suite's quick check. The corpus
cases are each one cost family, which says which path a change moved; a real
design is what says whether it moved for someone who builds one.

A case that cannot be measured costs only itself: the run goes on, the report
carries every number that was obtained, and the case is named there with the
step that failed and how it ended. The exit status is 0 when every case was
measured and 1 when any was not, or when there was nothing to run.

Usage:
    python3 tools/bench/run_benchmarks.py [--json PATH] [--filter SUBSTRING]
                                          [--seconds N] [--lyra PATH]
                                          [--backend llvm|cpp ...]
                                          [--count [--amounts PATH] [-j N]
                                                   [--design DIR ...]]
                                          [--revision TEXT]
"""

import argparse
import concurrent.futures
import contextlib
import json
import math
import os
import re
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

# What a run is counted at is chosen the same way, by timing natively, but far
# shorter: a count is exact at any amount, so the amount only has to be large
# enough that the work, rather than what a run does once, is what was counted,
# and every instruction counted costs about fifty times its native time.
COUNT_RUN_TARGET_SECONDS = 0.02

# A build is counted at one design size and twice it, the same for every case
# and backend, whatever a duration would choose. A search for a duration
# follows a cost that barely grows out to designs that take minutes to build
# under callgrind, and stops sooner on one backend than the other, so the two
# would not count the same design; how a build's cost grows with its size is
# the growth tests' subject. What a build does before any of the design is
# hundreds of millions of instructions and moves between runs, so whether the
# units between the two sizes add enough to be told from that is measured for
# each case, as its resolution.
COUNT_BUILD_AMOUNT = 64

# The least change of a count worth marking, in percent. A count can say this
# only where two counts of one compiler disagree by well under it, so a corpus
# case is counted twice at its lower amount and the disagreement is reported as
# the case's resolution; a case whose resolution is past a third of this cannot
# say it, and fails, since a number it reports would be noise however it moved.
MARK_PERCENT = 2.0
RESOLVABLE_PERCENT = MARK_PERCENT / 3

TIMEOUT_SECONDS = 900

# What a step of a real design may take under callgrind, which runs a program
# about fifty times slower than it runs natively: a design that builds in tens
# of seconds is counted in tens of minutes.
DESIGN_TIMEOUT_SECONDS = 2400

# What preparing a run's store may take: the C++ backend compiles the runtime
# library a program links, optimized, into the first store it builds in, and on
# a small machine that is longer than any one case's build is given.
PREPARE_TIMEOUT_SECONDS = 3600

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

# Lyra builds a program on either backend, and each is measured as a tool of
# its own, since one may regress while the other does not.
BACKENDS = ("llvm", "cpp")

# What a result file's numbers are counts of.
METRIC_TIME = "time"
METRIC_INSTRUCTIONS = "instructions"

# The version of the result file's shape, raised whenever a field is removed or
# changes type.
SCHEMA_VERSION = 8

MEASURE_BUILD = "build"
MEASURE_RUN = "run"

# The family a real design's rows are reported under. A design is what a user
# builds, where every other family is one cost, there to say which path a
# change moved.
DESIGN_FAMILY = "designs"

# What `declare_external_design` writes beside a design's declaration.
DESIGN_DESCRIPTION = "case.json"

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
    backend: str = ""
    status: str = STATUS_OK
    work: int = 0
    seconds: float = 0.0
    rate: float = 0.0
    instructions_per_unit: float = 0.0
    # How far two counts of the same work disagreed, as a percent of the work
    # counted; None where the count was taken once.
    resolution_percent: float | None = None
    build_s: float = 0.0
    binary_kb: int = 0
    readings: tuple[tuple[int, float], ...] = ()
    growth: float = 0.0
    detail: str = ""
    output: str = ""


def measured_name(tool: str, backend: str) -> str:
    """The tool and backend that make a measurement, as one name."""
    return f"{tool}-{backend}" if backend else tool


def result_name(r: "Result") -> str:
    """One measurement, as a report names it: a real design has two."""
    return f"{r.case}/{r.measure}/{measured_name(r.tool, r.backend)}"


@dataclass
class ProcessRun:
    """One subprocess and what running it cost."""
    elapsed_s: float
    returncode: int
    stdout: str
    stderr: str
    timed_out: bool
    timeout_s: int


# Every process group a step has started and not yet seen end, so that a run
# told to stop can stop all of them: with cases counted side by side, more than
# one step is in flight, and an exit unwinds through none of them.
_live_groups: set[int] = set()
# Reentrant, because the handler for being told to stop takes it, and the
# signal can land while the thread it interrupts already holds it.
_live_lock = threading.RLock()
_stopping = threading.Event()


def kill_group(pgid: int) -> None:
    try:
        os.killpg(pgid, signal.SIGKILL)
    except ProcessLookupError:
        pass


def stop_every_step() -> None:
    """Stop every step in flight, and let no further one start."""
    with _live_lock:
        _stopping.set()
        for pgid in _live_groups:
            kill_group(pgid)


def run_process(
    cmd: list[str], cwd: str | None = None, env: dict | None = None,
    timeout_s: int = TIMEOUT_SECONDS, output_path: Path | None = None,
) -> ProcessRun:
    """Run one command to completion and time it.

    Where `output_path` is given, everything the command prints goes there,
    both streams in the order it printed them, and is kept in the result as
    what it printed to standard output.
    """
    # What a step printed is only ever shown to a reader, and a program that
    # faults may have printed bytes that are no text at all.
    with contextlib.ExitStack() as files:
        out = files.enter_context(
            open(output_path, "w+", errors="backslashreplace")
            if output_path is not None else
            tempfile.TemporaryFile(mode="w+", errors="backslashreplace"))
        err = out if output_path is not None else files.enter_context(
            tempfile.TemporaryFile(mode="w+", errors="backslashreplace"))
        start = time.monotonic()
        # The command leads a process group of its own, so that stopping it
        # stops the host compiler it started too. One left running would be
        # taking the machine from the measurement after it.
        with _live_lock:
            if _stopping.is_set():
                raise OSError("the run is stopping")
            proc = subprocess.Popen(
                cmd, stdout=out, stderr=err, cwd=cwd, env=env, text=True,
                start_new_session=True)
            _live_groups.add(proc.pid)

        killed: list[bool] = []

        def on_timeout() -> None:
            killed.append(True)
            kill_group(proc.pid)

        watchdog = threading.Timer(timeout_s, on_timeout)
        watchdog.start()
        try:
            proc.wait()
            elapsed = time.monotonic() - start
        finally:
            watchdog.cancel()
            if proc.poll() is None:
                kill_group(proc.pid)
                proc.wait()
            with _live_lock:
                _live_groups.discard(proc.pid)

        out.seek(0)
        stdout = out.read()
        err.seek(0)
        return ProcessRun(
            elapsed_s=elapsed,
            returncode=proc.returncode,
            stdout=stdout,
            stderr="" if err is out else err.read(),
            timed_out=bool(killed),
            timeout_s=timeout_s,
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
    lyra: str, backend: str, case: Case, out_dir: str, amount: int | None,
    run_store: str,
) -> list[str]:
    # A build draws no progress: drawing follows the clock, which moves a count
    # of the same build from one run to the next. What it keeps goes in a store
    # of this run's, so a measurement leaves nothing in the user's.
    cmd = [lyra, "build", "--progress=none", "--backend", backend]
    # A simulation is timed on the code a user would ship, so its translation
    # unit is optimized together with the runtime. Its build is not timed, so
    # every such build shares one store, and the runtime's own sources, which
    # the C++ backend then compiles, are compiled once for the run. A build is
    # timed the way an edit loop builds, since that is the cost it exists to
    # protect and optimizing it would measure the host compiler's optimizer
    # instead; and every timed build is a build, since a program kept from an
    # earlier probe of the same amount would be copied rather than compiled.
    if case.measure == MEASURE_RUN:
        cmd.extend(["-O2", "--lto", "--cache-dir", run_store])
    else:
        cmd.extend(
            ["--rebuild", "--cache-dir", str(Path(out_dir) / "store")])
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
    backend: str = ""


def build_program(
    case: Case, tool: Tool, result: Result, root: str,
) -> Path | None:
    """Build a simulation case's program once, or record why there is none."""
    work = os.path.join(root, "build")
    os.makedirs(work, exist_ok=True)
    build = run_process(tool.build(case, work, None), cwd=work, env=tool.env)
    result.build_s = build.elapsed_s
    if record_failure(result, "build", build):
        return None
    binary = tool.binary(work)
    if not binary.is_file():
        result.status = STATUS_ERROR
        result.detail = f"build produced no {binary.name}"
        return None
    result.binary_kb = round(binary.stat().st_size / 1024)
    return binary


def prove_built(tool: Tool, result: Result, work: str, amount: int) -> bool:
    """Run once what a build case built, or record why it did not run.

    The artifact is run, not measured: a build case earns its number from the
    build, and running it is what says the build produced something real.
    """
    binary = tool.binary(work)
    if not binary.is_file():
        result.status = STATUS_ERROR
        result.detail = (
            f"build at {WORK_PARAM}={amount} produced no {binary.name}")
        return False
    result.binary_kb = round(binary.stat().st_size / 1024)
    proof = run_process([str(binary), "+work=1"])
    return not record_failure(
        result, f"run of the program built at {WORK_PARAM}={amount}", proof)


def measure_run_case(
    case: Case, tool: Tool, result: Result, root: str, target: float,
) -> Result:
    """Build once, then raise the work count until a run reaches the target."""
    binary = build_program(case, tool, result, root)
    if binary is None:
        return result

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
    if not prove_built(tool, result, builds[-1], amount):
        return result

    record_measurement(result, amount, build, history, target)
    return result


def count_instructions(
    cmd: list[str], cwd: str, env: dict | None = None,
    timeout_s: int = TIMEOUT_SECONDS, output_path: Path | None = None,
) -> tuple[int, ProcessRun]:
    """Run one command under callgrind; return the instructions it executed.

    Only the command's own process is counted. A build starts the host
    compiler, which runs natively and uncounted: it is not Lyra, and what Lyra
    hands it is held by the growth tests, which count what a build leaves.
    What callgrind itself says goes to a file of its own, so what the command
    printed is the command's alone.
    """
    out = Path(cwd) / "callgrind.out"
    run = run_process(
        ["valgrind", "--tool=callgrind", f"--callgrind-out-file={out}",
         f"--log-file={Path(cwd) / 'callgrind.log'}", *cmd],
        cwd=cwd, env=env, timeout_s=timeout_s, output_path=output_path)
    if run.timed_out or run.returncode != 0:
        return 0, run
    match = (
        re.search(r"^summary:\s+(\d+)", out.read_text(), re.MULTILINE)
        if out.is_file() else None)
    if match is None:
        raise OSError(f"callgrind wrote no total to {out}")
    return int(match.group(1)), run


def record_count(
    result: Result, readings: list[tuple[int, int]], repeated: int,
) -> None:
    """Record instructions per unit from counts taken at two amounts.

    Their difference is the work alone: what a run executes once -- the
    process starting, a design being constructed, a compiler reading its
    prelude -- is in both and cancels exactly, as no two timings can.

    `repeated` is a second count at the lower amount. What it disagrees with the
    first by is what a count of this case cannot tell from a change, and it is
    measured against the work counted rather than against either count: what a
    run does once can be nearly all of a count, and its smallest variation is
    then most of the difference. A case whose disagreement is past what a mark
    needs is recorded as failed, with what it measured.
    """
    (low, low_count), (high, high_count) = readings
    result.work = low
    result.readings = tuple(readings)
    work = high_count - low_count
    if work <= 0:
        result.status = STATUS_ERROR
        result.detail = (
            f"its work did not show: {high} {result.work_unit} counted no "
            f"more than {low}")
        return
    result.instructions_per_unit = work / (high - low)
    result.resolution_percent = abs(repeated - low_count) / work * 100.0
    if result.resolution_percent > RESOLVABLE_PERCENT:
        result.status = STATUS_ERROR
        result.detail = (
            f"its work is below what a count resolves: two counts at "
            f"{low} {result.work_unit} differ by "
            f"{result.resolution_percent:.2g}% of the work counted, and a "
            f"change of {MARK_PERCENT:g}% needs under {RESOLVABLE_PERCENT:.2g}%")


def count_run_case(
    case: Case, tool: Tool, result: Result, root: str, amount: int | None,
) -> Result:
    """Build once, then count a run at an amount and at twice it."""
    binary = build_program(case, tool, result, root)
    if binary is None:
        return result

    if amount is None:
        amount, run, _ = converge(
            lambda n: run_process([str(binary), f"+work={n}"]),
            COUNT_RUN_TARGET_SECONDS, case.work_max)
        if record_failure(result, f"run at +work={amount}", run):
            return result
    # The higher count is taken at twice the amount, which the case must still
    # hold.
    amount = max(1, min(amount, case.work_max // 2))

    counts: list[int] = []
    for n in (amount, 2 * amount, amount):
        count, run = count_instructions(
            [str(binary), f"+work={n}"], str(binary.parent))
        if record_failure(result, f"counted run at +work={n}", run):
            return result
        counts.append(count)
    record_count(
        result, [(amount, counts[0]), (2 * amount, counts[1])], counts[2])
    return result


def count_build_case(
    case: Case, tool: Tool, result: Result, root: str,
) -> Result:
    """Count what Lyra executes building the design at a size and twice it."""
    amount = COUNT_BUILD_AMOUNT
    counts: list[int] = []
    for index, n in enumerate((amount, 2 * amount, amount)):
        work = os.path.join(root, f"build-{index}")
        os.makedirs(work, exist_ok=True)
        count, run = count_instructions(
            tool.build(case, work, n), work, tool.env)
        if record_failure(result, f"counted build at {WORK_PARAM}={n}", run):
            return result
        counts.append(count)
    if not prove_built(tool, result, os.path.join(root, "build-1"), 2 * amount):
        return result

    record_count(
        result, [(amount, counts[0]), (2 * amount, counts[1])], counts[2])
    return result


@dataclass(frozen=True)
class Design:
    """A case of Verilator's benchmark suite, as `declare_external_design`
    wrote it: a directory holding its declaration and the directory its run
    starts in."""
    name: str
    declared: Path
    # Where the run starts, relative to the declaration.
    run: str
    options: tuple[str, ...]
    arguments: tuple[str, ...]
    check: Path | None


def read_design(declared: Path) -> Design:
    with open(declared / DESIGN_DESCRIPTION) as f:
        described = json.load(f)
    return Design(
        name=described["name"],
        declared=declared,
        run=described["run"],
        options=tuple(described["options"]),
        arguments=tuple(described["arguments"]),
        check=Path(described["check"]) if described["check"] else None,
    )


def count_design(
    design: Design, lyra: str, backend: str, run_store: str, tmpdir: str,
    unprepared: Callable[[str], str | None],
) -> list[Result]:
    """Count a real design whole: building it, and running its quick check.

    The build is counted as an edit loop builds, as a corpus case's is, and
    the run is counted on the program a long run would ship, optimized together
    with the runtime, as a corpus case's is. Each is one total rather than a
    difference of two: the suite fixes how much its quick check does, and what
    a run does once -- the design being constructed -- is part of what a user
    waits for. A total's disagreement between two counts is the small one a
    build's fixed part has, so a total is counted once. The run is then held to
    the suite's own check, since a count of a run that went wrong is of other
    work.
    """
    results = [
        Result(
            case=design.name, family=DESIGN_FAMILY, measure=measure,
            work_unit=measure, tool=LYRA, backend=backend, work=1)
        for measure in (MEASURE_BUILD, MEASURE_RUN)
    ]
    if (why := unprepared(backend)) is not None:
        for r in results:
            r.status = STATUS_ERROR
            r.detail = why
        return results
    print(f"  {design.name}/{backend} ...", file=sys.stderr, flush=True)
    # A step the operating system would not start is this design's failure
    # like any other.
    try:
        count_design_into(design, lyra, backend, run_store, tmpdir, *results)
    except OSError as e:
        for r in results:
            if r.status == STATUS_OK and r.instructions_per_unit == 0.0:
                r.status = STATUS_ERROR
                r.detail = f"a step could not be carried out ({e})"
    return results


def count_design_into(
    design: Design, lyra: str, backend: str, run_store: str, tmpdir: str,
    built: Result, ran: Result,
) -> None:
    root = Path(tmpdir) / f"{design.name.replace(':', '-')}-{backend}"
    shutil.copytree(design.declared, root, symlinks=True)

    # A design of this size draws warnings by the thousand, and what stops it
    # is an error, so only errors are asked for. One unit is lowered at a time:
    # a count of threads waiting on one another would count the waiting.
    build = [
        lyra, "build", "--backend", backend, "--progress=none", "-Wnone",
        "-j", "1", *design.options]
    count, run = count_instructions(
        [*build, "--rebuild", "--cache-dir", str(root / "store"), "-o",
         str(root / "sim")],
        str(root), timeout_s=DESIGN_TIMEOUT_SECONDS)
    if record_failure(built, "counted build", run):
        ran.status = STATUS_ERROR
        ran.detail = "not run, since the design did not build"
        return
    built.instructions_per_unit = float(count)

    run = run_process(
        [*build, "-O2", "--lto", "--cache-dir", run_store, "-o",
         str(root / BINARY_NAME)],
        cwd=str(root), timeout_s=DESIGN_TIMEOUT_SECONDS)
    if record_failure(ran, "optimized build", run):
        return

    # The suite's check reads what the run printed from where the suite keeps
    # it.
    here = root / design.run
    count, run = count_instructions(
        [str(root / BINARY_NAME), *design.arguments], str(here),
        timeout_s=DESIGN_TIMEOUT_SECONDS,
        output_path=here / "_execute" / "stdout.log")
    if record_failure(ran, "counted run", run):
        return
    if design.check is not None:
        run = run_process([str(design.check)], cwd=str(here))
        if record_failure(ran, "the suite's check of the counted run", run):
            return
    ran.instructions_per_unit = float(count)


def prepare_store(
    lyra: str, backend: str, run_store: str, tmpdir: str,
) -> str | None:
    """Build an empty design into the run's store, or say why it did not build.

    What every simulation case's build shares -- the runtime library the C++
    backend compiles, optimized, the first time a store is built into -- is
    built here once, before any case, under a limit of its own: it is no
    case's work, every case waits on the store while it is built, and on a
    small machine it takes longer than a case's build is given.
    """
    root = Path(tmpdir) / f"prepare-{backend}"
    root.mkdir()
    source = root / CASE_ENTRY
    source.write_text(f"module {CASE_TOP};\nendmodule\n")
    empty = Case(
        family="", name="prepare", measure=MEASURE_RUN, work_unit="",
        work_max=MAX_AMOUNT, sources=(source,))
    run = run_process(
        lyra_build_command(lyra, backend, empty, str(root), None, run_store),
        cwd=str(root), timeout_s=PREPARE_TIMEOUT_SECONDS)
    if run.timed_out or run.returncode != 0:
        # Said once here; every measurement it stops names only the step.
        print(
            f"error: preparing the {backend} store {how_it_ended(run)}\n"
            f"{last_lines(run)}", file=sys.stderr, flush=True)
        return f"preparing the {backend} store {how_it_ended(run)}"
    return None


def how_it_ended(run: ProcessRun) -> str:
    """How a step that did not succeed ended, as the rest of a sentence."""
    if run.timed_out:
        return f"was stopped after {run.timeout_s}s"
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


def lyra_tool(lyra: str, backend: str, run_store: str) -> Tool:
    return Tool(
        name=LYRA,
        build=lambda case, out, amount: lyra_build_command(
            lyra, backend, case, out, amount, run_store),
        binary=lambda out: Path(out) / BINARY_NAME,
        backend=backend,
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


def measure(
    case: Case, tool: Tool, tmpdir: str,
    step: Callable[[Case, Tool, Result, str], Result],
    unprepared: Callable[[str], str | None],
) -> Result:
    """Make one measurement of one case with one tool, by the step given.

    `unprepared` says, once a backend's store is ready, why it could not be
    made ready, or None where it is.
    """
    result = Result(
        case=case.name,
        family=case.family,
        measure=case.measure,
        work_unit=case.work_unit,
        tool=tool.name,
        backend=tool.backend,
    )
    name = measured_name(tool.name, tool.backend)
    root = os.path.join(tmpdir, f"{case.name}-{name}")
    os.makedirs(root, exist_ok=True)
    # A simulation case is built into the run's store, which a build case is
    # not.
    why = unprepared(tool.backend) if case.measure == MEASURE_RUN else None
    if why is not None:
        result.status = STATUS_ERROR
        result.detail = why
        return result
    # Said as it starts, so a run that hangs names the measurement it hung in.
    print(f"  {case.path}/{name} ...", file=sys.stderr, flush=True)

    # A step the operating system would not start -- a program that is not one,
    # a directory that cannot be made -- is this case's failure like any other.
    try:
        return step(case, tool, result, root)
    except OSError as e:
        result.status = STATUS_ERROR
        result.detail = f"a step could not be carried out ({e})"
        return result


def timing_step(target: float) -> Callable[[Case, Tool, Result, str], Result]:
    def step(case: Case, tool: Tool, result: Result, root: str) -> Result:
        if case.measure == MEASURE_BUILD:
            return measure_build_case(
                case, tool, result, root, BUILD_TARGET_SECONDS)
        return measure_run_case(case, tool, result, root, target)
    return step


def counting_step(
    amounts: dict[tuple[str, str], int],
) -> Callable[[Case, Tool, Result, str], Result]:
    def step(case: Case, tool: Tool, result: Result, root: str) -> Result:
        if case.measure == MEASURE_BUILD:
            return count_build_case(case, tool, result, root)
        return count_run_case(
            case, tool, result, root, amounts.get((case.name, tool.backend)))
    return step


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


def print_absences(
    results: list[Result], not_reached: list[str], extra: list[str],
) -> None:
    """Name each measurement that has no number, and say why."""
    failed = [r for r in results if r.status == STATUS_ERROR]
    if not (failed or not_reached or extra):
        return
    print()
    print("### Not measured")
    print()
    for r in failed:
        print(f"- **{result_name(r)}**: {r.detail}")
        if r.output:
            print()
            print("  ```")
            for line in r.output.splitlines():
                print(f"  {line}")
            print("  ```")
    for line in extra:
        print(f"- {line}")
    for name in not_reached:
        print(f"- **{name}**: the run ended before measuring it")


def print_absence_count(
    results: list[Result], not_run: int, number: str,
) -> None:
    """Say under a report's heading how many measurements have no number."""
    failed = sum(1 for r in results if r.status == STATUS_ERROR)
    if not (failed or not_run):
        return
    absences = ((failed, CELL_FAILED), (not_run, CELL_NOT_RUN))
    counts = [f"{count} {word}" for count, word in absences if count]
    print(
        f"> **Not every measurement has a number: "
        f"{' and '.join(counts)}, of {len(results) + not_run}.** Each is "
        f"named under \"Not measured\" below. A cell reading "
        f"`{CELL_FAILED}` or `{CELL_NOT_RUN}` is never a {number} of zero.")


def by_measurement(results: list[Result]) -> dict[tuple[str, str], Result]:
    return {(r.case, measured_name(r.tool, r.backend)): r for r in results}


def unreached_measurements(
    cases: list[Case], tools: list[Tool], results: list[Result],
) -> list[str]:
    reached = by_measurement(results)
    return [
        f"{case.name}/{measured_name(tool.name, tool.backend)}"
        for case in cases
        for tool in tools
        if (case.name, measured_name(tool.name, tool.backend)) not in reached
    ]


def families_of(cases: list[Case]) -> dict[str, list[Case]]:
    families: dict[str, list[Case]] = {}
    for case in cases:
        families.setdefault(case.family, []).append(case)
    return families


def print_report(
    cases: list[Case], tools: list[Tool], backends: list[str],
    results: list[Result], revision: str, target: float,
) -> None:
    """Print every rate obtained, and name each measurement that has none.

    The table has a row for every case on every backend whatever was reached,
    so a run that was cut short still says which cases it never got to. Each
    row sets one backend beside the reference simulator, which is measured once
    per case.
    """
    reached = by_measurement(results)
    not_reached = unreached_measurements(cases, tools, results)
    verilator_ran = any(t.name == VERILATOR for t in tools)
    not_run = len(not_reached) + (0 if verilator_ran else len(cases))

    print()
    print("## Lyra Benchmark Report")
    print()
    print(f"> git: `{revision}` | target: {target:g}s per measurement")
    print(
        "> Each tool is given the amount of work it needs to reach that "
        "target, so a rate is comparable across tools, machines, and runs "
        "wherever the cost of the work grew as `n^1.0`. Where Lyra's did "
        "not, its rate holds only at the amount beside it.")
    print_absence_count(results, not_run, "rate")

    for family, members in families_of(cases).items():
        print()
        print(f"## {family}")
        print()
        print(
            "| Case | Backend | Unit | Lyra /s | Verilator /s | vs Verilator "
            "| Lyra work | Lyra growth | Binary (KB) |")
        print(
            "|------|---------|------|--------:|-------------:|:-------------"
            "|----------:|:------------|------------:|")
        for case in members:
            ver = reached.get((case.name, VERILATOR))
            ver_ok = ver is not None and ver.status == STATUS_OK
            ver_rate = ver.rate if ver_ok else 0.0
            for backend in backends:
                lyra = reached.get((case.name, measured_name(LYRA, backend)))
                lyra_ok = lyra is not None and lyra.status == STATUS_OK
                lyra_rate = lyra.rate if lyra_ok else 0.0
                work = f"{lyra.work:,}" if lyra_ok else "-"
                growth = fmt_growth(lyra.growth) if lyra_ok else "-"
                binary = f"{lyra.binary_kb:,}" if lyra_ok else "-"
                print(
                    f"| {case.name} | {backend} | {case.work_unit} "
                    f"| {fmt_cell(lyra, fmt_rate(lyra_rate))} "
                    f"| {fmt_cell(ver, fmt_rate(ver_rate))} "
                    f"| {fmt_comparison(lyra_rate, ver_rate)} "
                    f"| {work} | {growth} | {binary} |")

    print_absences(
        results, not_reached,
        [] if verilator_ran else
        [f"**{VERILATOR}**: not on PATH, so no case was run with it"])
    print()


def print_count_report(
    cases: list[Case], designs: list[Design], tools: list[Tool],
    backends: list[str], results: list[Result], revision: str,
) -> None:
    """Print instructions per unit of work for every case on every backend,
    and each real design's totals."""
    reached = by_measurement(results)
    not_reached = unreached_measurements(cases, tools, results)

    print()
    print("## Lyra Instruction Count Report")
    print()
    print(
        f"> git: `{revision}` | instructions per unit of work, "
        f"under callgrind")
    print_absence_count(results, len(not_reached), "count")
    if designs:
        print()
        print(f"## {DESIGN_FAMILY}")
        print()
        print("| Design | Backend | Measure | Instructions |")
        print("|--------|---------|---------|-------------:|")
        for r in results:
            if r.family != DESIGN_FAMILY:
                continue
            count = f"{r.instructions_per_unit:,.0f}"
            print(
                f"| {r.case} | {r.backend} | {r.measure} "
                f"| {fmt_cell(r, count)} |")
    for family, members in families_of(cases).items():
        print()
        print(f"## {family}")
        print()
        print("| Case | Backend | Unit | Instructions per unit | Work |")
        print("|------|---------|------|----------------------:|-----:|")
        for case in members:
            for backend in backends:
                lyra = reached.get((case.name, measured_name(LYRA, backend)))
                ok = lyra is not None and lyra.status == STATUS_OK
                count = f"{lyra.instructions_per_unit:,.0f}" if ok else "-"
                work = f"{lyra.work:,}" if ok else "-"
                print(
                    f"| {case.name} | {backend} | {case.work_unit} "
                    f"| {fmt_cell(lyra, count)} | {work} |")

    print_absences(results, not_reached, [])
    print()


def result_to_dict(r: Result) -> dict:
    return {
        "case": r.case,
        "family": r.family,
        "measure": r.measure,
        "work_unit": r.work_unit,
        "tool": r.tool,
        "backend": r.backend,
        "status": r.status,
        "work": r.work,
        "seconds": r.seconds,
        "rate": r.rate,
        "instructions_per_unit": r.instructions_per_unit,
        "resolution_percent": r.resolution_percent,
        "build_s": r.build_s,
        "binary_kb": r.binary_kb,
        "readings": [list(reading) for reading in r.readings],
        "growth": r.growth,
        "detail": r.detail,
        "output": r.output,
    }


def write_json(
    results: list[Result], path: str, metric: str, revision: str,
    target: float,
) -> None:
    data = {
        "schema_version": SCHEMA_VERSION,
        "git": revision,
        "metric": metric,
        "target_seconds": target,
        "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "results": [result_to_dict(r) for r in results],
    }
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
        f.write("\n")


def read_count_file(path: str) -> dict:
    """A count run's result file, or ValueError where the file is not one."""
    with open(path) as f:
        data = json.load(f)
    if (data.get("schema_version") != SCHEMA_VERSION
            or data.get("metric") != METRIC_INSTRUCTIONS):
        raise ValueError(f"{path} is not a count run's result file")
    return data


def read_amounts(path: str) -> dict[tuple[str, str], int]:
    """The amount each case was counted at in an earlier count run.

    Two compilers are compared on the same work only where they did the same
    amount of it: a count is exact, but the amount a run converges on is chosen
    by timing, which does not repeat.
    """
    return {
        (r["case"], r["backend"]): r["work"]
        for r in read_count_file(path)["results"]
        if r["status"] == STATUS_OK
    }


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
    parser.add_argument(
        "--backend", action="append", choices=BACKENDS, default=None,
        help="A backend to measure Lyra on; may be given more than once, and "
             "every backend is measured if it is not given")
    parser.add_argument(
        "--count", action="store_true",
        help="Count instructions per unit of work under callgrind instead of "
             "timing, Lyra only")
    parser.add_argument(
        "--amounts", default=None,
        help="With --count, count each simulation at the amount this earlier "
             "count run's result file records for it")
    parser.add_argument(
        "-j", "--jobs", type=int, default=os.cpu_count() or 1,
        help="With --count, how many cases are counted at once")
    parser.add_argument(
        "--design", action="append", default=None,
        help="With --count, count this directory declare_external_design "
             "wrote instead of the corpus; may be given more than once")
    parser.add_argument(
        "--revision", default=None,
        help="What the measured compiler was built from, as the report and "
             "the result file name it; this checkout's HEAD if not given")
    args = parser.parse_args()

    lyra = os.path.abspath(args.lyra)
    if not os.path.isfile(lyra):
        print(f"error: lyra binary not found at {lyra}", file=sys.stderr)
        sys.exit(1)
    for given, name in ((args.amounts, "--amounts"), (args.design, "--design")):
        if given and not args.count:
            print(f"error: {name} is read only with --count", file=sys.stderr)
            sys.exit(1)
    if args.count and not shutil.which("valgrind"):
        print("error: --count needs valgrind on PATH", file=sys.stderr)
        sys.exit(1)
    backends = args.backend or list(BACKENDS)
    revision = args.revision or get_git_sha()
    try:
        amounts = read_amounts(args.amounts) if args.amounts else {}
    except (OSError, ValueError, KeyError) as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)

    try:
        designs = [read_design(Path(d)) for d in args.design or []]
        cases = [] if designs else discover_cases(CORPUS_ROOT)
    except (OSError, ValueError, KeyError) as e:
        print(f"error: {e}", file=sys.stderr)
        sys.exit(1)
    if args.filter:
        cases = [c for c in cases if args.filter in c.name]
        designs = [d for d in designs if args.filter in d.name]
    if not cases and not designs:
        print("error: no cases found", file=sys.stderr)
        sys.exit(1)

    # Made before the tools that build into it and removed once the run ends,
    # however it ends.
    run_store = tempfile.mkdtemp(prefix="lyra-bench-store-")
    tools = [lyra_tool(lyra, backend, run_store) for backend in backends]
    if args.count:
        metric = METRIC_INSTRUCTIONS
        step = counting_step(amounts)
        target = 0.0
        # A count is of one process and does not depend on what else the
        # machine is running, so cases are counted side by side. The timing
        # that picks a simulation's amount runs beside them too: the amount
        # need only be roughly right, and a compiler compared is given the
        # same one. A timing run is of the machine, and takes it alone.
        jobs = max(1, args.jobs)
    else:
        metric = METRIC_TIME
        step = timing_step(args.seconds)
        target = args.seconds
        jobs = 1
        verilator = shutil.which("verilator")
        if verilator:
            tools.append(verilator_tool(verilator))
        else:
            print("note: verilator not on PATH; Lyra only", file=sys.stderr)

    print(f"Running {len(cases) + len(designs)} cases", file=sys.stderr)

    # A step runs in a process group of its own, so a signal sent to this one
    # does not reach it. Being told to stop stops every step in flight, and is
    # then made an exit from here.
    def exit_on(signum: int, _frame: object) -> None:
        stop_every_step()
        sys.exit(128 + signum)

    for stopping in (signal.SIGTERM, signal.SIGHUP):
        signal.signal(stopping, exit_on)

    # Whatever ends the loop, what it obtained is reported: a run stopped part
    # way has still measured the cases before the one it stopped in. The result
    # file is rewritten as each result arrives for the same reason, since a run
    # that is killed outright prints no report at all.
    results: list[Result] = []

    def done(r: Result) -> None:
        results.append(r)
        name = f"{r.family}/{result_name(r)}"
        if r.status != STATUS_OK:
            print(f"  {name} -> {r.status}: {r.detail}", file=sys.stderr)
        elif metric == METRIC_INSTRUCTIONS:
            print(
                f"  {name} -> {r.status} "
                f"({r.instructions_per_unit:,.0f} instructions/{r.work_unit})",
                file=sys.stderr)
        else:
            print(
                f"  {name} -> {r.status} ({fmt_rate(r.rate)} {r.work_unit}/s)",
                file=sys.stderr)
        if args.json:
            write_json(results, args.json, metric, revision, target)

    try:
        with tempfile.TemporaryDirectory(prefix="lyra-bench-") as tmpdir:
            pool = concurrent.futures.ThreadPoolExecutor(max_workers=jobs)
            try:
                # Each backend's store is prepared first in the queue, so a
                # timing run, one step at a time, has every store ready before
                # it times anything, and a count run counts what needs no store
                # beside the preparing. A measurement on a backend waits for
                # that backend's store and is failed by it alone.
                prepared = {
                    backend: pool.submit(
                        prepare_store, lyra, backend, run_store, tmpdir)
                    for backend in backends
                }

                def unprepared(backend: str) -> str | None:
                    return (
                        prepared[backend].result() if backend in prepared
                        else None)

                pending = [
                    *(pool.submit(
                        lambda c=case, t=tool: [
                            measure(c, t, tmpdir, step, unprepared)])
                      for case in cases for tool in tools),
                    *(pool.submit(
                        count_design, design, lyra, backend, run_store, tmpdir,
                        unprepared)
                      for design in designs for backend in backends),
                ]
                for future in concurrent.futures.as_completed(pending):
                    for r in future.result():
                        done(r)
            finally:
                # Reached early only when the run is stopping: what has not
                # started never does, and what is in flight is stopped rather
                # than waited for.
                pool.shutdown(wait=False, cancel_futures=True)
                stop_every_step()
                pool.shutdown(wait=True)
    finally:
        shutil.rmtree(run_store, ignore_errors=True)
        results.sort(
            key=lambda r: (r.family, r.case, r.tool, r.backend, r.measure))
        if metric == METRIC_INSTRUCTIONS:
            print_count_report(
                cases, designs, tools, backends, results, revision)
        else:
            print_report(cases, tools, backends, results, revision, target)

    if args.json:
        print(f"JSON written to {args.json}", file=sys.stderr)

    if any(r.status == STATUS_ERROR for r in results):
        sys.exit(1)


if __name__ == "__main__":
    main()
