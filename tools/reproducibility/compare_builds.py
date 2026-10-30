#!/usr/bin/env python3
"""Two builds of Lyra write the same thing for every case of the corpus.

What Lyra writes for a design is a function of the design and of Lyra's own
source. Two builds of one revision -- in practice one compiled by clang and one
by GCC, which order a call's arguments oppositely -- are handed every case of
the conformance corpus, and each form either writes is compared byte for byte:
every intermediate form's dump, the emitted C++ project, and the program the
execution backend links. The first form that differs is the earliest stage
where the two builds parted, so that is the one reported, with its first
differing line.

`tools/policy/check_sequencing.py` reads the source for the one way this is
known to break. This is the measurement behind it, and it sees whatever that
reading does not model, at the price of seeing only what the corpus reaches.

    compare_builds.py <lyra> <other-lyra> [--cases DIR] [--jobs N]

Each Lyra finds its runtime by its runfiles; a binary copied out of `bazel-bin`
is pointed at them through `RUNFILES_DIR`, which `--runfiles` and
`--other-runfiles` set for each.
"""

from __future__ import annotations

import argparse
import difflib
import filecmp
import os
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

# In pipeline order, so the first that differs is where the builds parted.
DUMPS = ("hir", "mir", "lir", "llvm")


class Interrupted(Exception):
    """A run ended by a signal -- a memory ceiling, a timeout -- wrote what it
    had reached, which says nothing about what Lyra writes."""


@dataclass(frozen=True)
class Lyra:
    binary: Path
    runfiles: Path | None

    # Bytes, since a dump carries what the design wrote, which need be no text.
    def run(self, arguments: list[str], cwd: Path) -> subprocess.CompletedProcess[bytes]:
        env = dict(os.environ)
        if self.runfiles is not None:
            env["RUNFILES_DIR"] = str(self.runfiles)
        result = subprocess.run(
            [str(self.binary), *arguments],
            cwd=cwd,
            env=env,
            capture_output=True,
        )
        if result.returncode < 0:
            raise Interrupted(f"{arguments[0]} ended by signal {-result.returncode}")
        return result


def first_difference(a: bytes, b: bytes) -> str:
    lines = difflib.unified_diff(
        a.decode(errors="replace").splitlines(),
        b.decode(errors="replace").splitlines(),
        lineterm="",
        n=0,
    )
    # The first two lines name the two sides; what follows is the content.
    for line in list(lines)[2:]:
        if line.startswith(("-", "+")):
            return line
    return "(they differ in their final newline)"


def project_difference(a: Path, b: Path) -> str | None:
    comparison = filecmp.dircmp(a, b)
    pending = [(comparison, Path())]
    while pending:
        current, under = pending.pop()
        if current.left_only or current.right_only:
            names = sorted(current.left_only + current.right_only)
            return f"{under / names[0]} is written by one build only"
        _, mismatched, unreadable = filecmp.cmpfiles(
            current.left, current.right, current.common_files, shallow=False
        )
        if unreadable:
            return f"{under / sorted(unreadable)[0]} could not be read"
        for name in sorted(mismatched):
            left = (Path(current.left) / name).read_bytes()
            right = (Path(current.right) / name).read_bytes()
            return f"{under / name}: {first_difference(left, right)}"
        for name, sub in sorted(current.subdirs.items()):
            pending.append((sub, under / name))
    return None


def directives(entry: Path) -> dict[str, str]:
    """The `// @key: value` lines at the top of a case's entry source."""
    found: dict[str, str] = {}
    for line in entry.read_text(errors="replace").splitlines():
        if not line.startswith("// @"):
            break
        key, _, value = line[len("// @") :].partition(":")
        found[key.strip()] = value.strip()
    return found


def design_arguments(case: Path) -> tuple[list[str], list[str]]:
    """How the conformance runner names a case's design to Lyra, and the
    foreign sources a build links with it (conformance/adapters/lyra)."""
    entry = case / "main.sv"
    said = directives(entry)
    named: list[str] = []
    for top in said.get("top", "Top").split():
        named += ["--top", top]
    if said.get("compilation-units") == "single":
        named.append("--single-unit")
    if (case / "lib.map").exists():
        named += ["--libmap", str(case / "lib.map")]
    harness = ROOT / "conformance" / "harness"
    named += [str(harness / name) for name in said.get("includes", "").split()]
    named += sorted(
        str(p) for p in case.iterdir() if p.suffix == ".sv" and p.name != "main.sv"
    )
    named.append(str(entry))
    linked: list[str] = []
    for native in sorted(p for p in case.iterdir() if p.suffix in (".c", ".cpp")):
        linked += ["--dpi-link", str(native)]
    return named, linked


def compare_case(case: Path, lyras: tuple[Lyra, Lyra], scratch: Path) -> str | None:
    named, linked = design_arguments(case)

    for form in DUMPS:
        a, b = (lyra.run(["dump", form, *named], case) for lyra in lyras)
        if (a.returncode, a.stdout, a.stderr) != (b.returncode, b.stdout, b.stderr):
            text_a, text_b = a.stdout + a.stderr, b.stdout + b.stderr
            return f"dump {form}: {first_difference(text_a, text_b)}"

    projects = [scratch / "cpp-a", scratch / "cpp-b"]
    emitted = [
        lyra.run(["emit", "cpp", "-o", str(project), *named], case).returncode
        for lyra, project in zip(lyras, projects)
    ]
    if emitted[0] != emitted[1]:
        return "emit cpp: one build wrote a project and the other did not"
    if emitted[0] == 0:
        difference = project_difference(*projects)
        if difference is not None:
            return f"emit cpp: {difference}"

    programs = [scratch / "program-a", scratch / "program-b"]
    built = [
        lyra.run(
            ["build", "--backend", "llvm", "-o", str(program),
             "--cache-dir", str(program.with_suffix(".cache")), *linked, *named],
            case,
        ).returncode
        for lyra, program in zip(lyras, programs)
    ]
    if built[0] != built[1]:
        return "build: one build linked a program and the other did not"
    if built[0] == 0 and not filecmp.cmp(*programs, shallow=False):
        return "build: the linked programs differ"
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("lyra", type=Path)
    parser.add_argument("other", type=Path)
    parser.add_argument("--runfiles", type=Path)
    parser.add_argument("--other-runfiles", type=Path)
    parser.add_argument("--cases", type=Path, default=ROOT / "conformance" / "cases")
    # Each case runs two Lyras that each build a program, so how many run at
    # once is the memory the comparison takes; a shared machine is not told
    # how much is free by its core count.
    parser.add_argument("--jobs", type=int, default=2)
    args = parser.parse_args()

    lyras = (
        Lyra(args.lyra.resolve(), args.runfiles and args.runfiles.resolve()),
        Lyra(args.other.resolve(), args.other_runfiles and args.other_runfiles.resolve()),
    )
    cases = sorted(main_sv.parent for main_sv in args.cases.rglob("main.sv"))

    with tempfile.TemporaryDirectory() as scratch_root:

        def one(index_case: tuple[int, Path]) -> tuple[Path, str | None, str | None]:
            index, case = index_case
            scratch = Path(scratch_root) / str(index)
            scratch.mkdir()
            try:
                return case, compare_case(case, lyras, scratch), None
            except Interrupted as interrupted:
                return case, None, str(interrupted)

        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            results = list(pool.map(one, enumerate(cases)))

    differing = [(case, why) for case, why, _ in results if why is not None]
    interrupted = [(case, how) for case, _, how in results if how is not None]
    for case, why in differing:
        print(f"{case.relative_to(args.cases)}: {why}")
    for case, how in interrupted:
        print(f"{case.relative_to(args.cases)}: not compared, {how}")
    print(
        f"compare_builds: {len(differing)} of {len(cases)} cases written "
        f"differently by the two builds, {len(interrupted)} not compared"
    )
    return 1 if differing or interrupted else 0


if __name__ == "__main__":
    sys.exit(main())
