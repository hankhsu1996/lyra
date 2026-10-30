#!/usr/bin/env python3
"""Two operands of one evaluation never both build IR.

What Lyra writes for a design is a function of the design and of Lyra's own
source, whichever C++ compiler built Lyra. Every intermediate form is built by
calls that append to an arena or a builder and answer what they appended, so
the order of those calls is the order of the nodes, their ids and the
instructions. C++ leaves the order of a call's arguments, and of most
operators' operands, to the compiler ([expr.call], [intro.execution]), and
clang and GCC choose opposite ones; written as two arguments of one call, two
appends land in the order the host compiler chose. A braced initializer runs
its clauses first to last ([dcl.init.list]), and so does a statement after the
one before it, which are the two shapes that state an order.

No compiler warning sees this, since each call is well formed and modifies
nothing the expression names twice. So the property is read off the syntax
tree, with clang-query: an evaluation whose order C++ leaves open -- a call's
arguments, a constructor's in parentheses, an operator's operands -- may not
have two operands that each reach a call which builds. A call builds when it
is a method of a type that appends to an IR held elsewhere, or a method not
const of a type that holds one (both named below); when it is handed the first
kind, or a reference not const to the second; or when it calls a lambda, whose
body cannot be read at the call. An accessor -- no parameters, answering a
reference -- builds nothing. The reading is deliberately conservative: an operand it cannot clear
is bound to a local in a statement of its own, which costs a line and states
the order.

Run with no paths to read every source, or with the paths to read. The
compilation database at the repository root is what clang-query parses with.
`--self-test` holds the query to `sequencing_cases.cpp`, whose cases under a
`Reported:` comment it has to report and whose cases under an `In order:` one
it must not.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
CASES = Path(__file__).resolve().with_name("sequencing_cases.cpp")

# The types that hold an IR's nodes: Lyra's arenas and blocks, and an LLVM
# block. What one of them holds changes only through a method that is not const,
# or through a reference to it that is not const, so a const one only reads.
HOLDS = "Block|Arena|Registry|Interner|BasicBlock|CallableCode"

# The types that append to an IR held elsewhere: each layer's lowerer, emitter
# and writer, the walk state a lowering reaches its current block through, and
# LLVM's builder. What they append to is held by reference, so a method of one
# appends whether or not it is const to the object itself.
APPENDS = (
    "IRBuilderBase|BlockRebuild|BlockBuilder|ClosureBuilder|"
    "SynthesizedFunction|RuntimeRecordBuilder|DeclBuilder|[A-Za-z]*Lowerer|"
    "CodeGen[A-Za-z]*|IntegralEmitter|PlaneArithmetic|WalkFrame|"
    "[A-Za-z]*Writer|Renderer"
)

# Operators C++ sequences: the logical ones and the comma, assignment and
# compound assignment (the right operand first), shifts (the left first), and
# subscript.
SEQUENCED = (
    '"&&", "||", ",", "=", "<<", ">>", "+=", "-=", "*=", "/=", "%=", "&=", '
    '"|=", "^=", "<<=", ">>="'
)

# Arguments are compared in pairs; no call that builds takes more than this.
MOST_ARGUMENTS = 8


def query(within: str) -> str:
    pairs = ", ".join(
        f"allOf(hasArgument({a}, operand), hasArgument({b}, operand))"
        for b in range(1, MOST_ARGUMENTS)
        for a in range(b)
    )
    return "\n".join(
        [
            "set output diag",
            "set bind-root true",
            f'let holds cxxRecordDecl(matchesName("::({HOLDS})$"))',
            f'let appends cxxRecordDecl(matchesName("::({APPENDS})$"))',
            "let named qualType(hasDeclaration(appends))",
            "let mutableHolds qualType(hasDeclaration(holds), "
            "unless(isConstQualified()))",
            "let reaches qualType(anyOf(named, references(named), "
            "pointsTo(named), references(mutableHolds), pointsTo(mutableHolds)))",
            "let accessor cxxMethodDecl(parameterCountIs(0), "
            "returns(referenceType()))",
            "let effect expr(anyOf("
            "cxxMemberCallExpr(callee(cxxMethodDecl(ofClass(appends), "
            "unless(accessor)))), "
            "cxxMemberCallExpr(callee(cxxMethodDecl(ofClass(holds), "
            "unless(isConst()), unless(accessor)))), "
            # The standard library's casts and forwarding take what they are
            # handed by reference and append to nothing.
            "callExpr(callee(functionDecl(unless(isInStdNamespace()), "
            "hasAnyParameter(hasType(reaches))))), "
            "callExpr(callee(cxxMethodDecl(ofClass(cxxRecordDecl(isLambda())))))))",
            # A lambda handed over -- itself, the callable a parameter's type
            # is built from, or a set of them built to be handed over -- does
            # not run where it is written.
            "let lambdaOnly ignoringImplicit(anyOf(lambdaExpr(), "
            "cxxConstructExpr(hasArgument(0, ignoringImplicit(lambdaExpr())))))",
            "let handed ignoringImplicit(ignoringParenCasts(anyOf(lambdaOnly, "
            "initListExpr(unless(has(expr(unless(lambdaOnly))))), "
            "cxxConstructExpr(isListInitialization(), "
            "unless(hasAnyArgument(unless(lambdaOnly)))))))",
            "let operand expr(unless(handed), "
            "anyOf(ignoringParenImpCasts(effect), hasDescendant(effect)))",
            f"let pairs anyOf({pairs})",
            f'match expr(isExpansionInFileMatching("{within}"), anyOf('
            "callExpr(unless(cxxOperatorCallExpr()), pairs), "
            f"cxxOperatorCallExpr(unless(hasAnyOverloadedOperatorName({SEQUENCED}, "
            '"[]")), pairs), '
            "cxxConstructExpr(unless(isListInitialization()), pairs), "
            f"binaryOperator(unless(hasAnyOperatorName({SEQUENCED})), "
            "hasLHS(operand), hasRHS(operand))))",
            "",
        ]
    )


def clang_query() -> str:
    version = os.environ.get("LLVM_VERSION", "20")
    for name in (f"clang-query-{version}", "clang-query"):
        found = shutil.which(name)
        if found:
            return found
    sys.exit(f"check_sequencing: no clang-query-{version} or clang-query on PATH")


REPORTED = re.compile(r"^(.+?):(\d+):\d+: note: \"root\" binds here")

# clang-query still answers, and exits 0, for a source it could not compile --
# a header missing stops the parse early -- and what it answers then is that
# nothing matched. So any error is a source not read, never a source in order.
UNREAD = re.compile(r": (fatal )?error: ")


def run(
    paths: list[Path], within: str, extra: list[str], jobs: int
) -> set[tuple[str, int]]:
    tool = clang_query()
    with tempfile.NamedTemporaryFile("w", suffix=".query", delete=False) as f:
        f.write(query(within))
        query_file = f.name

    def one(path: Path) -> tuple[Path, subprocess.CompletedProcess[str]]:
        command = [tool, "-p", str(ROOT), "-f", query_file, str(path)] + extra
        return path, subprocess.run(command, capture_output=True, text=True)

    found: set[tuple[str, int]] = set()
    failed: list[str] = []
    try:
        with ThreadPoolExecutor(max_workers=jobs) as pool:
            for path, result in pool.map(one, paths):
                said = result.stdout + result.stderr
                if result.returncode != 0 or UNREAD.search(said):
                    errors = [line for line in said.splitlines() if UNREAD.search(line)]
                    failed.append(f"{path}:\n" + "\n".join(errors[:5] or [result.stderr]))
                    continue
                for line in result.stdout.splitlines():
                    reported = REPORTED.match(line)
                    if reported:
                        name = os.path.relpath(reported.group(1), ROOT)
                        found.add((name, int(reported.group(2))))
    finally:
        os.unlink(query_file)
    if failed:
        sys.exit("check_sequencing: clang-query could not read\n" + "\n".join(failed))
    return found


def self_test() -> int:
    # A case is the code under its comment, however the formatter wraps it, so
    # a report belongs to the nearest case comment above the line it names.
    cases: dict[int, bool] = {}
    for number, line in enumerate(CASES.read_text().splitlines(), start=1):
        stripped = line.strip()
        if stripped.startswith("// Reported:"):
            cases[number] = True
        elif stripped.startswith("// In order:"):
            cases[number] = False

    def case_of(line: int) -> int | None:
        above = [number for number in cases if number < line]
        return max(above) if above else None

    found = run([CASES], "tools/policy/", ["--", "-std=c++23"], jobs=1)
    reported = {case_of(line) for _, line in found}
    expected = {number for number, report in cases.items() if report}
    name = os.path.relpath(CASES, ROOT)
    for number in sorted(expected - reported):
        print(f"{name}:{number}: the query does not report this case")
    for number in sorted(reported - expected, key=lambda n: n or 0):
        print(f"{name}:{number}: the query reports a case that is in order")
    if reported != expected:
        return 1
    print(
        f"check_sequencing: the query reports all {len(expected)} cases and "
        f"none of the {len(cases) - len(expected)} in order"
    )
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("paths", nargs="*", type=Path)
    parser.add_argument("--self-test", action="store_true")
    # Each clang-query holds a whole translation unit's syntax tree, about a
    # gigabyte, so how many run at once is the memory the check takes, and a
    # machine the build shares is not told how much is free by its core count.
    parser.add_argument("--jobs", type=int, default=2)
    args = parser.parse_args()
    if args.self_test:
        return self_test()

    paths = args.paths or sorted((ROOT / "src" / "lyra").rglob("*.cpp"))
    paths = [p for p in paths if p.suffix == ".cpp" and p.exists()]
    if not paths:
        print("check_sequencing: no source to read")
        return 0
    if not (ROOT / "compile_commands.json").exists():
        sys.exit("check_sequencing: no compile_commands.json at the repository root")

    found = run(paths, "src/lyra/", [], args.jobs)
    for name, line in sorted(found):
        print(
            f"{name}:{line}: two operands of one evaluation each build IR, in "
            "an order C++ leaves to the compiler; bind each to a local first"
        )
    if found:
        print(f"check_sequencing: {len(found)} evaluations in no stated order")
        return 1
    print(f"check_sequencing: {len(paths)} sources read, every build in order")
    return 0


if __name__ == "__main__":
    sys.exit(main())
