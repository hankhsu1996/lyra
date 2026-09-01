#!/usr/bin/env python3
"""Runtime ABI policy.

An entry the generated module calls exists in three places: a prototype in the
ABI header, a definition beside the runtime it wraps, and a binding that gives
the execution session its address. The host compiler holds a definition to its
prototype and nothing else -- so an entry can be defined and unreachable, or
bound under a name nothing defines, and either way the failure lands at run
time as an unresolved symbol rather than as a build error. These rules are the
sides the compiler does not hold.

Rules:

  R001  The entries the ABI header declares and the entries the runtime
        defines are the same set. An entry on one side only is either a
        definition nothing can reach or a prototype for code that does not
        exist.

  R002  Every declared entry is bound into the execution session. An unbound
        entry still resolves while the module runs inside this process,
        because the session falls back to the host process's own exported
        symbols, so the omission stays invisible until a module runs
        somewhere else.

  R003  Every binding names a declared entry, binds it to the function of that
        same name, and binds it once. A name bound to another entry's address
        resolves and runs the wrong code, and a name bound twice keeps
        whichever binding ran last without saying so.

Usage:
  python3 tools/policy/check_runtime_abi.py
"""

import re
import sys
from pathlib import Path
from typing import NamedTuple

HEADER = "include/lyra/runtime/jit_execution.hpp"
SOURCE = "src/lyra/runtime/jit_execution.cpp"
BINDINGS = "src/lyra/jit/executor.cpp"

# An entry opens a line, so a prototype and a definition are the same shape and
# are read the same way. An indented match is a continuation line or a nested
# declaration, and neither publishes a symbol.
RE_ENTRY = re.compile(r"^(?:auto|void)\s+(lyra_rt_\w+)\s*\(", re.MULTILINE)
RE_BINDING = re.compile(r'add\(\s*"(lyra_rt_\w+)"\s*,\s*&(lyra_rt_\w+)\s*\)')

VIOLATION_HINT = (
    "An entry is one contract written in three places. Add the side that is "
    "missing rather than deleting the side that has no partner: an entry the "
    "session reaches under one spelling and the runtime defines under another "
    "is what these rules exist to make unspellable."
)


class Entry(NamedTuple):
    name: str
    line: int


class Binding(NamedTuple):
    name: str
    target: str
    line: int


class Abi(NamedTuple):
    declared: list[Entry]
    defined: list[Entry]
    bound: list[Binding]


def line_of(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def entries_of(text: str) -> list[Entry]:
    return [
        Entry(m.group(1), line_of(text, m.start()))
        for m in RE_ENTRY.finditer(text)
    ]


def bindings_of(text: str) -> list[Binding]:
    return [
        Binding(m.group(1), m.group(2), line_of(text, m.start()))
        for m in RE_BINDING.finditer(text)
    ]


def by_name(entries: list[Entry]) -> list[Entry]:
    return sorted(entries, key=lambda entry: entry.name)


def check_r001(abi: Abi) -> list[str]:
    declared = {entry.name for entry in abi.declared}
    defined = {entry.name for entry in abi.defined}
    return [
        f"  {SOURCE}:{entry.line}: R001 '{entry.name}' is defined but the ABI "
        f"header declares no such entry, so nothing can reach it"
        for entry in by_name(abi.defined)
        if entry.name not in declared
    ] + [
        f"  {HEADER}:{entry.line}: R001 '{entry.name}' is declared but the "
        f"runtime defines no such entry"
        for entry in by_name(abi.declared)
        if entry.name not in defined
    ]


def check_r002(abi: Abi) -> list[str]:
    bound = {binding.name for binding in abi.bound}
    return [
        f"  {HEADER}:{entry.line}: R002 '{entry.name}' is declared but never "
        f"bound, so it resolves only while the module runs in this process"
        for entry in by_name(abi.declared)
        if entry.name not in bound
    ]


def check_r003(abi: Abi) -> list[str]:
    declared = {entry.name for entry in abi.declared}
    errors = []
    first_bound_at: dict[str, int] = {}
    for binding in abi.bound:
        if binding.name != binding.target:
            errors.append(
                f"  {BINDINGS}:{binding.line}: R003 '{binding.name}' is bound "
                f"to '{binding.target}', so calling it runs another entry")
        elif binding.name not in declared:
            errors.append(
                f"  {BINDINGS}:{binding.line}: R003 '{binding.name}' is bound "
                f"but the ABI header declares no such entry")
        if binding.name in first_bound_at:
            errors.append(
                f"  {BINDINGS}:{binding.line}: R003 '{binding.name}' is bound "
                f"again, having been bound at line "
                f"{first_bound_at[binding.name]}")
        else:
            first_bound_at[binding.name] = binding.line
    return errors


def load(root: Path) -> Abi:
    return Abi(
        declared=entries_of((root / HEADER).read_text()),
        defined=entries_of((root / SOURCE).read_text()),
        bound=bindings_of((root / BINDINGS).read_text()))


def run_self_tests() -> bool:
    def expect(cond, msg):
        if not cond:
            print(f"SELF-TEST FAILED: {msg}")
            return False
        return True

    def abi(header="", source="", bindings="") -> Abi:
        return Abi(
            declared=entries_of(header),
            defined=entries_of(source),
            bound=bindings_of(bindings))

    ok = True
    ok &= expect(
        entries_of("auto lyra_rt_dynarray_new(const void* n) -> void*;")
        == [Entry("lyra_rt_dynarray_new", 1)],
        "an entry returning a value is read, with its line")
    ok &= expect(
        entries_of("\n\nvoid lyra_rt_cell_packed_set(void* cell);")
        == [Entry("lyra_rt_cell_packed_set", 3)],
        "an entry returning nothing is read, with its line")
    ok &= expect(
        not entries_of("  const void* size, void* p)"),
        "a continuation line declares no entry")
    ok &= expect(
        not entries_of("  auto lyra_rt_helper(void* p) -> void*;"),
        "an indented declaration is not an entry")
    ok &= expect(
        bindings_of('add("lyra_rt_dynarray_new", &lyra_rt_dynarray_new);')
        == [Binding("lyra_rt_dynarray_new", "lyra_rt_dynarray_new", 1)],
        "a binding yields the name it publishes and the function it names")
    ok &= expect(
        not bindings_of("auto lyra_rt_dynarray_new(void* p) -> void*;"),
        "a declaration is not a binding")

    ok &= expect(
        len(check_r001(abi(header="auto lyra_rt_a() -> void*;"))) == 1,
        "R001 reports a prototype the runtime does not define")
    ok &= expect(
        len(check_r001(abi(source="auto lyra_rt_a() -> void*{}"))) == 1,
        "R001 reports a definition the header does not declare")
    ok &= expect(
        not check_r001(
            abi(header="auto lyra_rt_a() -> void*;",
                source="auto lyra_rt_a() -> void*{}")),
        "R001 is silent when both sides carry the entry")
    ok &= expect(
        len(check_r002(abi(header="auto lyra_rt_a() -> void*;"))) == 1,
        "R002 reports a declared entry nothing binds")
    ok &= expect(
        len(check_r003(abi(bindings='add("lyra_rt_a", &lyra_rt_b);'))) == 1,
        "R003 reports a binding pointed at another entry")
    ok &= expect(
        len(
            check_r003(
                abi(header="auto lyra_rt_a() -> void*;",
                    bindings='add("lyra_rt_a", &lyra_rt_a);\n'
                             'add("lyra_rt_a", &lyra_rt_a);'))) == 1,
        "R003 reports the same entry bound twice")
    return ok


def main() -> int:
    if not run_self_tests():
        return 1

    abi = load(Path(__file__).resolve().parents[2])
    failures = check_r001(abi) + check_r002(abi) + check_r003(abi)

    if failures:
        print("Runtime ABI check failed:")
        print("\n".join(failures))
        print()
        print(VIOLATION_HINT)
        return 1

    print(
        f"Runtime ABI check passed: {len(abi.declared)} entries, each "
        f"declared, defined, and bound to itself once")
    return 0


if __name__ == "__main__":
    sys.exit(main())
