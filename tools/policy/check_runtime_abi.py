#!/usr/bin/env python3
"""Runtime ABI policy.

An entry the generated module calls exists in three places: a prototype in the
ABI header, a definition beside the runtime it wraps, and a line in the list of
what the library publishes, which every generated module is checked against
before anything links it. The host compiler holds a definition to its
prototype and nothing else -- so an entry can be defined and left off the list,
or listed under a name nothing defines, and either way the failure lands as a
design refused, or as a link that cannot resolve a name, rather than as a build
error. These rules are the sides the compiler does not hold.

Rules:

  R001  The entries the ABI header declares and the entries the runtime
        defines are the same set. An entry on one side only is either a
        definition nothing can reach or a prototype for code that does not
        exist.

  R002  Every declared entry is listed as published. An entry left off the
        list is refused by name in every design that calls it, although the
        library defines it and the program would link.

  R003  Every listing names a declared entry, reads its shape off the function
        of that same name, and lists it once. A name listed against another
        entry's function checks every call against the wrong shape, and a name
        listed twice keeps whichever listing ran last without saying so.

  R004  Whether an entry takes the engine handle is stated once. Its runtime
        entry declaration says so, and its ABI prototype takes the handle as a
        parameter; a call site composes the operands from the first while the
        runtime reads them by the second. A disagreement compiles and links,
        and then hands the runtime every operand shifted by one.

        Only an entry the ABI declares under its own name is checked. One
        realized per value representation publishes a symbol per
        representation, and one this backend does not realize publishes none;
        neither has a prototype to read the answer off.

  R008  Whether an entry can raise is stated once, the same way as R004.
        Its runtime entry declaration says a call to it only returns, and its
        prototype is `noexcept`; a call site builds no landing for the first,
        and the compiler holds the definition to the second. A row claiming an
        entry cannot raise while its definition can sends a departure through
        a frame that has nowhere to land it.

        Scoped like R004.

  R009  The operations over integral values are published as one table, whose
        entries both sides read off one declaration list, so nothing about an
        entry in it is written twice. The table's own name is the exception.
        The generated module loads it by a string and the runtime defines it
        as a symbol, so the two are one name stated once on each side. A
        disagreement is a module asking for a symbol nothing defines, which a
        link reports with no word about which operation was meant.

  R010  Whether an entry is handed the report of the evaluation calling it is
        stated once, the same way as R004: its runtime entry declaration
        says so, and its ABI prototype takes the report as a parameter named
        `evaluation`. Scoped like R004.

  R011  Whether an entry is handed where the operation calling it was written
        is stated once, the same way as R004: its runtime entry declaration
        says so, and its ABI prototype takes the location as a parameter
        named `location`, after every operand, since a call states it last of
        its operands. Only what the module adds after every operand follows
        it: what the entry is told of a type, and the storage of its answer. A
        disagreement compiles and links, and then the runtime places a report
        of the design's failure at whatever lies at that address. Scoped like
        R004.

Usage:
  python3 tools/policy/check_runtime_abi.py
"""

import re
import sys
from pathlib import Path
from typing import NamedTuple

HEADER = "include/lyra/runtime/runtime_abi.hpp"
SOURCE = "src/lyra/runtime/runtime_abi.cpp"
BINDINGS = "src/lyra/program/program_sink.cpp"
ENTRIES = "src/lyra/support/builtin_fn.cpp"
# Where the table of operations over integral values is named for the
# generated module, and where the runtime defines it.
TABLE_NAME = "include/lyra/runtime/integral_abi.hpp"
TABLE_DEFINITION = "src/lyra/runtime/integral_abi.cpp"

# An entry opens a line, so a prototype and a definition are the same shape and
# are read the same way. An indented match is a continuation line or a nested
# declaration, and neither publishes a symbol. An attribute may lead the line:
# an entry that carries one publishes its symbol like any other, and a pattern
# blind to it would leave that entry unchecked on every rule below.
RE_ATTRIBUTE = r"(?:\[\[[\w:, ]+\]\]\s*)*"
RE_ENTRY = re.compile(
    rf"^{RE_ATTRIBUTE}(?:auto|void)\s+(lyra_rt_\w+)\s*\(", re.MULTILINE)
RE_BINDING = re.compile(r'add\(\s*"(lyra_rt_\w+)"\s*,\s*&(lyra_rt_\w+)\s*\)')
# Whether a prototype may raise, read where its parameter list closes.
RE_NOEXCEPT = re.compile(r"\s*noexcept")
# One row of the runtime entry declaration: the entry it declares, and the
# properties it states.
RE_ROW = re.compile(r"case BuiltinFn::\w+:\s*return\s*\{(.*?)\};", re.S)
RE_ROW_NAME = re.compile(r'\.name = "(\w+)"')
# The table's name as the string the generated module loads it by, and as the
# symbol a definition opening a line gives it.
RE_TABLE_NAME = re.compile(r'kIntegralEntriesSymbol\s*=\s*"(lyra_rt_\w+)"')
RE_TABLE_DEFINITION = re.compile(
    r"^constinit const [\w:]+\s+(lyra_rt_\w+)\s*=", re.MULTILINE)

HANDLE_PARAMETER = "runtime"
HANDLE_PROPERTY = ".takes_the_runtime_handle = true"
REPORT_PARAMETER = "evaluation"
REPORT_PROPERTY = ".takes_the_read_report = true"
LOCATION_PARAMETER = "location"
NO_LOCATION_PROPERTY = ".takes_the_location = false"
# What the module hands after every operand: what an entry is told of a type,
# and the storage of its answer. An operand can end in the same word, so these
# are named rather than matched.
AFTER_EVERY_OPERAND = frozenset({
    "answer_type", "answer_element_type", "answer_operations", "array_type",
    "array_operations", "memory_type", "source_type", "value_operations",
    "value_take_positions", "out"})
RETURN_PROPERTY = ".ending = CallEnding::kReturns"

VIOLATION_HINT = (
    "An entry is one contract written in three places. Add the side that is "
    "missing rather than deleting the side that has no partner: an entry a "
    "module is checked against under one spelling and the runtime defines "
    "under another is what these rules exist to make unspellable."
)


class Entry(NamedTuple):
    name: str
    line: int


class Binding(NamedTuple):
    name: str
    target: str
    line: int


class Prototype(NamedTuple):
    name: str
    line: int
    parameters: list[str]
    cannot_raise: bool


class Abi(NamedTuple):
    declared: list[Entry]
    defined: list[Entry]
    bound: list[Binding]
    handle_takers: set[str] = set()
    report_takers: set[str] = set()
    location_takers: set[str] = set()
    non_raisers: set[str] = set()
    handle_rows: dict[str, bool] = {}
    report_rows: dict[str, bool] = {}
    location_rows: dict[str, bool] = {}
    return_rows: dict[str, bool] = {}
    misplaced_locations: list[Entry] = []
    table_named: list[str] = []
    table_defined: list[str] = []


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


def prototypes_of(text: str) -> list[Prototype]:
    """Each declared entry with its parameters and whether it can raise.

    The parameter list is closed by the parenthesis that matches its opening
    one and split only at commas outside any nested pair, because a parameter
    may be a code address whose own type holds a parameter list.
    """
    prototypes = []
    for m in RE_ENTRY.finditer(text):
        depth = 1
        position = m.end()
        start = position
        parameters = []
        while depth:
            character = text[position]
            if character == "(":
                depth += 1
            elif character == ")":
                depth -= 1
            elif character == "," and depth == 1:
                parameters.append(text[start:position])
                start = position + 1
            position += 1
        last = text[start:position - 1]
        if last.strip():
            parameters.append(last)
        noexcept = RE_NOEXCEPT.match(text, position)
        prototypes.append(
            Prototype(
                m.group(1), line_of(text, m.start()),
                [parameter.strip() for parameter in parameters],
                noexcept is not None))
    return prototypes


def takers_of(text: str, parameter_name: str) -> set[str]:
    """The declared entries whose prototype takes a parameter of that name."""
    return {
        prototype.name
        for prototype in prototypes_of(text)
        if any(
            parameter_name in re.findall(r"\w+", parameter)
            for parameter in prototype.parameters)
    }


def misplaced_locations_of(text: str) -> list[Entry]:
    """The declared entries taking the location ahead of an operand.

    A parameter following it is what the entry is told of a type or the storage
    of its answer, and that storage never precedes it.
    """
    misplaced = []
    for prototype in prototypes_of(text):
        names = [re.findall(r"\w+", parameter)[-1]
                 for parameter in prototype.parameters]
        if LOCATION_PARAMETER not in names:
            continue
        at = names.index(LOCATION_PARAMETER)
        if (any(name in AFTER_EVERY_OPERAND for name in names[:at])
                or any(name not in AFTER_EVERY_OPERAND
                       for name in names[at + 1:])):
            misplaced.append(Entry(prototype.name, prototype.line))
    return misplaced


def handle_takers_of(text: str) -> set[str]:
    """The declared entries whose prototype takes the engine handle."""
    return takers_of(text, HANDLE_PARAMETER)


def non_raisers_of(text: str) -> set[str]:
    """The declared entries whose prototype says they cannot raise."""
    return {
        prototype.name
        for prototype in prototypes_of(text)
        if prototype.cannot_raise
    }


def rows_of(text: str, states: str) -> dict[str, bool]:
    """Each runtime entry, against whether its row states `states`."""
    rows = {}
    for m in RE_ROW.finditer(text):
        body = m.group(1)
        name = RE_ROW_NAME.search(body)
        if name is not None:
            rows[name.group(1)] = states in body
    return rows


def location_rows_of(text: str) -> dict[str, bool]:
    """Each runtime entry, against whether it takes a location: every entry
    does unless its row states that it takes none."""
    return {
        name: not free
        for name, free in rows_of(text, NO_LOCATION_PROPERTY).items()
    }


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
        f"listed as published, so every design that calls it is refused"
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
                f"  {BINDINGS}:{binding.line}: R003 '{binding.name}' is listed "
                f"against '{binding.target}', so its calls are checked against "
                f"another entry's shape")
        elif binding.name not in declared:
            errors.append(
                f"  {BINDINGS}:{binding.line}: R003 '{binding.name}' is listed "
                f"but the ABI header declares no such entry")
        if binding.name in first_bound_at:
            errors.append(
                f"  {BINDINGS}:{binding.line}: R003 '{binding.name}' is listed "
                f"again, having been listed at line "
                f"{first_bound_at[binding.name]}")
        else:
            first_bound_at[binding.name] = binding.line
    return errors


def check_agreement(
        abi: Abi, rule: str, rows: dict[str, bool], prototypes: set[str],
        fact: str, carried: str) -> list[str]:
    """Each entry's row against what its own ABI prototype says.

    One comparison for both facts: what separates them is which property the
    row states and which part of the prototype answers, and each side is read
    before this is reached.
    """
    declared = {entry.name for entry in abi.declared}
    errors = []
    for name, states_it in sorted(rows.items()):
        symbol = f"lyra_rt_{name}"
        if symbol not in declared:
            continue
        has_it = symbol in prototypes
        if states_it and not has_it:
            errors.append(
                f"  {ENTRIES}: {rule} '{name}' states that it {fact}, and "
                f"'{symbol}' {carried} no such thing")
        elif has_it and not states_it:
            errors.append(
                f"  {ENTRIES}: {rule} '{name}' does not state that it {fact}, "
                f"and '{symbol}' {carried} one")
    return errors


def check_r004(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R004", abi.handle_rows, abi.handle_takers,
        "takes the engine handle", "declares")


def check_r010(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R010", abi.report_rows, abi.report_takers,
        "is handed the evaluation's report", "takes")


def check_r011(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R011", abi.location_rows, abi.location_takers,
        "is handed where the operation calling it was written", "takes") + [
            f"  {HEADER}:{entry.line}: R011 '{entry.name}' takes the location "
            f"after the storage of its answer or ahead of an operand, where "
            f"the generated module hands it after every operand and ahead of "
            f"that storage"
            for entry in by_name(abi.misplaced_locations)
        ]


def check_r008(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R008", abi.return_rows, abi.non_raisers, "cannot raise",
        "declares")


def check_r009(abi: Abi) -> list[str]:
    if len(abi.table_named) != 1:
        return [
            f"  {TABLE_NAME}: R009 the table of integral operations is named "
            f"for the generated module {len(abi.table_named)} times, and it is "
            f"one table"]
    if len(abi.table_defined) != 1:
        return [
            f"  {TABLE_DEFINITION}: R009 the table of integral operations is "
            f"defined {len(abi.table_defined)} times, and it is one table"]
    if abi.table_named != abi.table_defined:
        return [
            f"  {TABLE_DEFINITION}: R009 the generated module loads the table "
            f"of integral operations as '{abi.table_named[0]}' and the runtime "
            f"defines it as '{abi.table_defined[0]}'"]
    return []


def load(root: Path) -> Abi:
    header = (root / HEADER).read_text()
    entries = (root / ENTRIES).read_text()
    return Abi(
        declared=entries_of(header),
        defined=entries_of((root / SOURCE).read_text()),
        table_named=RE_TABLE_NAME.findall((root / TABLE_NAME).read_text()),
        table_defined=RE_TABLE_DEFINITION.findall(
            (root / TABLE_DEFINITION).read_text()),
        bound=bindings_of((root / BINDINGS).read_text()),
        handle_takers=handle_takers_of(header),
        report_takers=takers_of(header, REPORT_PARAMETER),
        location_takers=takers_of(header, LOCATION_PARAMETER),
        misplaced_locations=misplaced_locations_of(header),
        non_raisers=non_raisers_of(header),
        handle_rows=rows_of(entries, HANDLE_PROPERTY),
        report_rows=rows_of(entries, REPORT_PROPERTY),
        location_rows=location_rows_of(entries),
        return_rows=rows_of(entries, RETURN_PROPERTY))


def run_self_tests() -> bool:
    def expect(cond, msg):
        if not cond:
            print(f"SELF-TEST FAILED: {msg}")
            return False
        return True

    def abi(header="", source="", bindings="", entries="") -> Abi:
        return Abi(
            declared=entries_of(header),
            defined=entries_of(source),
            bound=bindings_of(bindings),
            handle_takers=handle_takers_of(header),
            report_takers=takers_of(header, REPORT_PARAMETER),
            location_takers=takers_of(header, LOCATION_PARAMETER),
            misplaced_locations=misplaced_locations_of(header),
            non_raisers=non_raisers_of(header),
            handle_rows=rows_of(entries, HANDLE_PROPERTY),
            report_rows=rows_of(entries, REPORT_PROPERTY),
            location_rows=location_rows_of(entries),
            return_rows=rows_of(entries, RETURN_PROPERTY))

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
        [e.name for e in entries_of("[[noreturn]] void lyra_rt_a();")]
        == ["lyra_rt_a"],
        "an entry an attribute leads is still an entry")
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
        "R002 reports a declared entry nothing lists")
    ok &= expect(
        len(check_r003(abi(bindings='add("lyra_rt_a", &lyra_rt_b);'))) == 1,
        "R003 reports a listing pointed at another entry")
    ok &= expect(
        len(
            check_r003(
                abi(header="auto lyra_rt_a() -> void*;",
                    bindings='add("lyra_rt_a", &lyra_rt_a);\n'
                             'add("lyra_rt_a", &lyra_rt_a);'))) == 1,
        "R003 reports the same entry listed twice")

    row = 'case BuiltinFn::kA:\n      return {{.name = "a"{}}};'
    takes = "auto lyra_rt_a(void* runtime) -> void*;"
    takes_none = "auto lyra_rt_a(const void* p) -> void*;"
    states = row.format(", .takes_the_runtime_handle = true")
    states_none = row.format("")
    ok &= expect(
        handle_takers_of(
            "auto lyra_rt_a(\n    void* runtime,\n    const void* p)"
            " -> void*;") == {"lyra_rt_a"},
        "a prototype spanning lines is read for the handle it takes")
    ok &= expect(
        rows_of(states, HANDLE_PROPERTY) == {"a": True}
        and rows_of(states_none, HANDLE_PROPERTY) == {"a": False},
        "a row yields the entry it declares and what it says about a property")
    ok &= expect(
        len(check_r004(abi(header=takes_none, entries=states))) == 1,
        "R004 reports a row claiming a handle the prototype does not take")
    ok &= expect(
        len(check_r004(abi(header=takes, entries=states_none))) == 1,
        "R004 reports a prototype taking a handle the row does not state")
    ok &= expect(
        not check_r004(abi(header=takes, entries=states)),
        "R004 is silent when the two sides agree")
    ok &= expect(
        not check_r004(abi(entries=states)),
        "R004 says nothing about an entry the ABI does not declare by name")

    reports = "auto lyra_rt_a(const void* p, void* evaluation) -> void*;"
    states_report = row.format(", .takes_the_read_report = true")
    ok &= expect(
        len(check_r010(abi(header=takes_none, entries=states_report))) == 1,
        "R010 reports a row claiming a report the prototype does not take")
    ok &= expect(
        len(check_r010(abi(header=reports, entries=states_none))) == 1,
        "R010 reports a prototype taking a report the row does not state")
    ok &= expect(
        not check_r010(abi(header=reports, entries=states_report)),
        "R010 is silent when the two sides agree")

    located = "auto lyra_rt_a(const void* p, const void* location) -> void*;"
    no_location = row.format(", .takes_the_location = false")
    takes_location = states_none
    ok &= expect(
        len(check_r011(abi(header=takes_none, entries=takes_location))) == 1,
        "R011 reports a row not stating it takes none whose prototype "
        "takes no location")
    ok &= expect(
        len(check_r011(abi(header=located, entries=no_location))) == 1,
        "R011 reports a prototype taking a location the row says it does not")
    ok &= expect(
        not check_r011(abi(header=located, entries=takes_location)),
        "R011 is silent when an entry that can report takes a location")
    ok &= expect(
        not check_r011(abi(header=takes_none, entries=no_location)),
        "R011 is silent when an entry stating it takes none takes none")
    answered = (
        "auto lyra_rt_a(const void* p, const void* location, void* out)"
        " -> void*;")
    told = (
        "auto lyra_rt_a(const void* p, const void* location, "
        "const void* answer_type, void* out) -> void*;")
    past_the_answer = (
        "auto lyra_rt_a(const void* p, void* out, const void* location)"
        " -> void*;")
    ahead_of_an_operand = (
        "auto lyra_rt_a(const void* location, const void* p) -> void*;")
    ok &= expect(
        not misplaced_locations_of(answered)
        and not misplaced_locations_of(told),
        "only what an entry is told and the storage of its answer follow the "
        "location")
    ok &= expect(
        len(misplaced_locations_of(past_the_answer)) == 1,
        "a location past the storage of the answer is misplaced")
    ok &= expect(
        len(misplaced_locations_of(ahead_of_an_operand)) == 1,
        "a location ahead of an operand is misplaced")
    ok &= expect(
        len(misplaced_locations_of(
            "auto lyra_rt_a(const void* location, const void* values_type)"
            " -> void*;")) == 1,
        "an operand ending as what an entry is told is still an operand")
    ok &= expect(
        len(check_r011(abi(header=past_the_answer, entries=takes_location)))
        == 1,
        "R011 reports a prototype taking the location past the storage of its "
        "answer")

    raises_none = "void lyra_rt_a(void* p) noexcept;"
    raises = "void lyra_rt_a(void* p);"
    says_returns = row.format(", .ending = CallEnding::kReturns")
    ok &= expect(
        non_raisers_of(raises_none) == {"lyra_rt_a"}
        and not non_raisers_of(raises),
        "a prototype is read for whether it can raise")
    ok &= expect(
        len(check_r008(abi(header=raises, entries=says_returns))) == 1,
        "R008 reports a row claiming an entry the prototype lets raise")
    ok &= expect(
        len(check_r008(abi(header=raises_none, entries=states_none))) == 1,
        "R008 reports a prototype that cannot raise under a row that may depart")
    ok &= expect(
        not check_r008(abi(header=raises_none, entries=says_returns)),
        "R008 is silent when the two sides agree")

    named = 'kIntegralEntriesSymbol =\n    "lyra_rt_t";'
    defines = (
        "extern constinit const lyra::runtime::IntegralEntries\n"
        "    lyra_rt_t;\n"
        "constinit const lyra::runtime::IntegralEntries lyra_rt_{} =\n"
        "    Table();")

    def table(name, definition) -> Abi:
        return Abi(
            [], [], [], table_named=RE_TABLE_NAME.findall(name),
            table_defined=RE_TABLE_DEFINITION.findall(definition))

    ok &= expect(
        not check_r009(table(named, defines.format("t"))),
        "R009 is silent when the table is defined under the name it is "
        "loaded by")
    ok &= expect(
        len(check_r009(table(named, defines.format("u")))) == 1,
        "R009 reports a table defined under another name than it is loaded by")
    ok &= expect(
        len(check_r009(table("", defines.format("t")))) == 1,
        "R009 reports a table the generated module is given no name for")
    ok &= expect(
        len(check_r009(table(named, ""))) == 1,
        "R009 reports a table nothing defines")

    ok &= expect(
        prototypes_of(
            "auto lyra_rt_a(\n    const void* p, void* (*body)(void* self, "
            "const void* item),\n    void* runtime) -> bool;")
        == [Prototype(
            "lyra_rt_a", 1,
            ["const void* p", "void* (*body)(void* self, const void* item)",
             "void* runtime"], False)],
        "a parameter holding a parameter list of its own is read whole, and "
        "the ones after it are still read")
    return ok


def main() -> int:
    if not run_self_tests():
        return 1

    abi = load(Path(__file__).resolve().parents[2])
    failures = (
        check_r001(abi) + check_r002(abi) + check_r003(abi) + check_r004(abi)
        + check_r008(abi) + check_r009(abi)
        + check_r010(abi) + check_r011(abi))

    if failures:
        print("Runtime ABI check failed:")
        print("\n".join(failures))
        print()
        print(VIOLATION_HINT)
        return 1

    print(
        f"Runtime ABI check passed: {len(abi.declared)} entries, each "
        f"declared, defined, and listed as itself once")
    return 0


if __name__ == "__main__":
    sys.exit(main())
