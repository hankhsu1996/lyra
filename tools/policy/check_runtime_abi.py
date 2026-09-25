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

  R005  Whether a call to an entry parks the caller is stated once, the same
        way. An entry that parks answers the generated module whether it must
        suspend, so its prototype returns the host `bool` that answer crosses
        as; everything an entry computes for the program crosses as an opaque
        value instead. So the return type says it, and the entry declaration
        has to agree -- a disagreement either suspends where the runtime did
        not park or runs on where it did.

        Scoped like R004, and to entries the ABI declares by their own name.

  R006  How much of a scope's construction is the same for every class is
        stated once. The entry type says it: the parameters before the run of
        values a class is parameterized by. The generated side composes that
        prototype from a count of its own, because it describes this boundary
        in its own terms rather than including the host's declaration of it --
        which is the whole reason a value crosses here as an opaque pointer.
        So the two sides cannot be held together by the compiler, and a
        disagreement emits a call whose arguments the runtime reads shifted,
        which links and runs.

  R007  An entry takes a span only while two integer argument registers are
        still free for it. The generated module hands a span over as its two
        words and places each on its own, while the host's C ABI places a
        struct that no longer fits the remaining registers wholly on the
        stack. Past the sixth word the two disagree: the runtime reads the
        pointer from one place and the length from another, and the length it
        reads was never one. Nothing else sees it, because the call links and
        the entry is well-formed on both sides.

  R008  Whether an entry can raise is stated once, the same way as R005.
        Its runtime entry declaration says a call to it only returns, and its
        prototype is `noexcept`; a call site builds no landing for the first,
        and the compiler holds the definition to the second. A row claiming an
        entry cannot raise while its definition can sends a departure through
        a frame that has nowhere to land it.

        Scoped like R004.

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
CONSTRUCT_ENTRY = "include/lyra/runtime/scope_program.hpp"
CONSTRUCT_PROTOTYPE = "include/lyra/backend/llvm/runtime_entry.hpp"

# An entry opens a line, so a prototype and a definition are the same shape and
# are read the same way. An indented match is a continuation line or a nested
# declaration, and neither publishes a symbol. An attribute may lead the line:
# an entry that carries one publishes its symbol like any other, and a pattern
# blind to it would leave that entry unchecked on every rule below.
RE_ATTRIBUTE = r"(?:\[\[[\w:, ]+\]\]\s*)*"
RE_ENTRY = re.compile(
    rf"^{RE_ATTRIBUTE}(?:auto|void)\s+(lyra_rt_\w+)\s*\(", re.MULTILINE)
RE_BINDING = re.compile(r'add\(\s*"(lyra_rt_\w+)"\s*,\s*&(lyra_rt_\w+)\s*\)')
# Whether a prototype may raise, read where its parameter list closes, and then
# what it answers with. An entry may answer with a code address, whose type
# holds parentheses of its own, so the answer runs to the end of the declaration
# rather than to the first one.
RE_NOEXCEPT = re.compile(r"\s*noexcept")
RE_ANSWER = re.compile(r"\s*->\s*([^;{]+)")
# One row of the runtime entry declaration: the entry it declares, and the
# properties it states.
RE_ROW = re.compile(r"case BuiltinFn::\w+:\s*return\s*\{(.*?)\};", re.S)
RE_ROW_NAME = re.compile(r'\.name = "(\w+)"')
# The construct entry's own parameter list, and the count the generated side
# composes its prototype from. The list ends at the first `)` because every
# parameter here is a pointer or the plain-data run, never a function type.
RE_CONSTRUCT_ENTRY = re.compile(
    r"using\s+ScopeConstructEntry\s*=\s*\w+\s*\(\s*\*\s*\)\s*\(([^)]*)\)")
RE_CONSTRUCT_SHARED = re.compile(
    r"kScopeConstructSharedParams\s*=\s*(\d+)")

# How many integer argument registers the host's calling convention has
# (System V AMD64), what a span is spelled as, and the parameter types that go
# in floating-point registers instead and so take none of them.
INTEGER_ARGUMENT_REGISTERS = 6
SPAN_PARAMETER = "LyraSpan"
FLOAT_PARAMETERS = {"double", "float"}

HANDLE_PARAMETER = "runtime"
HANDLE_PROPERTY = ".takes_the_runtime_handle = true"
PARK_ANSWER = "bool"
PARK_PROPERTY = ".parks_the_caller = true"
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
    answer: str
    cannot_raise: bool


class Abi(NamedTuple):
    declared: list[Entry]
    defined: list[Entry]
    bound: list[Binding]
    handle_takers: set[str] = set()
    parkers: set[str] = set()
    non_raisers: set[str] = set()
    handle_rows: dict[str, bool] = {}
    park_rows: dict[str, bool] = {}
    return_rows: dict[str, bool] = {}
    # Absent where the side it is read from declares nothing of the kind, which
    # R006 reports rather than passing over.
    construct_shared: int | None = None
    construct_stated: int | None = None
    spilled_spans: list[Entry] = []


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
    """Each declared entry with its parameters and what it answers with.

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
        if noexcept:
            position = noexcept.end()
        answer = RE_ANSWER.match(text, position)
        prototypes.append(
            Prototype(
                m.group(1), line_of(text, m.start()),
                [parameter.strip() for parameter in parameters],
                answer.group(1).strip() if answer else "",
                noexcept is not None))
    return prototypes


def handle_takers_of(text: str) -> set[str]:
    """The declared entries whose prototype takes the engine handle."""
    return {
        prototype.name
        for prototype in prototypes_of(text)
        if any(
            HANDLE_PARAMETER in re.findall(r"\w+", parameter)
            for parameter in prototype.parameters)
    }


def parkers_of(text: str) -> set[str]:
    """The declared entries whose prototype answers whether the caller parks."""
    return {
        prototype.name
        for prototype in prototypes_of(text)
        if prototype.answer == PARK_ANSWER
    }


def non_raisers_of(text: str) -> set[str]:
    """The declared entries whose prototype says they cannot raise."""
    return {
        prototype.name
        for prototype in prototypes_of(text)
        if prototype.cannot_raise
    }


def spilled_spans_of(text: str) -> list[Entry]:
    """The declared entries taking a span no integer register pair is left for.

    A parameter takes one integer register unless it is a floating-point value,
    which takes none of them, or a span, which takes two.
    """
    spilled = []
    for prototype in prototypes_of(text):
        used = 0
        for parameter in prototype.parameters:
            words = set(re.findall(r"\w+", parameter))
            if SPAN_PARAMETER in words:
                if used + 2 > INTEGER_ARGUMENT_REGISTERS:
                    spilled.append(Entry(prototype.name, prototype.line))
                    break
                used += 2
            elif "*" in parameter or not words & FLOAT_PARAMETERS:
                used += 1
    return spilled


def rows_of(text: str, states: str) -> dict[str, bool]:
    """Each runtime entry, against whether its row states `states`."""
    rows = {}
    for m in RE_ROW.finditer(text):
        body = m.group(1)
        name = RE_ROW_NAME.search(body)
        if name is not None:
            rows[name.group(1)] = states in body
    return rows


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


def construct_shared_of(text: str) -> int | None:
    """How many parameters the entry type shares, from the type itself.

    Every parameter but the last is one every construction takes; the last is
    the run of values a class is parameterized by, which is what makes one
    prototype serve them all.
    """
    match = RE_CONSTRUCT_ENTRY.search(text)
    if match is None:
        return None
    return len(match.group(1).split(",")) - 1


def construct_stated_of(text: str) -> int | None:
    match = RE_CONSTRUCT_SHARED.search(text)
    return None if match is None else int(match.group(1))


def check_r006(abi: Abi) -> list[str]:
    if abi.construct_shared is None:
        return [
            f"  {CONSTRUCT_ENTRY}: R006 declares no construction entry type, "
            f"so nothing states how much of a construction every class shares"
        ]
    if abi.construct_stated is None:
        return [
            f"  {CONSTRUCT_PROTOTYPE}: R006 composes the construction "
            f"prototype without saying how much of it is shared"
        ]
    if abi.construct_shared == abi.construct_stated:
        return []
    return [
        f"  {CONSTRUCT_PROTOTYPE}: R006 the generated side composes "
        f"{abi.construct_stated} shared parameters and the entry type takes "
        f"{abi.construct_shared}"
    ]


def check_r007(abi: Abi) -> list[str]:
    return [
        f"  {HEADER}:{entry.line}: R007 '{entry.name}' takes a span after the "
        f"integer argument registers are spent, so the runtime reads its length "
        f"from somewhere the generated module did not put it"
        for entry in abi.spilled_spans
    ]


def check_r004(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R004", abi.handle_rows, abi.handle_takers,
        "takes the engine handle", "declares")


def check_r005(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R005", abi.park_rows, abi.parkers, "parks the caller",
        "answers with")


def check_r008(abi: Abi) -> list[str]:
    return check_agreement(
        abi, "R008", abi.return_rows, abi.non_raisers, "cannot raise",
        "declares")


def load(root: Path) -> Abi:
    header = (root / HEADER).read_text()
    entries = (root / ENTRIES).read_text()
    return Abi(
        declared=entries_of(header),
        defined=entries_of((root / SOURCE).read_text()),
        bound=bindings_of((root / BINDINGS).read_text()),
        handle_takers=handle_takers_of(header),
        parkers=parkers_of(header),
        non_raisers=non_raisers_of(header),
        handle_rows=rows_of(entries, HANDLE_PROPERTY),
        park_rows=rows_of(entries, PARK_PROPERTY),
        return_rows=rows_of(entries, RETURN_PROPERTY),
        construct_shared=construct_shared_of(
            (root / CONSTRUCT_ENTRY).read_text()),
        construct_stated=construct_stated_of(
            (root / CONSTRUCT_PROTOTYPE).read_text()),
        spilled_spans=spilled_spans_of(header))


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
            parkers=parkers_of(header),
            non_raisers=non_raisers_of(header),
            handle_rows=rows_of(entries, HANDLE_PROPERTY),
            park_rows=rows_of(entries, PARK_PROPERTY),
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

    parks = "auto lyra_rt_a(void* p) -> bool;"
    parks_none = "auto lyra_rt_a(void* p) -> void*;"
    says_parks = row.format(", .parks_the_caller = true")
    ok &= expect(
        parkers_of(parks) == {"lyra_rt_a"} and not parkers_of(parks_none),
        "a prototype is read for whether it answers the park question")
    ok &= expect(
        not parkers_of("void lyra_rt_a(void* p);"),
        "an entry answering with nothing does not answer the park question")
    ok &= expect(
        len(check_r005(abi(header=parks_none, entries=says_parks))) == 1,
        "R005 reports a row claiming a park the prototype does not answer")
    ok &= expect(
        len(check_r005(abi(header=parks, entries=states_none))) == 1,
        "R005 reports a prototype answering a park the row does not state")
    ok &= expect(
        not check_r005(abi(header=parks, entries=says_parks)),
        "R005 is silent when the two sides agree")
    ok &= expect(
        parkers_of("auto lyra_rt_a(void* p) noexcept -> bool;")
        == {"lyra_rt_a"},
        "a prototype that cannot raise is still read for the park question")

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

    entry_type = (
        "using ScopeConstructEntry = void (*)(\n"
        "    Scope* self, Scope* parent, HierarchySegment* segment,\n"
        "    ScopeConstructArguments arguments);")
    ok &= expect(
        construct_shared_of(entry_type) == 3,
        "the shared parameters are the entry type's own, less the run")
    ok &= expect(
        construct_stated_of(
            "inline constexpr std::size_t kScopeConstructSharedParams = 3;")
        == 3,
        "the generated side's count is read")
    agree = Abi([], [], [], construct_shared=3, construct_stated=3)
    disagree = Abi([], [], [], construct_shared=4, construct_stated=3)
    ok &= expect(not check_r006(agree), "R006 is silent when the two agree")
    ok &= expect(
        len(check_r006(disagree)) == 1,
        "R006 reports a count the entry type does not take")
    ok &= expect(
        len(check_r006(Abi([], [], [], construct_stated=3))) == 1,
        "R006 reports an entry type it could not read")
    ok &= expect(
        len(check_r006(Abi([], [], [], construct_shared=3))) == 1,
        "R006 reports a generated side that states no count")

    ok &= expect(
        not spilled_spans_of(
            "auto lyra_rt_a(const void* p, LyraSpan a, LyraSpan b) -> void*;"),
        "a span with a register pair left for it is not reported")
    ok &= expect(
        [e.name for e in spilled_spans_of(
            "auto lyra_rt_a(\n    const void* p, LyraSpan a, LyraSpan b,\n"
            "    LyraSpan c) -> void*;")] == ["lyra_rt_a"],
        "a span past the sixth integer register is reported")
    ok &= expect(
        not spilled_spans_of(
            "auto lyra_rt_a(double x, double y, LyraSpan a, LyraSpan b,\n"
            "    LyraSpan c) -> void*;"),
        "a floating-point parameter takes no integer register")
    ok &= expect(
        len(spilled_spans_of(
            "auto lyra_rt_a(double* x, void* y, void* z, LyraSpan a, "
            "LyraSpan b) -> void*;")) == 1,
        "a pointer to a floating-point value takes an integer register")
    ok &= expect(
        len(check_r007(Abi([], [], [], spilled_spans=[Entry("lyra_rt_a", 1)])))
        == 1,
        "R007 reports each entry it was handed")

    ok &= expect(
        prototypes_of(
            "auto lyra_rt_a(\n    const void* p, void* (*body)(void* self, "
            "const void* item),\n    void* runtime) -> bool;")
        == [Prototype(
            "lyra_rt_a", 1,
            ["const void* p", "void* (*body)(void* self, const void* item)",
             "void* runtime"], "bool", False)],
        "a parameter holding a parameter list of its own is read whole, and "
        "the ones after it are still read")
    ok &= expect(
        prototypes_of("auto lyra_rt_a(void* s) -> void (*)();")[0].answer
        == "void (*)()",
        "an answer that is a code address is read to the end of its type")
    return ok


def main() -> int:
    if not run_self_tests():
        return 1

    abi = load(Path(__file__).resolve().parents[2])
    failures = (
        check_r001(abi) + check_r002(abi) + check_r003(abi) + check_r004(abi)
        + check_r005(abi) + check_r006(abi) + check_r007(abi)
        + check_r008(abi))

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
