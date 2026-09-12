#!/usr/bin/env python3
"""Every source name the C++ backend writes goes through the map that spells it.

`docs/decisions/a-name-is-a-relation-not-an-identity.md`: what a declaration is
called in a target language is that target's to mint, and a source name reaches
the target's identifier space through one total, injective map. SystemVerilog
admits every printable non-space character in an identifier (LRM 5.6.1) and C++
both admits fewer and reserves some of what is left, so a name written straight
into emitted text is a legal program that does not compile -- and nothing in the
compiler reports it, because the emitted text is only read by a host compiler
that runs later, if at all.

The failure this catches is not a missing map but a *bypassed* one, and it is
invisible from either end. A declaration's definition and the references to it
are emitted by different functions; when one routes its name through the map and
the other does not, the two ends spell one declaration differently. That is a
link error where the name is legal C++ and a syntax error where it is not, and
both were live: a package variable was defined as `a+b` and referenced as its
escape, and a package subroutine the other way round.

So the rule is positional rather than semantic, which is what makes it
checkable, and it is written to fail closed. Everything an emitter does with a
name ends in the artifact, whether the name is formatted into a larger string or
returned as the whole of one, so a name an emitter reads is a violation unless
it is an argument of the map, an argument of a call that looks something up
rather than writing it, or listed below with the reason it is neither.
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

# Everything that writes C++ declarations or expressions. `naming.hpp` is absent
# on purpose: it defines the map, so it is the one place a name is handled
# without having been mapped yet.
EMITTERS = [
    "src/lyra/backend/cpp/emit_cpp.cpp",
    "src/lyra/backend/cpp/render_call.cpp",
    "src/lyra/backend/cpp/render_decl.cpp",
    "src/lyra/backend/cpp/render_expr.cpp",
    "src/lyra/backend/cpp/render_stmt.cpp",
    "src/lyra/backend/cpp/render_type.cpp",
]

# The functions that answer what a declaration is spelled in C++. A name inside
# a call to one of these has been mapped by definition; the rest of this script
# is about names that are not.
MAPPERS = [
    "ToCppName",
    "UnitNamespaceOf",
    "CppClassCallableName",
    "CppAbiAdapterName",
    "CppStaticConstantName",
    "CppStorageEntryName",
    "CppUnitCallableName",
]

# An expression an emitter writes without mapping, and why that is right. Each
# entry names the expression rather than the line it sits on, so the entry
# survives the code moving and stops applying when the expression changes --
# which is when it wants re-reading. Each needs a reason that does not reduce to
# "it happens to work today"; an entry added without one is a bypass wearing an
# exemption. Empty is not the finished state here: a name that never belonged to
# SystemVerilog's identifier space has nothing to be mapped out of.
ADMITTED: dict[str, str] = {
    "t.linkage_name": (
        "a DPI-C linkage name is an identifier of C, not of SystemVerilog "
        "(LRM 35.4), so it is already spelled the way the target must see it"
    ),
    "f.qualified_name": (
        "a runtime library entry is declared in C++ and named by the library, "
        "so what spells it is the entry declaration rather than this map"
    ),
    "callee.name": (
        "a resolved callee is text this backend has already spelled, so "
        "mapping it again would escape the punctuation it is made of"
    ),
    "root.name": (
        "the design root's own name is formatted into a quoted slot, so it "
        "leaves as a string the simulation reports itself by rather than as an "
        "identifier"
    ),
    "record.unit_name": (
        "a unit this backend declines to emit is named in the diagnostic that "
        "declines it, which reaches the reader rather than the artifact -- and "
        "the name it has to carry is the one the reader compiled, not one "
        "spelled for a language that never sees this unit"
    ),
}

LINE_COMMENT = re.compile(r"//[^\n]*")
BLOCK_COMMENT = re.compile(r"/\*.*?\*/", re.DOTALL)
STRING_LITERAL = re.compile(r'"(?:[^"\\\n]|\\.)*"')
CHAR_LITERAL = re.compile(r"'(?:[^'\\\n]|\\.)'")
# A member read whose member is a name: `.name`, `->callable_name`. The pattern
# anchors nothing to its left, because a name is as often reached through an
# arena as through a plain member. A designated initializer names the same
# member and is a write, so the lookahead lets an `=` through while keeping a
# comparison.
NAME_MEMBER = re.compile(
    r"(?:\.|->)\s*(?:[A-Za-z_][A-Za-z0-9_]*_)?name\b(?!\s*=[^=])")
# A name can also arrive as an element of a list of names rather than as a
# member, and a class's behaviors are the one such list an emitter reads. A list
# added beside it is invisible here until it is named, which is the one way this
# check can be wrong in the direction that matters.
NAME_ELEMENT = re.compile(r"(?:\.|->)\s*behaviors\s*\[")
MAPPER_CALL = re.compile(rf"\b(?:{'|'.join(MAPPERS)})\s*\(")
# The calls that take a name to find or collect something rather than to write
# it, which is what a name may be read for without being spelled.
LOOKUPS: dict[str, str] = {
    "FindExternalClass": (
        "resolves a consumed promise by the unit and class it names, so the "
        "name is a key rather than a spelling"
    ),
    "add": (
        "collects which units this one includes; each is mapped again where "
        "the include line that names it is written"
    ),
}
LOOKUP_CALL = re.compile(rf"\b(?:{'|'.join(LOOKUPS)})\s*\(")
# What an expression may be spelled out of, walking back from a name to the
# whole read it belongs to. Brackets are matched so an arena's subscript stays
# part of the expression rather than ending it.
EXPRESSION_CHARS = set(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.->()[]")


@dataclass
class Source:
    """One emitter's text, with what is not code blanked out.

    Blanking rather than removing keeps every offset equal to the original's,
    so a line number computed here names the line a reader opens.
    """

    relative: str
    code: str

    def line_of(self, position: int) -> int:
        return self.code.count("\n", 0, position) + 1


def blanked(text: str) -> str:
    out = list(text)
    position = 0
    while position < len(text):
        rest = text[position:]
        match = (
            LINE_COMMENT.match(rest)
            or BLOCK_COMMENT.match(rest)
            or STRING_LITERAL.match(rest)
            or CHAR_LITERAL.match(rest)
        )
        if not match:
            position += 1
            continue
        for index in range(position, position + match.end()):
            if out[index] != "\n":
                out[index] = " "
        position += match.end()
    return "".join(out)


def argument_spans(code: str, call: re.Pattern[str]) -> list[tuple[int, int]]:
    """Half-open ranges covering the arguments of every call the pattern opens."""
    spans: list[tuple[int, int]] = []
    for opening in call.finditer(code):
        depth = 0
        position = opening.end() - 1
        while position < len(code):
            if code[position] == "(":
                depth += 1
            elif code[position] == ")":
                depth -= 1
                if depth == 0:
                    spans.append((opening.end(), position))
                    break
            position += 1
    return spans


def expression_before(code: str, end: int) -> str:
    """The read a name is the tail of, so a report and an entry name the same
    thing however the receiver is spelled."""
    start = end
    depth = 0
    while start > 0:
        character = code[start - 1]
        if character in ")]":
            depth += 1
        elif character in "([":
            if depth == 0:
                break
            depth -= 1
        elif character not in EXPRESSION_CHARS:
            break
        start -= 1
    return code[start:end]


def stale(blob: str, exercised: set[str]) -> list[str]:
    """The entries above that nothing reaches any more.

    An exemption nobody exercises reads exactly like one that is working, so
    without this it outlives the expression it was written for and the summary
    below goes on counting it. The same holds for a name in either list: one
    that has been renamed away stops exempting anything, and the entry is then
    a sentence about the code that is no longer true.
    """
    out: list[str] = []
    for name in MAPPERS:
        if not re.search(rf"\b{re.escape(name)}\s*\(", blob):
            out.append(f"{name}: listed as a mapper but no emitter calls it")
    for name in LOOKUPS:
        if not re.search(rf"\b{re.escape(name)}\s*\(", blob):
            out.append(f"{name}: listed as a lookup but no emitter calls it")
    for written in ADMITTED:
        if written not in exercised:
            out.append(f"`{written}`: admitted but no emitter writes it")
    return out


def violations() -> list[str]:
    found: list[str] = []
    sources: list[Source] = []
    exercised: set[str] = set()
    for relative in EMITTERS:
        path = ROOT / relative
        if not path.exists():
            found.append(f"{relative}: listed as an emitter but does not exist")
            continue
        source = Source(relative, blanked(path.read_text()))
        sources.append(source)
        mapped = argument_spans(source.code, MAPPER_CALL)
        looked_up = argument_spans(source.code, LOOKUP_CALL)
        reads = list(NAME_MEMBER.finditer(source.code))
        reads += list(NAME_ELEMENT.finditer(source.code))
        for member in reads:
            position = member.start()
            written = " ".join(
                (expression_before(source.code, position)
                 + member.group()).split())
            if written in ADMITTED:
                exercised.add(written)
                continue
            if any(start <= position < end for start, end in mapped):
                continue
            if any(start <= position < end for start, end in looked_up):
                continue
            found.append(
                f"{relative}:{source.line_of(position)}: `{written}` "
                f"reaches emitted text without the map that spells a source "
                f"name as a C++ identifier"
            )
    found += stale("\n".join(source.code for source in sources), exercised)
    return found


def main() -> int:
    found = violations()
    if found:
        for line in found:
            print(line, file=sys.stderr)
        return 1
    admitted = (
        "none admitted"
        if not ADMITTED
        else f"{len(ADMITTED)} admitted, each with its reason"
    )
    print(f"OK: every source name the C++ backend writes is mapped ({admitted})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
