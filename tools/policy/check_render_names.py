#!/usr/bin/env python3
"""No value-emission entry names a runtime library identifier.

`docs/architecture/backend_contract.md` invariant 8: every name a value-emission
entry writes is either the target language's own syntax or the answer of a
dispatch that owns naming -- type mapping for a type, place access for a
wrapper's access protocol, the shared runtime-entry declaration for an
operation. The entry composes punctuation around what its children render to and
looks nothing up itself.

That is checkable by reading rather than by reasoning, which is the whole reason
the invariant is phrased the way it is: a string literal in an emitter that
carries a library name is the defect, whatever it was written for. This is the
reading, done mechanically.

**What separates a name from target syntax is a capital letter.** C++'s own
syntax is lowercase keywords and punctuation, so an identifier written with a
capital in it is something the emitter was told rather than something the
language spells, and the only question left is who told it. The exceptions are
listed below and there are two kinds, kept apart on purpose: syntax that happens
to carry a capital, which is permanent, and a library name an emitter still
writes, which is an open item and is meant to reach zero.

The earlier form of this check matched `lyra::`-qualified names alone, so it
never saw the runtime method spellings an emitter had invented for itself --
they carry no namespace, being written where a receiver already stands. It
passed, and partly for the wrong reason. The qualified-name rule stays beside
the capital-letter one because a bare namespace prefix has no capital of its
own.

Type mapping is exempt by the contract -- naming a library type is its job -- and
so are diagnostics, which name the compiler's own functions and types on purpose
and reach no emitted artifact.
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

# The value-emission entries. `render_type.cpp` is absent on purpose: it is the
# type-mapping dispatch, and invariant 3 makes it the one place a runtime
# library type is spelled.
EMITTERS = [
    "src/lyra/backend/cpp/render_expr.cpp",
    "src/lyra/backend/cpp/render_call.cpp",
    "src/lyra/backend/cpp/render_stmt.cpp",
    "src/lyra/backend/cpp/render_decl.cpp",
]

# Target syntax that carries a capital, which no rule about names can tell from
# a name. Permanent, and short by construction: C++ spells almost nothing this
# way.
TARGET_SYNTAX = {
    # The suffixes that fix an integer literal's own type, so a machine integer
    # is read back at the width it was written: `LL` for a signed one, `ULL`
    # for a bit pattern in hex, which a signed spelling would narrow.
    "LL",
    "ULL",
    # The language linkage a DPI-C entry point is declared with (LRM 35.4).
    "C",
}

# A library name an emitter still writes, and why it is not yet reachable
# through a dispatch that owns naming. Each is an open item of the
# mechanical-translation workstream, not a shape the contract admits: an entry
# added here needs the reason it cannot be read from somewhere, and taking one
# away is what finishing that item looks like. Empty is the finished state.
ADMITTED: dict[str, str] = {}

STRING_LITERAL = re.compile(r'"(?:[^"\\\n]|\\.)*"')
RAW_STRING = re.compile(r'R"([^("\s\\]*)\(')
LINE_COMMENT = re.compile(r"//[^\n]*")
BLOCK_COMMENT = re.compile(r"/\*.*?\*/", re.DOTALL)
IDENTIFIER = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
QUALIFIED_NAME = re.compile(r"lyra::[A-Za-z_][A-Za-z0-9_:]*|lyra::")
# A message handed to one of these names the compiler's own functions and types
# and reaches no emitted artifact, so its literals say nothing about invariant 8.
DIAGNOSTIC_CALL = re.compile(r"\b(?:InternalError|Unsupported)\s*\(\s*")


@dataclass
class Literal:
    line: int
    text: str
    in_diagnostic: bool


def literals_of(source: str) -> list[Literal]:
    """Every string literal in the source, and whether a diagnostic takes it.

    A single pass, because deciding either question needs the other: telling a
    literal from a comment needs the scan, and telling which call encloses a
    literal needs parentheses counted outside of literals.
    """
    found: list[Literal] = []
    # The parenthesis depths at which a diagnostic call is still open.
    diagnostics: list[int] = []
    depth = 0
    position = 0
    while position < len(source):
        rest = source[position:]
        if comment := LINE_COMMENT.match(rest) or BLOCK_COMMENT.match(rest):
            position += comment.end()
            continue
        if raw := RAW_STRING.match(rest):
            close = f"){raw.group(1)}\""
            end = source.find(close, position + raw.end())
            end = len(source) if end < 0 else end + len(close)
            found.append(
                Literal(
                    line=source.count("\n", 0, position) + 1,
                    text=source[position + raw.end() : end - len(close)],
                    in_diagnostic=bool(diagnostics),
                )
            )
            position = end
            continue
        if literal := STRING_LITERAL.match(rest):
            found.append(
                Literal(
                    line=source.count("\n", 0, position) + 1,
                    text=literal.group()[1:-1],
                    in_diagnostic=bool(diagnostics),
                )
            )
            position += literal.end()
            continue
        if call := DIAGNOSTIC_CALL.match(rest):
            diagnostics.append(depth)
            depth += 1
            position += call.end()
            continue
        character = source[position]
        if character == "(":
            depth += 1
        elif character == ")":
            depth -= 1
            while diagnostics and diagnostics[-1] >= depth:
                diagnostics.pop()
        position += 1
    return found


def names_in(literal: str) -> list[str]:
    """The runtime library names the literal writes, if any."""
    written = [
        name
        for name in QUALIFIED_NAME.findall(literal)
        if name not in TARGET_SYNTAX
    ]
    written += [
        token
        for token in IDENTIFIER.findall(literal)
        if token[0].isupper() and token not in TARGET_SYNTAX
    ]
    return written


def violations() -> list[str]:
    found: list[str] = []
    for relative in EMITTERS:
        path = ROOT / relative
        if not path.exists():
            found.append(f"{relative}: listed as an emitter but does not exist")
            continue
        for literal in literals_of(path.read_text()):
            if literal.in_diagnostic:
                continue
            for name in names_in(literal.text):
                if name in ADMITTED:
                    continue
                found.append(
                    f"{relative}:{literal.line}: value emission names "
                    f"`{name}`; a name it emits comes from type mapping, "
                    f"place access, or the runtime-entry declaration"
                )
    return found


def main() -> int:
    found = violations()
    if found:
        for line in found:
            print(line, file=sys.stderr)
        return 1
    remaining = (
        "none admitted"
        if not ADMITTED
        else f"{len(ADMITTED)} admitted, each with its reason"
    )
    print(f"OK: value emission names no runtime library identifier ({remaining})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
