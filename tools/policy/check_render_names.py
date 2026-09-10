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

Type mapping is exempt by the contract -- naming a library type is its job.
Everything else is listed below, and an entry that still names one is written
down with the reason, so what is left is counted rather than remembered.
"""

from __future__ import annotations

import re
import sys
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

# A library name a value-emission entry still writes, and why it is not yet
# reachable through a dispatch that owns naming. Each is an open item of the
# mechanical-translation workstream, not a shape the contract admits: an entry
# added here needs the reason it cannot be read from somewhere, and taking one
# away is what finishing that item looks like.
ADMITTED = {
    "lyra::runtime::ScopeExit": (
        "the C++ realization of a body paired with a cleanup. No MIR type names "
        "it, so type mapping has nothing to answer with; what it needs is for "
        "the pairing to reach the backend as something the type system carries."
    ),
    "lyra::runtime::GcObject": (
        "the base every managed class is emitted with. It is a fact about how "
        "this target realizes the object model rather than a type any node "
        "names, and it belongs with the rest of that realization."
    ),
    "lyra::runtime::": (
        "the namespace an imported class's method symbol is qualified with. The "
        "symbol itself is read from the shared declaration; only the namespace "
        "is written here, and it belongs with the type mapping that already "
        "spells that namespace for every other library name."
    ),
}

STRING_LITERAL = re.compile(r'"((?:[^"\\]|\\.)*)"')
LIBRARY_NAME = re.compile(r"lyra::[A-Za-z_][A-Za-z0-9_:]*")


def violations() -> list[str]:
    found: list[str] = []
    for relative in EMITTERS:
        path = ROOT / relative
        if not path.exists():
            found.append(f"{relative}: listed as an emitter but does not exist")
            continue
        for number, line in enumerate(path.read_text().splitlines(), start=1):
            for literal in STRING_LITERAL.findall(line):
                for name in LIBRARY_NAME.findall(literal):
                    if name in ADMITTED:
                        continue
                    found.append(
                        f"{relative}:{number}: value emission names the runtime "
                        f"library's `{name}`; a name it emits comes from type "
                        f"mapping, place access, or the runtime-entry "
                        f"declaration"
                    )
    return found


def main() -> int:
    found = violations()
    if found:
        for line in found:
            print(line, file=sys.stderr)
        return 1
    print(
        f"OK: value emission names no runtime library identifier "
        f"({len(ADMITTED)} admitted, each with its reason)"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
