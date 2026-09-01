#!/usr/bin/env python3
"""Check that the runtime ABI is one contract, not three lists.

An entry the generated module calls exists in three places: a prototype in the
ABI header, a definition beside the runtime it wraps, and a binding that gives
the execution session its address. The host compiler holds the definition to the
prototype, and nothing holds the binding to either -- so an entry can be defined
and unreachable, or bound under a name nothing defines, and either way the
failure lands at run time as an unresolved symbol rather than as a build error.

Rules:
  R001: The entries declared in the ABI header and the entries defined beside
        the runtime are the same set.
  R002: Every entry declared in the ABI header is bound to its definition.
  R003: Every binding names an entry the header declares, and binds it to the
        function of that same name.

Usage:
  python3 tools/policy/check_runtime_abi.py
"""

import re
import sys
from pathlib import Path

HEADER = Path("include/lyra/runtime/jit_execution.hpp")
SOURCE = Path("src/lyra/runtime/jit_execution.cpp")
BINDINGS = Path("src/lyra/jit/executor.cpp")

# An entry opens a line, so a prototype and a definition are the same shape and
# are read the same way.
RE_ENTRY = re.compile(r"^(?:auto|void)\s+(lyra_rt_\w+)\s*\(", re.MULTILINE)
RE_BINDING = re.compile(
    r'add\(\s*"(lyra_rt_\w+)"\s*,\s*&(lyra_rt_\w+)\s*\)', re.DOTALL)


def main() -> int:
    root = Path(__file__).resolve().parents[2]
    header = (root / HEADER).read_text()
    source = (root / SOURCE).read_text()
    bindings = (root / BINDINGS).read_text()

    declared = set(RE_ENTRY.findall(header))
    defined = set(RE_ENTRY.findall(source))
    bound = RE_BINDING.findall(bindings)
    bound_names = {name for name, _ in bound}

    failures = []
    for entry in sorted(defined - declared):
        failures.append(
            f"  {SOURCE}: R001 '{entry}' is defined but the ABI header does "
            f"not declare it, so nothing can reach it")
    for entry in sorted(declared - defined):
        failures.append(
            f"  {HEADER}: R001 '{entry}' is declared but the runtime defines "
            f"no such entry")
    for entry in sorted(declared - bound_names):
        failures.append(
            f"  {HEADER}: R002 '{entry}' is declared but never bound, so a "
            f"call to it fails to resolve at run time")
    for name, target in bound:
        if name != target:
            failures.append(
                f"  {BINDINGS}: R003 '{name}' is bound to '{target}'")
        elif name not in declared:
            failures.append(
                f"  {BINDINGS}: R003 '{name}' is bound but the ABI header "
                f"does not declare it")

    if failures:
        print("Runtime ABI check failed:")
        print("\n".join(failures))
        return 1
    print(
        f"Runtime ABI check passed: {len(declared)} entries, each declared, "
        f"defined, and bound to itself")
    return 0


if __name__ == "__main__":
    sys.exit(main())
