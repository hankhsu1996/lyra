#!/usr/bin/env python3
"""Architecture boundary policy enforcement.

Enforces minimal cross-layer boundaries between IR layers and adjacent
subsystems. Rules are kept small on purpose; future cuts add rules
incrementally.

Rules:

1. HIR must not contain MIR/runtime terms.
   Scope: src/lyra/hir/**/*.cpp and include/lyra/hir/**/*.hpp
   Forbidden tokens: Runtime, Engine

2. HIR must not include MIR headers.
   Scope: src/lyra/hir/**/*.cpp and include/lyra/hir/**/*.hpp
   Forbidden include: #include "lyra/mir/..."

3. HIR and MIR must not include driver or backend headers.
   Scope: src/lyra/{hir,mir}/**/*.cpp and include/lyra/{hir,mir}/**/*.hpp
   Forbidden includes: #include "lyra/driver/..." and #include "lyra/backend/..."

Usage:
  python3 tools/policy/check_architecture_boundaries.py
"""

import re
import sys
from pathlib import Path

HIR_FORBIDDEN_TERMS = ["Runtime", "Engine"]
HIR_FORBIDDEN_TERM_PATTERN = re.compile(
    r"\b(" + "|".join(HIR_FORBIDDEN_TERMS) + r")\b"
)

HIR_FORBIDDEN_INCLUDE_PATTERN = re.compile(
    r'#\s*include\s*"lyra/mir/'
)

IR_FORBIDDEN_INCLUDE_PATTERN = re.compile(
    r'#\s*include\s*"lyra/(driver|backend)/'
)


def strip_comment(line: str) -> str:
    """Remove // comments from a line (simple heuristic, not a full parser)."""
    idx = line.find("//")
    if idx != -1:
        return line[:idx]
    return line


def iter_layer_files(repo_root: Path, layer: str):
    """Yield (path, rel_path) for .cpp and .hpp files under src and include for a layer."""
    for root in (repo_root / "src" / "lyra" / layer,
                 repo_root / "include" / "lyra" / layer):
        if not root.exists():
            continue
        for ext in ("*.cpp", "*.hpp"):
            for path in root.rglob(ext):
                yield path, path.relative_to(repo_root).as_posix()


def check_hir_forbidden_terms(repo_root: Path) -> list[str]:
    """Rule 1: HIR sources must not reference MIR/runtime terms."""
    errors = []
    for path, rel_path in iter_layer_files(repo_root, "hir"):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            code = strip_comment(line)
            match = HIR_FORBIDDEN_TERM_PATTERN.search(code)
            if match:
                errors.append(f"  {rel_path}:{lineno}: {match.group(0)}")
    return errors


def check_hir_no_mir_include(repo_root: Path) -> list[str]:
    """Rule 2: HIR must not include MIR headers."""
    errors = []
    for path, rel_path in iter_layer_files(repo_root, "hir"):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            if HIR_FORBIDDEN_INCLUDE_PATTERN.search(line):
                errors.append(f"  {rel_path}:{lineno}: {line.strip()}")
    return errors


def check_ir_no_driver_or_backend_include(repo_root: Path) -> list[str]:
    """Rule 3: HIR and MIR must not include driver or backend headers."""
    errors = []
    for layer in ("hir", "mir"):
        for path, rel_path in iter_layer_files(repo_root, layer):
            for lineno, line in enumerate(path.read_text().splitlines(), 1):
                if IR_FORBIDDEN_INCLUDE_PATTERN.search(line):
                    errors.append(f"  {rel_path}:{lineno}: {line.strip()}")
    return errors


def run_self_tests() -> bool:
    """Verify each rule's pattern fires on a positive example and ignores negatives."""
    if not HIR_FORBIDDEN_TERM_PATTERN.search("class Runtime {"):
        print("SELF-TEST FAILED: forbidden term not detected")
        return False
    if not HIR_FORBIDDEN_TERM_PATTERN.search("auto x = Engine{};"):
        print("SELF-TEST FAILED: forbidden term not detected")
        return False
    if HIR_FORBIDDEN_TERM_PATTERN.search("auto runtime_local = 1;"):
        print("SELF-TEST FAILED: pattern matched non-banned identifier")
        return False
    if HIR_FORBIDDEN_TERM_PATTERN.search("class Slot {};"):
        print("SELF-TEST FAILED: pattern matched non-banned identifier")
        return False

    if not HIR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/mir/dump.hpp"'):
        print("SELF-TEST FAILED: MIR include not detected")
        return False
    if HIR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/hir/dump.hpp"'):
        print("SELF-TEST FAILED: HIR include incorrectly flagged")
        return False

    if not IR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/driver/cli.hpp"'):
        print("SELF-TEST FAILED: driver include not detected")
        return False
    if not IR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/backend/cpp/api.hpp"'):
        print("SELF-TEST FAILED: backend include not detected")
        return False
    if IR_FORBIDDEN_INCLUDE_PATTERN.search('#include "lyra/support/overloaded.hpp"'):
        print("SELF-TEST FAILED: support include incorrectly flagged")
        return False

    if strip_comment("auto x = 1;  // Runtime in comments is fine") != "auto x = 1;  ":
        print("SELF-TEST FAILED: strip_comment did not remove trailing comment")
        return False

    return True


def main() -> int:
    if not run_self_tests():
        return 1

    repo_root = Path(__file__).resolve().parent.parent.parent

    term_errors = check_hir_forbidden_terms(repo_root)
    mir_include_errors = check_hir_no_mir_include(repo_root)
    layer_include_errors = check_ir_no_driver_or_backend_include(repo_root)

    all_errors = term_errors + mir_include_errors + layer_include_errors
    if not all_errors:
        print("OK: architecture boundaries enforced")
        return 0

    if term_errors:
        print("ERROR: HIR contains forbidden runtime/MIR terms:")
        print()
        for e in term_errors:
            print(e)
        print()
    if mir_include_errors:
        print("ERROR: HIR includes MIR headers:")
        print()
        for e in mir_include_errors:
            print(e)
        print()
    if layer_include_errors:
        print("ERROR: HIR/MIR includes driver or backend headers:")
        print()
        for e in layer_include_errors:
            print(e)
        print()
    print("See tools/policy/check_architecture_boundaries.py for rules.")
    return 1


if __name__ == "__main__":
    sys.exit(main())
