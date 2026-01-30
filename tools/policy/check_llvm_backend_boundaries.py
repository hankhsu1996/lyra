#!/usr/bin/env python3
"""LLVM backend boundary policy enforcement.

This script enforces architectural boundaries in the LLVM backend:

1. Runtime store getters (GetLyraStore*) must be called only from:
   - src/lyra/llvm_backend/commit/ (store operations)
   - src/lyra/llvm_backend/lifecycle/ (lifecycle getters only)

2. Commit-internal symbols must not leak outside commit module:
   - .GetWriteTarget( and .GetCanonicalRootSignalId( only in commit/
   - IsDesignSlot symbols only in commit/

3. WriteTarget struct definition only in include/lyra/llvm_backend/commit/

4. Module boundaries (after refactor):
   - compute/ must not include commit.hpp or lifecycle.hpp
   - layout/ must not include context.hpp
   - instruction/ must not include commit internals (commit/access.hpp)

Usage:
  python3 tools/policy/check_llvm_backend_boundaries.py
"""

import re
import sys
from pathlib import Path

COMMIT_DIR = "src/lyra/llvm_backend/commit/"
COMMIT_INCLUDE_DIR = "include/lyra/llvm_backend/commit/"
LIFECYCLE_DIR = "src/lyra/llvm_backend/lifecycle/"
COMPUTE_DIR = "src/lyra/llvm_backend/compute/"
LAYOUT_DIR = "src/lyra/llvm_backend/layout/"
TYPE_OPS_DIR = "src/lyra/llvm_backend/type_ops/"
INSTRUCTION_DIR = "src/lyra/llvm_backend/instruction/"

STORE_GETTERS = [
    "GetLyraStorePacked",
    "GetLyraStoreDynArray",
    "GetLyraStoreString",
]

LIFECYCLE_GETTERS = [
    "GetLyraStringRetain",
    "GetLyraStringRelease",
    "GetLyraDynArrayClone",
    "GetLyraDynArrayRelease",
    "GetLyraDynArrayDestroy",
]

COMMIT_ONLY_PATTERNS = [
    (r"\.GetWriteTarget\(", ".GetWriteTarget("),
    (r"\.GetCanonicalRootSignalId\(", ".GetCanonicalRootSignalId("),
]

ISDESIGN_PATTERN = re.compile(r"\bIsDesignSlot\w*\b")


def strip_comment(line: str) -> str:
    """Remove // comments from a line (simple heuristic, not a full parser)."""
    idx = line.find("//")
    if idx != -1:
        return line[:idx]
    return line


def is_in_commit_dir(rel_path: str) -> bool:
    """Check if path is in the commit module source directory."""
    return rel_path.startswith(COMMIT_DIR)


def is_in_commit_include_dir(rel_path: str) -> bool:
    """Check if path is in the commit module include directory."""
    return rel_path.startswith(COMMIT_INCLUDE_DIR)


def is_in_lifecycle_dir(rel_path: str) -> bool:
    """Check if path is in the lifecycle module directory."""
    return rel_path.startswith(LIFECYCLE_DIR)


def is_in_compute_dir(rel_path: str) -> bool:
    """Check if path is in the compute module directory."""
    return rel_path.startswith(COMPUTE_DIR)


def is_in_layout_dir(rel_path: str) -> bool:
    """Check if path is in the layout module directory."""
    return rel_path.startswith(LAYOUT_DIR)


def is_in_type_ops_dir(rel_path: str) -> bool:
    """Check if path is in the type_ops module directory."""
    return rel_path.startswith(TYPE_OPS_DIR)


def is_in_instruction_dir(rel_path: str) -> bool:
    """Check if path is in the instruction module directory."""
    return rel_path.startswith(INSTRUCTION_DIR)


def is_context_impl(rel_path: str) -> bool:
    """Check if this is context.cpp or context_*.cpp (where Context methods are implemented)."""
    name = Path(rel_path).name
    return (rel_path == "src/lyra/llvm_backend/context.cpp" or
            (rel_path.startswith("src/lyra/llvm_backend/") and name.startswith("context_") and name.endswith(".cpp")))


def check_store_getter_boundaries(repo_root: Path) -> list[str]:
    """Check that store/lifecycle getters in type_ops files go through commit/lifecycle.

    Scope: src/lyra/llvm_backend/type_ops/*.cpp files (the main handler files).
    Other files (context*.cpp defines getters, instruction handlers have legitimate uses)
    are not checked here.
    """
    type_ops_dir = repo_root / "src/lyra/llvm_backend/type_ops"
    if not type_ops_dir.exists():
        return []

    type_ops_files = list(type_ops_dir.glob("*.cpp"))
    errors = []

    for cpp in type_ops_files:
        rel_path = cpp.relative_to(repo_root).as_posix()
        content = cpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            code = strip_comment(line)

            # Store getters: type_ops should go through commit/
            for pattern in STORE_GETTERS:
                if pattern in code:
                    errors.append(f"  {rel_path}:{lineno}: {pattern}")

            # Lifecycle getters: type_ops should go through lifecycle/
            for pattern in LIFECYCLE_GETTERS:
                if pattern in code:
                    errors.append(f"  {rel_path}:{lineno}: {pattern}")

    return errors


def check_commit_only_patterns(repo_root: Path) -> list[str]:
    """Check that commit-internal method calls don't leak outside.

    Scope: all .cpp under src/lyra/llvm_backend/ (recursive).
    Allowed: commit/ directory and context.cpp (implementation).
    """
    llvm_backend_dir = repo_root / "src/lyra/llvm_backend"
    errors = []

    for cpp in llvm_backend_dir.rglob("*.cpp"):
        rel_path = cpp.relative_to(repo_root).as_posix()

        # Skip commit/ - that's where these are allowed
        if is_in_commit_dir(rel_path):
            continue

        # context.cpp contains the private method implementations
        if is_context_impl(rel_path):
            continue

        content = cpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            code = strip_comment(line)
            for pattern, token in COMMIT_ONLY_PATTERNS:
                if re.search(pattern, code):
                    errors.append(f"  {rel_path}:{lineno}: {token} outside commit/")

    return errors


def check_isdesign_symbols(repo_root: Path) -> list[str]:
    """Check that IsDesignSlot symbols don't leak outside commit module."""
    errors = []

    # Check all .cpp under src/lyra/llvm_backend/ (excluding commit/)
    llvm_backend_dir = repo_root / "src/lyra/llvm_backend"
    for cpp in llvm_backend_dir.rglob("*.cpp"):
        rel_path = cpp.relative_to(repo_root).as_posix()

        # Skip commit/ - that's where these are allowed
        if is_in_commit_dir(rel_path):
            continue

        content = cpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            code = strip_comment(line)
            if ISDESIGN_PATTERN.search(code):
                errors.append(f"  {rel_path}:{lineno}: IsDesignSlot outside commit/")

    # Check include/lyra/llvm_backend/**/*.hpp (excluding commit/)
    include_dir = repo_root / "include/lyra/llvm_backend"
    for hpp in include_dir.rglob("*.hpp"):
        rel_path = hpp.relative_to(repo_root).as_posix()

        # Allowed in commit include directory
        if is_in_commit_include_dir(rel_path):
            continue

        content = hpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            code = strip_comment(line)
            if ISDESIGN_PATTERN.search(code):
                errors.append(f"  {rel_path}:{lineno}: IsDesignSlot outside commit/")

    return errors


def is_writetarget_definition(content: str) -> bool:
    """Check if content contains a WriteTarget struct definition."""
    return "struct WriteTarget {" in content


def check_write_target_definition(repo_root: Path) -> list[str]:
    """Check that WriteTarget struct is only defined in commit include directory."""
    include_dir = repo_root / "include"
    errors = []

    for hpp in include_dir.rglob("*.hpp"):
        rel_path = hpp.relative_to(repo_root).as_posix()

        # Allowed in commit include directory
        if is_in_commit_include_dir(rel_path):
            continue

        content = hpp.read_text()
        if is_writetarget_definition(content):
            errors.append(f"  {rel_path}: WriteTarget definition outside commit/")

    return errors


def check_compute_boundaries(repo_root: Path) -> list[str]:
    """Check that compute/ module doesn't include commit or lifecycle headers.

    compute/ is pure expression evaluation and must not depend on commit or lifecycle.
    All storage and lifecycle operations belong in instruction/ layer.

    Additionally, compute/ must not call GetPlacePointer() directly - all place
    reads must go through LoadPlaceValue() or LoadPlaceBaseValue() to enforce
    the rvalue purity boundary (no pointer exposure that could enable writes).
    """
    compute_dir = repo_root / "src/lyra/llvm_backend/compute"
    if not compute_dir.exists():
        return []

    errors = []
    forbidden_includes = [
        '#include "lyra/llvm_backend/commit.hpp"',
        '#include "lyra/llvm_backend/lifecycle.hpp"',
    ]
    forbidden_calls = [
        '.GetPlacePointer(',
    ]

    for cpp in compute_dir.rglob("*.cpp"):
        rel_path = cpp.relative_to(repo_root).as_posix()
        content = cpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            code = strip_comment(line)
            for forbidden in forbidden_includes:
                if forbidden in code:
                    errors.append(f"  {rel_path}:{lineno}: {forbidden}")
            for forbidden in forbidden_calls:
                if forbidden in code:
                    errors.append(f"  {rel_path}:{lineno}: {forbidden} (use LoadPlaceValue/LoadPlaceBaseValue instead)")

    # Also check compute headers
    compute_include_dir = repo_root / "include/lyra/llvm_backend/compute"
    if compute_include_dir.exists():
        for hpp in compute_include_dir.rglob("*.hpp"):
            rel_path = hpp.relative_to(repo_root).as_posix()
            content = hpp.read_text()
            lines = content.split('\n')

            for lineno, line in enumerate(lines, 1):
                code = strip_comment(line)
                for forbidden in forbidden_includes:
                    if forbidden in code:
                        errors.append(f"  {rel_path}:{lineno}: {forbidden}")
                for forbidden in forbidden_calls:
                    if forbidden in code:
                        errors.append(f"  {rel_path}:{lineno}: {forbidden} (use LoadPlaceValue/LoadPlaceBaseValue instead)")

    return errors


def check_layout_boundaries(repo_root: Path) -> list[str]:
    """Check that layout/ module doesn't include context.hpp.

    layout/ is the ABI oracle and must only use llvm::LLVMContext& and const TypeArena&.

    Exception: layout/union_storage.cpp needs Context for DataLayout access via
    context.GetModule().getDataLayout(). This should eventually be refactored to
    pass llvm::DataLayout& explicitly.
    """
    layout_dir = repo_root / "src/lyra/llvm_backend/layout"
    if not layout_dir.exists():
        return []

    errors = []
    forbidden_include = '#include "lyra/llvm_backend/context.hpp"'

    # Exception: union_storage.cpp needs Context for DataLayout
    allowed_exceptions = {"src/lyra/llvm_backend/layout/union_storage.cpp"}

    for cpp in layout_dir.rglob("*.cpp"):
        rel_path = cpp.relative_to(repo_root).as_posix()

        # Skip allowed exceptions
        if rel_path in allowed_exceptions:
            continue

        content = cpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            if forbidden_include in line:
                errors.append(f"  {rel_path}:{lineno}: {forbidden_include}")

    # Also check layout headers (no exceptions for headers)
    layout_include_dir = repo_root / "include/lyra/llvm_backend/layout"
    if layout_include_dir.exists():
        for hpp in layout_include_dir.rglob("*.hpp"):
            rel_path = hpp.relative_to(repo_root).as_posix()
            content = hpp.read_text()
            lines = content.split('\n')

            for lineno, line in enumerate(lines, 1):
                if forbidden_include in line:
                    errors.append(f"  {rel_path}:{lineno}: {forbidden_include}")

    return errors


def check_instruction_boundaries(repo_root: Path) -> list[str]:
    """Check that instruction/ module doesn't include commit internals.

    instruction/ should go through type_ops, not directly include commit internals.
    """
    instruction_dir = repo_root / "src/lyra/llvm_backend/instruction"
    if not instruction_dir.exists():
        return []

    errors = []
    forbidden_includes = [
        '#include "lyra/llvm_backend/commit/access.hpp"',
        '#include "lyra/llvm_backend/commit/notify.hpp"',
    ]

    for cpp in instruction_dir.rglob("*.cpp"):
        rel_path = cpp.relative_to(repo_root).as_posix()
        content = cpp.read_text()
        lines = content.split('\n')

        for lineno, line in enumerate(lines, 1):
            for forbidden in forbidden_includes:
                if forbidden in line:
                    errors.append(f"  {rel_path}:{lineno}: {forbidden}")

    # Also check instruction headers
    instruction_include_dir = repo_root / "include/lyra/llvm_backend/instruction"
    if instruction_include_dir.exists():
        for hpp in instruction_include_dir.rglob("*.hpp"):
            rel_path = hpp.relative_to(repo_root).as_posix()
            content = hpp.read_text()
            lines = content.split('\n')

            for lineno, line in enumerate(lines, 1):
                for forbidden in forbidden_includes:
                    if forbidden in line:
                        errors.append(f"  {rel_path}:{lineno}: {forbidden}")

    return errors


def run_self_tests() -> bool:
    """Run minimal self-tests to verify the checks work correctly."""
    # Test 1: commit-only pattern detection
    test_code = "auto wt = ctx.GetWriteTarget(target);"
    for pattern, _ in COMMIT_ONLY_PATTERNS:
        if re.search(pattern, test_code):
            break
    else:
        print("SELF-TEST FAILED: commit-only patterns don't match expected code")
        return False

    # Test 2: IsDesignSlot pattern detection
    test_code2 = "if (IsDesignSlot(ctx, target)) {"
    if not ISDESIGN_PATTERN.search(test_code2):
        print("SELF-TEST FAILED: IsDesignSlot pattern doesn't match expected code")
        return False

    # Test 3: Verify directory checks
    if not is_in_commit_dir("src/lyra/llvm_backend/commit/commit_packed.cpp"):
        print("SELF-TEST FAILED: is_in_commit_dir doesn't recognize commit files")
        return False

    if is_in_commit_dir("src/lyra/llvm_backend/type_ops/struct.cpp"):
        print("SELF-TEST FAILED: is_in_commit_dir incorrectly allows non-commit files")
        return False

    # Test 3a: Verify new directory checks
    if not is_in_compute_dir("src/lyra/llvm_backend/compute/driver.cpp"):
        print("SELF-TEST FAILED: is_in_compute_dir doesn't recognize compute files")
        return False

    if not is_in_layout_dir("src/lyra/llvm_backend/layout/layout.cpp"):
        print("SELF-TEST FAILED: is_in_layout_dir doesn't recognize layout files")
        return False

    if not is_in_type_ops_dir("src/lyra/llvm_backend/type_ops/dispatch.cpp"):
        print("SELF-TEST FAILED: is_in_type_ops_dir doesn't recognize type_ops files")
        return False

    if not is_in_instruction_dir("src/lyra/llvm_backend/instruction/assign.cpp"):
        print("SELF-TEST FAILED: is_in_instruction_dir doesn't recognize instruction files")
        return False

    # Test 3b: Verify context_*.cpp recognition
    if not is_context_impl("src/lyra/llvm_backend/context_runtime.cpp"):
        print("SELF-TEST FAILED: is_context_impl doesn't recognize context_runtime.cpp")
        return False

    if not is_context_impl("src/lyra/llvm_backend/context_place.cpp"):
        print("SELF-TEST FAILED: is_context_impl doesn't recognize context_place.cpp")
        return False

    # Test 4: WriteTarget definition detection
    if not is_writetarget_definition("struct WriteTarget {\n  llvm::Value* ptr;\n};"):
        print("SELF-TEST FAILED: WriteTarget definition not detected")
        return False

    if is_writetarget_definition("struct WriteTarget;"):  # Forward declaration
        print("SELF-TEST FAILED: Forward declaration incorrectly flagged as definition")
        return False

    # Test 5: GetPlacePointer pattern detection (compute/ boundary)
    test_code3 = "auto ptr = context.GetPlacePointer(place_id);"
    if ".GetPlacePointer(" not in test_code3:
        print("SELF-TEST FAILED: GetPlacePointer pattern doesn't match expected code")
        return False

    return True


def main() -> int:
    # Run self-tests first
    if not run_self_tests():
        return 1

    repo_root = Path(__file__).resolve().parent.parent.parent

    # Collect all errors
    store_errors = check_store_getter_boundaries(repo_root)
    commit_errors = check_commit_only_patterns(repo_root)
    isdesign_errors = check_isdesign_symbols(repo_root)
    writetarget_errors = check_write_target_definition(repo_root)
    compute_errors = check_compute_boundaries(repo_root)
    layout_errors = check_layout_boundaries(repo_root)
    instruction_errors = check_instruction_boundaries(repo_root)

    all_errors = (store_errors + commit_errors + isdesign_errors +
                  writetarget_errors + compute_errors + layout_errors +
                  instruction_errors)

    if all_errors:
        if store_errors:
            print("ERROR: Runtime getters used directly in type_ops/*.cpp (must go through commit/lifecycle):")
            print()
            for e in store_errors:
                print(e)
            print()
        if commit_errors:
            print("ERROR: Commit-internal method calls outside commit/:")
            print()
            for e in commit_errors:
                print(e)
            print()
        if isdesign_errors:
            print("ERROR: IsDesignSlot symbols outside commit/:")
            print()
            for e in isdesign_errors:
                print(e)
            print()
        if writetarget_errors:
            print("ERROR: WriteTarget definition outside commit/:")
            print()
            for e in writetarget_errors:
                print(e)
            print()
        if compute_errors:
            print("ERROR: compute/ module boundary violations (rvalue purity):")
            print()
            for e in compute_errors:
                print(e)
            print()
        if layout_errors:
            print("ERROR: layout/ module includes context.hpp (must only use llvm::LLVMContext&):")
            print()
            for e in layout_errors:
                print(e)
            print()
        if instruction_errors:
            print("ERROR: instruction/ module includes commit internals (should go through type_ops):")
            print()
            for e in instruction_errors:
                print(e)
            print()
        print("This script enforces LLVM backend module boundaries.")
        print("See tools/policy/check_llvm_backend_boundaries.py for details.")
        return 1

    print("OK: LLVM backend boundaries enforced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
