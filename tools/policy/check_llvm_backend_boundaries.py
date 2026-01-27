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

Usage:
  python3 tools/policy/check_llvm_backend_boundaries.py
"""

import re
import sys
from pathlib import Path

COMMIT_DIR = "src/lyra/llvm_backend/commit/"
COMMIT_INCLUDE_DIR = "include/lyra/llvm_backend/commit/"
LIFECYCLE_DIR = "src/lyra/llvm_backend/lifecycle/"

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


def is_context_impl(rel_path: str) -> bool:
    """Check if this is context.cpp (where private methods are implemented)."""
    return rel_path == "src/lyra/llvm_backend/context.cpp"


def check_store_getter_boundaries(repo_root: Path) -> list[str]:
    """Check that store/lifecycle getters in type_ops files go through commit/lifecycle.

    Scope: type_ops_*.cpp files only (the main handler files).
    Other files (context.cpp defines getters, instruction handlers have legitimate uses)
    are not checked here.
    """
    llvm_backend_dir = repo_root / "src/lyra/llvm_backend"
    type_ops_files = list(llvm_backend_dir.glob("type_ops_*.cpp"))
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

    if is_in_commit_dir("src/lyra/llvm_backend/type_ops_struct.cpp"):
        print("SELF-TEST FAILED: is_in_commit_dir incorrectly allows non-commit files")
        return False

    # Test 4: WriteTarget definition detection
    if not is_writetarget_definition("struct WriteTarget {\n  llvm::Value* ptr;\n};"):
        print("SELF-TEST FAILED: WriteTarget definition not detected")
        return False

    if is_writetarget_definition("struct WriteTarget;"):  # Forward declaration
        print("SELF-TEST FAILED: Forward declaration incorrectly flagged as definition")
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

    all_errors = store_errors + commit_errors + isdesign_errors + writetarget_errors

    if all_errors:
        if store_errors:
            print("ERROR: Runtime getters used directly in type_ops_*.cpp (must go through commit/lifecycle):")
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
        print("This script enforces LLVM backend module boundaries.")
        print("Store getters and commit-internal symbols must stay in commit/.")
        return 1

    print("OK: LLVM backend boundaries enforced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
