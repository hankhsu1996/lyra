#!/usr/bin/env python3
"""Check that type_ops handler files follow commit-layer conventions.

This script enforces the rule: runtime getter calls in type_ops_*.cpp handler
files must go through the commit module (src/lyra/llvm_backend/commit/), with
explicit per-pattern exceptions for legitimate Tier-2 design choices.

Scope: type_ops_*.cpp files only. Other files (instruction handlers, lower.cpp)
have legitimate direct runtime calls for different purposes and are not checked.

Usage:
  python3 tools/policy/check_runtime_layer.py
"""

import sys
from pathlib import Path

COMMIT_LAYER = "src/lyra/llvm_backend/commit/"

ALLOWED = {
    "GetLyraStorePacked": {
        f"{COMMIT_LAYER}commit_packed.cpp",
        f"{COMMIT_LAYER}commit_notify.cpp",
    },
    "GetLyraStoreDynArray": {
        f"{COMMIT_LAYER}commit_container.cpp",
        f"{COMMIT_LAYER}commit_notify.cpp",
    },
    "GetLyraStoreString": {
        f"{COMMIT_LAYER}commit_string.cpp",
    },
    # Lifecycle patterns - still go through lifecycle module, not commit
    "GetLyraStringRetain": {
        "src/lyra/llvm_backend/lifecycle/lifecycle_string.cpp",
    },
    "GetLyraStringRelease": {
        f"{COMMIT_LAYER}commit_string.cpp",
        "src/lyra/llvm_backend/lifecycle/lifecycle_string.cpp",
    },
    "GetLyraDynArrayClone": {
        "src/lyra/llvm_backend/lifecycle/lifecycle_container.cpp",
    },
    "GetLyraDynArrayRelease": {
        f"{COMMIT_LAYER}commit_container.cpp",
        "src/lyra/llvm_backend/lifecycle/lifecycle_container.cpp",
    },
    "GetLyraDynArrayDestroy": {
        "src/lyra/llvm_backend/lifecycle/lifecycle_container.cpp",
    },
}


def strip_comment(line: str) -> str:
    """Remove // comments from a line (simple heuristic, not a full parser)."""
    idx = line.find("//")
    if idx != -1:
        return line[:idx]
    return line


def main() -> int:
    repo_root = Path(__file__).resolve().parent.parent.parent
    type_ops_dir = repo_root / "src/lyra/llvm_backend"

    type_ops_files = list(type_ops_dir.glob("type_ops_*.cpp"))
    if not type_ops_files:
        print("No type_ops_*.cpp files found")
        return 1

    errors = []
    for filepath in type_ops_files:
        rel_path = filepath.relative_to(repo_root).as_posix()

        content = filepath.read_text()
        lines = content.split('\n')
        for lineno, line in enumerate(lines, 1):
            code = strip_comment(line)
            for pattern, allowed_files in ALLOWED.items():
                if pattern in code and rel_path not in allowed_files:
                    errors.append(f"  {rel_path}:{lineno}: {pattern}")

    if errors:
        print("ERROR: Runtime getter calls in type_ops handler files outside allowed locations:")
        print()
        for e in errors:
            print(e)
        print()
        print("This script enforces commit-layer conventions for type_ops_*.cpp files.")
        print("Runtime getters should go through commit/ module helpers.")
        print()
        print("If this is a legitimate Tier-2 exception, add it to ALLOWED in:")
        print(f"  {Path(__file__).relative_to(repo_root)}")
        return 1

    print(f"OK: Checked {len(type_ops_files)} type_ops files, commit layer conventions satisfied")
    return 0


if __name__ == "__main__":
    sys.exit(main())
