#!/usr/bin/env python3
"""Check test file naming convention.

Rules:
  T001: YAML files under tests/sv_features/ must use directory-based naming.
        Valid filenames: default.yaml, four_state.yaml, two_state_only.yaml
        Old-style suffixes (*_four_state.yaml, *_two_state_only.yaml,
        *_two_state.yaml) are not allowed.

Usage:
  python3 tools/policy/check_test_naming.py                          # All files
  python3 tools/policy/check_test_naming.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_test_naming.py --staged                 # Staged files
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

VALID_STEMS = frozenset({"default", "four_state", "two_state_only"})
OLD_SUFFIX_PATTERN = re.compile(r"_(four_state|two_state_only|two_state)\.yaml$")
TEST_PREFIX = "tests/sv_features/"


def get_repo_root() -> Path:
    result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True, text=True, check=True
    )
    return Path(result.stdout.strip())


def get_git_files(args: list[str]) -> list[str]:
    result = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMRT"] + args,
        capture_output=True, text=True, check=True
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def get_all_files(repo_root: Path) -> list[str]:
    result = subprocess.run(
        ["git", "ls-files"],
        capture_output=True, text=True, check=True, cwd=repo_root
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def check_file(filepath: str) -> list[str]:
    errors = []
    name = Path(filepath).name
    stem = Path(filepath).stem

    if OLD_SUFFIX_PATTERN.search(name):
        errors.append(
            f"{filepath}: T001 old-style mode suffix in filename "
            f"(migrate to directory structure)")

    if name.endswith(".yaml") and stem not in VALID_STEMS:
        # Non-mode filename is fine (e.g., readmem_2state.yaml) as long as
        # it doesn't match the old suffix pattern (checked above).
        pass

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Check test naming convention")
    parser.add_argument(
        "--diff-base", help="Check files changed since git ref")
    parser.add_argument("--staged", action="store_true",
                        help="Check staged files")
    args = parser.parse_args()

    repo_root = get_repo_root()

    if args.staged:
        files = get_git_files(["--cached"])
    elif args.diff_base:
        files = get_git_files([args.diff_base])
    else:
        files = get_all_files(repo_root)

    # Filter to test YAML files
    files = [f for f in files if f.startswith(TEST_PREFIX) and f.endswith(".yaml")]

    if not files:
        print("No test files to check")
        return 0

    all_errors = []
    for filepath in sorted(set(files)):
        all_errors.extend(check_file(filepath))

    if all_errors:
        print("Test naming violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  T001: Use directory-based naming convention:")
        print("        feature/default.yaml, feature/four_state.yaml,")
        print("        feature/two_state_only.yaml")
        print("        Old suffixes (*_four_state.yaml, etc.) are not allowed")
        return 1

    print(f"Checked {len(files)} test files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
