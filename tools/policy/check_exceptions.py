#!/usr/bin/env python3
"""Check exception policy compliance.

Rules:
  E001: No std::runtime_error or std::logic_error
  E002: No catch(...) except in driver
  E003: No throw except InternalError (rethrow 'throw;' also banned outside driver)

Usage:
  python3 tools/policy/check_exceptions.py                          # All files
  python3 tools/policy/check_exceptions.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_exceptions.py --staged                 # Staged files
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

INCLUDE_PATHS = ("src/lyra", "include/lyra")
CATCH_ALL_ALLOWLIST = ("src/lyra/driver",)
FULL_ALLOWLIST = frozenset({"include/lyra/common/internal_error.hpp"})
EXTENSIONS = frozenset({".cpp", ".hpp", ".cc", ".cxx",
                       ".h", ".hh", ".hxx", ".inl", ".ipp"})

# Compiled patterns
RE_BLOCK_COMMENT = re.compile(r'/\*.*?\*/', re.DOTALL)
RE_LINE_COMMENT = re.compile(r'//[^\n]*')
RE_STRING_LITERAL = re.compile(r'"(?:[^"\\]|\\.)*"')
RE_RAW_STRING = re.compile(r'R"([^(]*)\(.*?\)\1"', re.DOTALL)
RE_CHAR_LITERAL = re.compile(r"'(?:[^'\\]|\\.)*'")

RE_BANNED_EXCEPTION = re.compile(r'\bstd::(runtime_error|logic_error)\b')
RE_CATCH_ALL = re.compile(r'\bcatch\s*\(\s*\.\.\.\s*\)', re.DOTALL)
RE_THROW_STMT = re.compile(r'\bthrow\s+([^;]+);', re.DOTALL)
RE_RETHROW = re.compile(r'\bthrow\s*;')
RE_INTERNAL_ERROR = re.compile(r'\b(common::)?InternalError\b')


def get_repo_root() -> Path:
    result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True, text=True, check=True
    )
    return Path(result.stdout.strip())


def normalize_path(filepath: Path, repo_root: Path) -> str:
    """Normalize to repo-relative POSIX path."""
    try:
        resolved = filepath.resolve()
        return resolved.relative_to(repo_root.resolve()).as_posix()
    except ValueError:
        return filepath.as_posix()


def get_git_files(args: list[str]) -> list[str]:
    """Run git diff and return file list."""
    result = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=ACMRT"] + args,
        capture_output=True, text=True, check=True
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def get_all_files(repo_root: Path) -> list[str]:
    """Get all C++ files in include paths (single scan)."""
    files = []
    for include_path in INCLUDE_PATHS:
        path = repo_root / include_path
        if path.exists():
            for f in path.rglob("*"):
                if f.is_file() and f.suffix in EXTENSIONS:
                    files.append(normalize_path(f, repo_root))
    return files


def strip_comments_and_strings(content: str) -> str:
    """Remove comments, strings, and char literals to reduce false positives."""
    # Order matters: raw strings before regular strings
    content = RE_RAW_STRING.sub('""', content)
    content = RE_BLOCK_COMMENT.sub(' ', content)
    content = RE_LINE_COMMENT.sub(' ', content)
    content = RE_STRING_LITERAL.sub('""', content)
    content = RE_CHAR_LITERAL.sub("' '", content)
    return content


def offset_to_line(content: str, offset: int) -> int:
    """Convert character offset to 1-based line number."""
    return content[:offset].count('\n') + 1


def check_file(filepath: str, repo_root: Path) -> list[str]:
    """Check a single file for policy violations."""
    errors = []
    full_path = repo_root / filepath
    in_driver = any(filepath.startswith(a) for a in CATCH_ALL_ALLOWLIST)

    try:
        original = full_path.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    content = strip_comments_and_strings(original)

    # E001: No std::runtime_error or std::logic_error
    for match in RE_BANNED_EXCEPTION.finditer(content):
        line = offset_to_line(original, match.start())
        errors.append(
            f"{filepath}:{line}: E001 std::{match.group(1)} is banned")

    # E002: No catch(...) except in driver
    if not in_driver:
        for match in RE_CATCH_ALL.finditer(content):
            line = offset_to_line(original, match.start())
            errors.append(
                f"{filepath}:{line}: E002 catch(...) banned outside driver")

    # E003: No throw except InternalError
    for match in RE_THROW_STMT.finditer(content):
        thrown_expr = match.group(1).strip()
        if RE_INTERNAL_ERROR.search(thrown_expr):
            continue
        line = offset_to_line(original, match.start())
        snippet = thrown_expr.replace('\n', ' ')[:40]
        if len(thrown_expr) > 40:
            snippet += "..."
        errors.append(
            f"{filepath}:{line}: E003 throw {snippet} - use InternalError")

    # E003: Rethrow 'throw;' also banned outside driver
    if not in_driver:
        for match in RE_RETHROW.finditer(content):
            line = offset_to_line(original, match.start())
            errors.append(
                f"{filepath}:{line}: E003 rethrow banned outside driver")

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Check exception policy")
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

    # Filter to relevant paths/extensions, exclude allowlisted
    files = [
        f for f in files
        if any(f.startswith(inc) for inc in INCLUDE_PATHS)
        and Path(f).suffix in EXTENSIONS
        and f not in FULL_ALLOWLIST
    ]

    if not files:
        print("No files to check")
        return 0

    all_errors = []
    for filepath in sorted(set(files)):
        if (repo_root / filepath).exists():
            all_errors.extend(check_file(filepath, repo_root))

    if all_errors:
        print("Exception policy violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  E001: Use std::expected<T, Diagnostic> instead of std exceptions")
        print("  E002: catch(...) allowed only in src/lyra/driver/")
        print("  E003: Only throw common::InternalError for compiler bugs")
        return 1

    print(f"Checked {len(files)} files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
