#!/usr/bin/env python3
"""Check ASCII-only policy compliance.

Rules:
  A001: No non-ASCII characters in text files

This enforces ASCII-only content to avoid encoding issues and ensure
consistency (no Unicode arrows, emojis, curly quotes, etc.).

Usage:
  python3 tools/policy/check_ascii.py                          # All files
  python3 tools/policy/check_ascii.py --diff-base origin/main  # Changed files
  python3 tools/policy/check_ascii.py --staged                 # Staged files
"""

import argparse
import subprocess
import sys
from pathlib import Path

EXTENSIONS = frozenset({
    ".cpp", ".hpp", ".cc", ".cxx", ".h", ".hh", ".hxx", ".inl", ".ipp",
    ".py",
    ".md",
    ".yaml", ".yml",
})

# Files without extensions that should be checked
SPECIAL_FILES = frozenset({"BUILD", "BUILD.bazel", "WORKSPACE", "MODULE.bazel"})


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
    """Get all tracked files matching our criteria."""
    result = subprocess.run(
        ["git", "ls-files"],
        capture_output=True, text=True, check=True, cwd=repo_root
    )
    return [f for f in result.stdout.strip().split("\n") if f]


def should_check_file(filepath: str) -> bool:
    """Check if file should be scanned for ASCII compliance."""
    path = Path(filepath)
    if path.suffix in EXTENSIONS:
        return True
    if path.name in SPECIAL_FILES:
        return True
    return False


def check_file(filepath: str, repo_root: Path) -> list[str]:
    """Check a single file for non-ASCII characters."""
    errors = []
    full_path = repo_root / filepath

    try:
        content = full_path.read_bytes()
    except OSError as e:
        return [f"{filepath}: failed to read: {e}"]

    lines = content.split(b'\n')
    for lineno, line in enumerate(lines, 1):
        for col, byte in enumerate(line, 1):
            if byte > 127:
                # Try to decode the character for display
                try:
                    # Find the full UTF-8 sequence
                    char_bytes = bytes([byte])
                    remaining = line[col:]
                    for i in range(min(3, len(remaining))):
                        if remaining[i] & 0xC0 == 0x80:
                            char_bytes += bytes([remaining[i]])
                        else:
                            break
                    char = char_bytes.decode('utf-8', errors='replace')
                except Exception:
                    char = '?'
                errors.append(
                    f"{filepath}:{lineno}:{col}: A001 non-ASCII character '{char}' (0x{byte:02x})")
                break  # Only report first non-ASCII per line

    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description="Check ASCII-only policy")
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

    # Filter to relevant files
    files = [f for f in files if should_check_file(f)]

    if not files:
        print("No files to check")
        return 0

    all_errors = []
    for filepath in sorted(set(files)):
        if (repo_root / filepath).exists():
            all_errors.extend(check_file(filepath, repo_root))

    if all_errors:
        print("ASCII policy violations:\n")
        for error in all_errors:
            print(f"  {error}")
        print(f"\nTotal: {len(all_errors)} violations")
        print("\nRules:")
        print("  A001: Only ASCII characters (0x00-0x7F) allowed in text files")
        print("        No Unicode arrows, emojis, curly quotes, etc.")
        return 1

    print(f"Checked {len(files)} files, no violations")
    return 0


if __name__ == "__main__":
    sys.exit(main())
