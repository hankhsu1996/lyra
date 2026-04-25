#!/usr/bin/env python3
"""C++ style policy.

Style rules sit here, separate from architecture rules. Both scripts run
under the central C++ Style/Policy CI step.

Rules:

  S001  No /*name=*/value parameter labels at call sites and no /*name*/
        commented-out unused parameter names. IDE inlay hints render
        parameter names; both forms rot when the parameter is renamed.
        Scope: every .cpp/.hpp under src/, include/, tests/.

Usage:
  python3 tools/policy/check_cpp_style.py
"""

import re
import sys
from pathlib import Path

EXTENSIONS = frozenset({".cpp", ".hpp", ".cc", ".cxx", ".h", ".hh"})
SKIP_PREFIXES = ("archived/", "external/", "bazel-")

# Match /*name=*/ (parameter label) and /*name*/ (unused-parameter comment).
PARAM_COMMENT_PATTERN = re.compile(r"/\*[A-Za-z_][A-Za-z0-9_]*=?\*/")


def iter_cpp_files(repo_root: Path):
    for root in ("src", "include", "tests"):
        base = repo_root / root
        if not base.exists():
            continue
        for ext in ("*.cpp", "*.hpp"):
            for path in base.rglob(ext):
                rel = path.relative_to(repo_root).as_posix()
                if any(rel.startswith(p) for p in SKIP_PREFIXES):
                    continue
                yield path, rel


def check_s001(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_cpp_files(repo_root):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            for m in PARAM_COMMENT_PATTERN.finditer(line):
                errors.append(
                    f"  {rel}:{lineno}: S001 parameter comment '{m.group(0)}'"
                )
    return errors


def run_self_tests() -> bool:
    def expect(cond, msg):
        if not cond:
            print(f"SELF-TEST FAILED: {msg}")
            return False
        return True

    ok = True
    ok &= expect(PARAM_COMMENT_PATTERN.search("foo(/*x=*/1);"), "S001 label")
    ok &= expect(PARAM_COMMENT_PATTERN.search("void f(int /*x*/);"),
                 "S001 unused")
    ok &= expect(not PARAM_COMMENT_PATTERN.search("/* explanatory */"),
                 "S001 prose")
    ok &= expect(not PARAM_COMMENT_PATTERN.search("/* a b */"),
                 "S001 multi-token")
    return ok


CHECKS = [
    ("S001 block-comment parameter labels / unused-parameter comments",
     check_s001),
]


def main() -> int:
    if not run_self_tests():
        return 1

    repo_root = Path(__file__).resolve().parent.parent.parent

    failed = False
    for label, fn in CHECKS:
        errors = fn(repo_root)
        if errors:
            failed = True
            print(f"ERROR: {label}:")
            for e in errors:
                print(e)
            print()

    if failed:
        print("See tools/policy/check_cpp_style.py for rule definitions.")
        return 1

    print("OK: C++ style enforced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
