#!/usr/bin/env python3
"""C++ style policy.

Style rules sit here, separate from architecture rules. Both scripts run
under the central C++ Style/Policy CI step.

Rules:

  S001  No /*name=*/value parameter labels at call sites and no /*name*/
        commented-out unused parameter names. IDE inlay hints render
        parameter names; both forms rot when the parameter is renamed.
        Scope: every .cpp/.hpp under src/, include/, tests/.

  S002  No header files (.hpp/.h/.hh/.hxx) anywhere under src/. Headers
        live in include/, sources live in src/. The two trees mirror
        each other's structure.
        Scope: every header file under src/.

  S003  No migration-progress / development-stage language in comments
        or string literals. Source must describe its current semantics,
        never the process by which it was written or its development
        cadence. Forbidden phrases include "in this cut", "this cut",
        "future cut", "later cut", "next cut", "previous cut", "Cut N",
        "sub-commit N", "out of scope", "out-of-scope", "deferred to a
        later cut". User-visible "not yet supported" wording is the
        correct replacement. Scope: every .cpp/.hpp under src/, include/.

  S004  Include style: C++ stdlib headers must use angle brackets
        (`#include <vector>`), and lyra project headers must use double
        quotes (`#include "lyra/..."`). 3rd-party libraries follow their
        upstream convention (e.g. fmt uses `<fmt/...>`, absl uses
        `"absl/..."` because of Bazel's `-iquote` exposure) and are not
        policed by this rule.
        Scope: every .cpp/.hpp under src/, include/, tests/.

Usage:
  python3 tools/policy/check_cpp_style.py
"""

import re
import sys
from pathlib import Path

EXTENSIONS = frozenset({".cpp", ".hpp", ".cc", ".cxx", ".h", ".hh"})
HEADER_EXTENSIONS = frozenset({".hpp", ".h", ".hh", ".hxx"})
SKIP_PREFIXES = ("archived/", "external/", "bazel-")

# Match /*name=*/ (parameter label) and /*name*/ (unused-parameter comment).
PARAM_COMMENT_PATTERN = re.compile(r"/\*[A-Za-z_][A-Za-z0-9_]*=?\*/")

# Forbidden migration-progress / development-stage phrases.
#
# `(?i)` -> case-insensitive. `\b` boundaries make the matches token-aware
# so unrelated words ("scope" alone, "cut" alone) do not trip. The phrases
# here are the ones called out in CLAUDE.local.md.
STAGE_LANGUAGE_PATTERNS = [
    (re.compile(r"(?i)\bin\s+this\s+cut\b"), "in this cut"),
    (re.compile(r"(?i)\bfor\s+this\s+cut\b"), "for this cut"),
    (re.compile(r"(?i)\bthis\s+cut\b"), "this cut"),
    (re.compile(r"(?i)\b(?:future|next|later|previous|earlier)\s+cuts?\b"),
     "future/later/next/previous/earlier cut"),
    (re.compile(r"(?i)\bcut\s+\d+\b"), "Cut N"),
    (re.compile(r"(?i)\bsub-?commit\s+\d+\b"), "sub-commit N"),
    (re.compile(r"(?i)\bout[\s-]of[\s-]scope\b"), "out of scope"),
    (re.compile(r"(?i)\bdeferred\s+to\s+a\s+(?:later|future)\b"),
     "deferred to a later/future"),
]

# S004: C++ stdlib headers (always angle-bracket form). This list is the
# universal portion of the standard library; we intentionally do not police
# 3rd-party libraries because each tracks its upstream convention (e.g.
# bazel-imported absl uses `"absl/..."` per its `-iquote` exposure, while
# fmt uses `<fmt/...>`).
STDLIB_HEADERS = frozenset({
    "algorithm", "array", "atomic", "bit", "bitset", "cassert", "ccomplex",
    "cctype", "cerrno", "cfenv", "cfloat", "charconv", "chrono", "cinttypes",
    "climits", "clocale", "cmath", "compare", "complex", "concepts",
    "condition_variable", "coroutine", "csetjmp", "csignal", "cstdarg",
    "cstdbool", "cstddef", "cstdint", "cstdio", "cstdlib", "cstring", "ctime",
    "cuchar", "cwchar", "cwctype", "deque", "exception", "execution",
    "expected", "filesystem", "format", "forward_list", "fstream",
    "functional", "future", "initializer_list", "iomanip", "ios", "iosfwd",
    "iostream", "istream", "iterator", "latch", "limits", "list", "locale",
    "map", "memory", "memory_resource", "mutex", "new", "numbers", "numeric",
    "optional", "ostream", "queue", "random", "ranges", "ratio", "regex",
    "scoped_allocator", "semaphore", "set", "shared_mutex", "source_location",
    "span", "sstream", "stack", "stdexcept", "stop_token", "streambuf",
    "string", "string_view", "syncstream", "system_error", "thread", "tuple",
    "type_traits", "typeindex", "typeinfo", "unordered_map", "unordered_set",
    "utility", "valarray", "variant", "vector", "version",
})

INCLUDE_QUOTE_PATTERN = re.compile(r'^\s*#\s*include\s*"([^"]+)"')
INCLUDE_ANGLE_PATTERN = re.compile(r'^\s*#\s*include\s*<([^>]+)>')


def iter_cpp_files(repo_root: Path, roots=("src", "include", "tests")):
    for root in roots:
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


def check_s002(repo_root: Path) -> list[str]:
    errors = []
    src_root = repo_root / "src"
    if not src_root.exists():
        return errors
    for path in src_root.rglob("*"):
        if not path.is_file():
            continue
        if path.suffix not in HEADER_EXTENSIONS:
            continue
        rel = path.relative_to(repo_root).as_posix()
        if any(rel.startswith(p) for p in SKIP_PREFIXES):
            continue
        errors.append(
            f"  {rel}: S002 header file under src/; move to include/"
        )
    return errors


def check_s003(repo_root: Path) -> list[str]:
    """Forbid migration-progress language in src/ and include/.

    Tests are excluded: golden expectations may legitimately mention these
    phrases when they appear in test fixtures.
    """
    errors = []
    seen_per_line = set()
    for path, rel in iter_cpp_files(repo_root, roots=("src", "include")):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            for pattern, label in STAGE_LANGUAGE_PATTERNS:
                m = pattern.search(line)
                if m is None:
                    continue
                key = (rel, lineno, label)
                if key in seen_per_line:
                    continue
                seen_per_line.add(key)
                errors.append(
                    f"  {rel}:{lineno}: S003 stage-language phrase '{m.group(0)}' "
                    f"(forbidden: {label})"
                )
    return errors


def check_s004(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_cpp_files(repo_root):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            if (m := INCLUDE_QUOTE_PATTERN.match(line)):
                inc = m.group(1)
                if inc in STDLIB_HEADERS:
                    errors.append(
                        f"  {rel}:{lineno}: S004 stdlib header "
                        f'"{inc}" must use angle brackets'
                    )
            elif (m := INCLUDE_ANGLE_PATTERN.match(line)):
                inc = m.group(1)
                if inc.startswith("lyra/"):
                    errors.append(
                        f"  {rel}:{lineno}: S004 lyra header <{inc}> "
                        f'must use double quotes'
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

    def hits(text):
        return any(p.search(text) for p, _ in STAGE_LANGUAGE_PATTERNS)

    ok &= expect(hits("not supported in this cut"), "S003 'in this cut'")
    ok &= expect(hits("for this cut: ..."), "S003 'for this cut'")
    ok &= expect(hits("a future cut may"), "S003 'future cut'")
    ok &= expect(hits("// later cuts extend this"), "S003 'later cuts'")
    ok &= expect(hits("// Cut 1 materializes only X"), "S003 'Cut 1'")
    ok &= expect(hits("// sub-commit 6 flips the allocator"),
                 "S003 'sub-commit 6'")
    ok &= expect(hits("is out of scope"), "S003 'out of scope'")
    ok &= expect(hits("// out-of-scope"), "S003 'out-of-scope'")
    ok &= expect(hits("// deferred to a later cut"),
                 "S003 'deferred to a later cut'")
    ok &= expect(not hits("// describes a scope"), "S003 'scope' unrelated")
    ok &= expect(not hits("scope is the lookup unit"),
                 "S003 'scope' unrelated bare")
    ok &= expect(not hits("// to cut the loop short"), "S003 'cut' unrelated")

    def quote_inc(text):
        m = INCLUDE_QUOTE_PATTERN.match(text)
        return m.group(1) if m else None

    def angle_inc(text):
        m = INCLUDE_ANGLE_PATTERN.match(text)
        return m.group(1) if m else None

    ok &= expect(quote_inc('#include "vector"') == "vector", "S004 quote")
    ok &= expect(angle_inc("#include <vector>") == "vector", "S004 angle")
    ok &= expect("vector" in STDLIB_HEADERS, "S004 vector is stdlib")
    ok &= expect("absl/x.h" not in STDLIB_HEADERS, "S004 absl not stdlib")
    return ok


CHECKS = [
    ("S001 block-comment parameter labels / unused-parameter comments",
     check_s001),
    ("S002 header files under src/", check_s002),
    ("S003 migration-progress / development-stage language", check_s003),
    ("S004 include style (stdlib <>, lyra \"\")", check_s004),
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
