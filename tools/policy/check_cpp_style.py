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
        upstream convention (e.g. fmt uses `<fmt/...>`) and are not
        policed by this rule.
        Scope: every .cpp/.hpp under src/, include/, tests/.

  S005  No cast-to-void of a bare name as a statement -- `(void)x;` or
        `static_cast<void>(x);` where `x` is a plain identifier. This is the
        unused-parameter / unused-variable suppression hack. The correct fix
        is to drop the parameter, leave it unnamed (when an interface mandates
        its presence), or remove the dead variable -- never silence the
        warning. Intentional discard of a call's return value (`(void)f();`)
        is spared: the identifier char class stops at `(`, so anything with a
        call does not match.
        Scope: every .cpp/.hpp under src/, include/, tests/.

  S006  No `[[maybe_unused]]` on a parameter or variable. Like `(void)x;`, it
        keeps something unused and silences the warning instead of fixing the
        cause. Remove the parameter (narrow the signature so the caller passes
        only what is used), or for a parameter an external interface mandates
        but the body cannot use, leave it unnamed (with a single-line
        `// NOLINT(readability-named-parameter)` only if that check then
        objects).
        Scope: every .cpp/.hpp under src/, include/, tests/.

  S007  No project-documentation references in comments. A comment that names
        a `docs/` path, a project `.md` file (architecture / decision /
        progress / glossary), or a numbered "invariant N" couples the code to
        a document that renames and rots. The LRM (IEEE 1800 SystemVerilog
        spec) is the one allowed external citation, and it never appears as a
        `.md` file -- so seeing a `.md` reference is enough to know the comment
        cites a project doc. State the stable contract inline instead.
        Scope: every .cpp/.hpp under src/, include/.

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

# S005: cast-to-void of a bare name as a statement. A member/scope chain
# (`a.b`, `a->b`, `ns::x`) is still a bare name; a call (`f(...)`) is not,
# because the identifier char class never crosses a `(`.
_VOID_NAME = r"[A-Za-z_]\w*(?:\s*(?:\.|->|::)\s*[A-Za-z_]\w*)*"
VOID_DISCARD_PATTERNS = [
    re.compile(r"\(\s*void\s*\)\s*" + _VOID_NAME + r"\s*;"),
    re.compile(r"static_cast\s*<\s*void\s*>\s*\(\s*" + _VOID_NAME + r"\s*\)\s*;"),
]

# S006: the `[[maybe_unused]]` attribute. Same smell as S005 -- it keeps an
# unused parameter/variable and silences the warning instead of removing it.
MAYBE_UNUSED_PATTERN = re.compile(r"\[\[\s*maybe_unused\s*\]\]")

# S007: references to project documentation. A `docs/` path, any project `.md`
# filename, or a numbered "invariant N" all point at a document that renames
# and rots. The LRM is cited as "LRM <section>" and matches none of these, so
# it stays the one allowed external citation. The patterns are deliberately
# generic: any `.md` mention is taken as a doc reference rather than enumerating
# specific doc names (which would themselves rot).
DOC_REFERENCE_PATTERNS = [
    (re.compile(r"\bdocs/"), "docs/ path"),
    (re.compile(r"\b[\w-]+\.md\b"), ".md document"),
    (re.compile(r"(?i)\binvariant\s+\d+\b"), "numbered invariant"),
]


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


def check_s005(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_cpp_files(repo_root):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            for pattern in VOID_DISCARD_PATTERNS:
                if (m := pattern.search(line)):
                    errors.append(
                        f"  {rel}:{lineno}: S005 cast-to-void '{m.group(0)}' "
                        f"suppresses an unused-parameter/variable warning; "
                        f"drop or unname the parameter instead"
                    )
                    break
    return errors


def check_s006(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_cpp_files(repo_root):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            if MAYBE_UNUSED_PATTERN.search(line):
                errors.append(
                    f"  {rel}:{lineno}: S006 [[maybe_unused]] keeps an unused "
                    f"parameter/variable; remove it or leave the parameter "
                    f"unnamed instead"
                )
    return errors


def check_s007(repo_root: Path) -> list[str]:
    """Forbid project-documentation references in src/ and include/ comments.

    Tests are excluded for the same reason as S003: fixtures may legitimately
    mention these tokens.
    """
    errors = []
    seen_per_line = set()
    for path, rel in iter_cpp_files(repo_root, roots=("src", "include")):
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            for pattern, label in DOC_REFERENCE_PATTERNS:
                m = pattern.search(line)
                if m is None:
                    continue
                key = (rel, lineno, label)
                if key in seen_per_line:
                    continue
                seen_per_line.add(key)
                errors.append(
                    f"  {rel}:{lineno}: S007 comment references project "
                    f"documentation '{m.group(0)}' ({label}); source comments "
                    f"cite only the LRM, never a project doc"
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
    ok &= expect("fmt/core.h" not in STDLIB_HEADERS, "S004 fmt not stdlib")

    def void_hit(text):
        return any(p.search(text) for p in VOID_DISCARD_PATTERNS)

    ok &= expect(void_hit("(void)handle;"), "S005 (void)name")
    ok &= expect(void_hit("    (void)call_span;"), "S005 indented")
    ok &= expect(void_hit("(void)foo.bar;"), "S005 member chain")
    ok &= expect(void_hit("(void)ns::x;"), "S005 scope chain")
    ok &= expect(void_hit("static_cast<void>(x);"), "S005 static_cast name")
    ok &= expect(not void_hit("(void)RunProcess(args);"), "S005 spares call")
    ok &= expect(not void_hit("(void)node.as<std::int64_t>();"),
                 "S005 spares templated call")
    ok &= expect(not void_hit("static_cast<void>(f());"),
                 "S005 spares static_cast call")

    ok &= expect(MAYBE_UNUSED_PATTERN.search("[[maybe_unused]] int x"),
                 "S006 attribute")
    ok &= expect(not MAYBE_UNUSED_PATTERN.search("int x = 0;"),
                 "S006 unrelated")

    def doc_hits(text):
        return any(p.search(text) for p, _ in DOC_REFERENCE_PATTERNS)

    ok &= expect(doc_hits("see decisions/unpacked-struct-representation.md"),
                 "S007 .md reference")
    ok &= expect(doc_hits("docs/architecture/mir.md"), "S007 docs/ path")
    ok &= expect(doc_hits("the model mir.md invariant 11 requires"),
                 "S007 .md plus invariant")
    ok &= expect(doc_hits("restores invariant 7"), "S007 numbered invariant")
    ok &= expect(not doc_hits("LRM 21.3.4.3 wildcard equality"),
                 "S007 spares LRM citation")
    ok &= expect(not doc_hits("maintains the invariant that x > 0"),
                 "S007 spares unnumbered invariant")
    ok &= expect(not doc_hits("the explicit-receiver model"),
                 "S007 spares plain prose")
    return ok


CHECKS = [
    ("S001 block-comment parameter labels / unused-parameter comments",
     check_s001),
    ("S002 header files under src/", check_s002),
    ("S003 migration-progress / development-stage language", check_s003),
    ("S004 include style (stdlib <>, lyra \"\")", check_s004),
    ("S005 cast-to-void unused-parameter/variable suppression", check_s005),
    ("S006 [[maybe_unused]] suppression", check_s006),
    ("S007 project-documentation references in comments", check_s007),
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
        print(
            "These are project C++ comment and style conventions. Read the "
            "failing rule's definition at the top of this file to understand "
            "the convention, then fix the code to satisfy it -- do not work "
            "around the check."
        )
        return 1

    print("OK: C++ style enforced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
