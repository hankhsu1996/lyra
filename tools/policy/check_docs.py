#!/usr/bin/env python3
"""Documentation policy enforcement.

Prose carries no enforcement of its own: a doc naming a directory stays
green when the directory is deleted, so the claim survives as a lie until
someone reads it and tries it. These rules cover the subset of doc claims a
machine can settle, which is the subset that should never have needed a
reader in the first place. What is left over -- whether a contract is the
right contract, whether a stated capability is real -- is the part worth a
reader's time.

Rules:

  D001  A repo-rooted path cited in a doc must exist.
        Scope: every tracked markdown file.
        A path counts as repo-rooted when its first component is a
        directory at the repo root, which is what distinguishes
        `tests/cases/` from a bare `dispatch.hpp` named relative to
        whatever directory the surrounding prose is discussing.

  D002  A relative markdown link must resolve.
        Scope: every tracked markdown file, outside fenced code.

  D003  No migration-cadence vocabulary in the timeless tiers.
        Scope: docs/architecture/**, docs/glossary/**, top-level docs/*.md,
               README.md, CLAUDE.md.
        An architecture contract states what holds, not when it started
        holding; "in this cut" has no referent once the cut is history.
        Cadence belongs in docs/progress/, which exists to carry it, and
        in docs/decisions/, which records a decision at a point in time.
        Text inside code spans is exempt: a quoted counter-example is not
        an assertion.

  D004  An index must list every document in the directory it indexes.
        Scope: the INDEXES table below.
        An index that silently omits an entry is worse than no index --
        it reads as complete.

Usage:
  python3 tools/policy/check_docs.py
"""

import re
import sys
from pathlib import Path

SKIP_PREFIXES = (
    "archived/", "external/", "bazel-", "node_modules/",
    # Slash-command definitions for the coding assistant, not project docs.
    ".claude/",
)

# Per-workspace scratch, git-ignored, and free to name anything.
SKIP_SUFFIXES = (".local.md",)

# Git-ignored working directories a doc may legitimately name even though
# a fresh clone has none of them.
UNTRACKED_ROOTS = frozenset({"playground", "obj_dir", "build", "out"})

# --- Rule D002 -----------------------------------------------------------
FENCE_PATTERN = re.compile(r"^```.*?^```", re.MULTILINE | re.DOTALL)
INLINE_CODE_PATTERN = re.compile(r"`[^`\n]*`")
LINK_PATTERN = re.compile(r"\[[^\]]*\]\(([^)]+)\)")

# A markdown link target is a path or a URL. Anything carrying whitespace or
# C++ punctuation came from prose or a code sample that merely looks like a
# link, and is not a claim about the filesystem.
NON_PATH_CHARS = re.compile(r"[\s<>&*(),;]")

# A bare identifier is not a path either. Unfenced pseudocode supplies the
# rest of the link-shaped noise -- `[N](src)`, `[cap1 = ...](closure_params)`
# -- and every real relative link in this repo names a directory or a file
# with an extension.
PATH_SHAPED = re.compile(r"/|\.[A-Za-z0-9]+$")

# --- Rule D001 -----------------------------------------------------------
CITED_PATH_PATTERN = re.compile(r"`([A-Za-z0-9_.][A-Za-z0-9_./-]*)`")

# --- Rule D003 -----------------------------------------------------------
# "that" is deliberately absent: it reads as a relative pronoun before a
# verb ("a design that cuts one stage"), which is ordinary prose.
CADENCE_PATTERN = re.compile(
    r"\b(?:this|next|later|earlier|previous|following)\s+cuts?\b",
    re.IGNORECASE,
)
TIMELESS_DIRS = ("docs/architecture/", "docs/glossary/")
TIMELESS_FILES = ("README.md", "CLAUDE.md")

# --- Rule D004 -----------------------------------------------------------
# (index file, indexed directory). An index not listed here makes no
# completeness claim -- docs/progress/README.md deliberately lists nothing,
# because its directory turns over every PR.
INDEXES = (
    ("docs/decisions/README.md", "docs/decisions"),
    ("docs/architecture/README.md", "docs/architecture"),
)


def strip_fenced(text: str) -> str:
    return FENCE_PATTERN.sub("", text)


def strip_inline_code(text: str) -> str:
    return INLINE_CODE_PATTERN.sub("", text)


def iter_docs(repo_root: Path):
    for path in sorted(repo_root.rglob("*.md")):
        rel = path.relative_to(repo_root).as_posix()
        if any(rel.startswith(p) for p in SKIP_PREFIXES):
            continue
        if any(rel.endswith(s) for s in SKIP_SUFFIXES):
            continue
        yield path, rel


def repo_top_level_dirs(repo_root: Path) -> frozenset[str]:
    return frozenset(
        p.name for p in repo_root.iterdir()
        if p.is_dir() and not p.name.startswith((".", "bazel-"))
    ) | {".github"}


def line_of(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


# --- Checks --------------------------------------------------------------

def check_d001(repo_root: Path) -> list[str]:
    tops = repo_top_level_dirs(repo_root)
    errors = []
    for path, rel in iter_docs(repo_root):
        text = path.read_text()
        for m in CITED_PATH_PATTERN.finditer(text):
            cited = m.group(1)
            head = cited.split("/", 1)[0]
            if "/" not in cited or head not in tops:
                continue
            if head in UNTRACKED_ROOTS:
                continue
            if (repo_root / cited).exists():
                continue
            # A trailing-glob or directory form is cited as a family.
            trimmed = cited.rstrip("/")
            if (repo_root / trimmed).exists():
                continue
            errors.append(
                f"  {rel}:{line_of(text, m.start())}: D001 cites "
                f"'{cited}', which does not exist"
            )
    return errors


def check_d002(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_docs(repo_root):
        text = path.read_text()
        body = strip_fenced(text)
        for m in LINK_PATTERN.finditer(body):
            target = m.group(1)
            if target.startswith(("http://", "https://", "#", "mailto:")):
                continue
            if NON_PATH_CHARS.search(target):
                continue
            target = target.split("#", 1)[0]
            if not target or not PATH_SHAPED.search(target):
                continue
            if (path.parent / target).exists():
                continue
            errors.append(
                f"  {rel}: D002 link to '{target}' does not resolve"
            )
    return errors


def check_d003(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_docs(repo_root):
        timeless = (
            rel.startswith(TIMELESS_DIRS)
            or rel in TIMELESS_FILES
            or (rel.startswith("docs/") and rel.count("/") == 1)
        )
        if not timeless:
            continue
        body = strip_inline_code(path.read_text())
        for m in CADENCE_PATTERN.finditer(body):
            errors.append(
                f"  {rel}:{line_of(body, m.start())}: D003 cadence "
                f"vocabulary '{m.group(0).strip()}'; state what holds, and "
                f"leave when it started holding to git"
            )
    return errors


def check_d004(repo_root: Path) -> list[str]:
    errors = []
    for index_rel, dir_rel in INDEXES:
        index_path = repo_root / index_rel
        dir_path = repo_root / dir_rel
        if not index_path.exists() or not dir_path.is_dir():
            errors.append(f"  D004 index '{index_rel}' or '{dir_rel}' missing")
            continue
        index_text = index_path.read_text()
        for entry in sorted(dir_path.glob("*.md")):
            if entry.name == "README.md":
                continue
            if entry.name not in index_text:
                errors.append(
                    f"  {index_rel}: D004 does not list "
                    f"'{dir_rel}/{entry.name}'"
                )
    return errors


# --- Self-tests ----------------------------------------------------------

def run_self_tests() -> bool:
    def expect(cond, msg):
        if not cond:
            print(f"SELF-TEST FAILED: {msg}")
            return False
        return True

    ok = True

    # strip_fenced removes the code samples that look like links.
    fenced = "before\n```text\n[N](src)\n```\nafter [a](b.md)\n"
    stripped = strip_fenced(fenced)
    ok &= expect("[N](src)" not in stripped, "strip_fenced drops fenced body")
    ok &= expect("[a](b.md)" in stripped, "strip_fenced keeps prose")

    # strip_inline_code exempts a quoted counter-example.
    quoted = "Forbidden: `// later cuts will extend this`. Comments are timeless."
    ok &= expect(
        not CADENCE_PATTERN.search(strip_inline_code(quoted)),
        "D003 exempts a cadence phrase quoted as a counter-example")
    ok &= expect(
        CADENCE_PATTERN.search("In this cut a StructType is generated.")
        is not None,
        "D003 detects 'In this cut'")
    ok &= expect(
        CADENCE_PATTERN.search("a slot in a later cut.") is not None,
        "D003 detects 'a later cut'")
    ok &= expect(
        CADENCE_PATTERN.search("the receiver in this cut is a borrow")
        is not None,
        "D003 detects mid-sentence 'in this cut'")
    ok &= expect(
        not CADENCE_PATTERN.search("a clean cut through the layer"),
        "D003 false-pos: 'cut' as an ordinary noun")
    ok &= expect(
        not CADENCE_PATTERN.search(
            "A design that cuts one stage of the loop at the cost of another"),
        "D003 false-pos: 'that cuts' is a relative pronoun plus a verb")

    # D002 rejects link-shaped prose without touching real links.
    ok &= expect(
        NON_PATH_CHARS.search("const mir::Compute& compute") is not None,
        "D002 rejects a C++ signature as a link target")
    ok &= expect(
        NON_PATH_CHARS.search("self = <enclosing self expr>") is not None,
        "D002 rejects a pseudocode capture list")
    ok &= expect(
        not NON_PATH_CHARS.search("../decisions/mir-type-interning.md"),
        "D002 accepts a real relative path")
    ok &= expect(not PATH_SHAPED.search("closure_params"),
                 "D002 false-pos: a bare identifier is not a path")
    ok &= expect(not PATH_SHAPED.search("src"),
                 "D002 false-pos: a bare word is not a path")
    ok &= expect(PATH_SHAPED.search("../style.md") is not None,
                 "D002 accepts a file with an extension")
    ok &= expect(PATH_SHAPED.search("examples/") is not None,
                 "D002 accepts a directory")

    # D001 only fires on repo-rooted paths.
    m = CITED_PATH_PATTERN.search("see `tests/cases/` for the corpus")
    ok &= expect(m is not None and m.group(1) == "tests/cases/",
                 "D001 captures a repo-rooted path")
    m2 = CITED_PATH_PATTERN.search("the `dispatch.hpp` entry")
    ok &= expect(m2 is not None and "/" not in m2.group(1),
                 "D001 leaves a bare filename to be skipped")

    ok &= expect(line_of("a\nb\nc", 4) == 3, "line_of counts newlines")

    return ok


# --- Main ----------------------------------------------------------------

CHECKS = [
    ("D001 doc cites a path that does not exist", check_d001),
    ("D002 relative link does not resolve", check_d002),
    ("D003 cadence vocabulary in a timeless doc", check_d003),
    ("D004 index omits a document in its directory", check_d004),
]

VIOLATION_HINT = """
DOCUMENTATION POLICY VIOLATION

A doc claim a machine can check is a claim that should have been correct.
The fix is to correct the claim, never to loosen the rule -- and if the
claim keeps going stale, it is written at the wrong layer: mechanism
belongs in a "current implementation" note or in progress docs, while an
architecture contract states only what must hold.
"""


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
        print(VIOLATION_HINT)
        print("See tools/policy/check_docs.py for rule definitions.")
        return 1

    print("OK: documentation policy enforced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
