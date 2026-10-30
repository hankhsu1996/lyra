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
        `conformance/cases/` from a bare `dispatch.hpp` named relative to
        whatever directory the surrounding prose is discussing.

  D002  A relative markdown link must resolve.
        Scope: every tracked markdown file, outside fenced code.

  D003  No migration-cadence vocabulary in the timeless tiers.
        Scope: docs/architecture/**, docs/glossary/**, top-level docs/*.md,
               README.md, CLAUDE.md.
        An architecture contract states what holds, not when it started
        holding; "in this cut" has no referent once the cut is history.
        Cadence belongs in the issues that track work, which exist to
        carry it, and in docs/decisions/, which records a decision at a
        point in time.
        Text inside code spans is exempt: a quoted counter-example is not
        an assertion.

  D004  An index must list every document in the directory it indexes.
        Scope: the INDEXES table below.
        An index that silently omits an entry is worse than no index --
        it reads as complete.

  D005  A reader-facing README must not state a count of anything the
        repository contains.
        Scope: README.md, examples/README.md.
        A case total or a coverage ratio is wrong the next time someone
        adds one of the thing counted, nothing forces its update, and
        publishing it as a measure of scope makes it a target. Say that
        coverage is measured and point at where. Numbers inside code
        blocks, links, and code spans are exempt, as is a digit bound to
        a word (`C++23`) and a standard's designation (`IEEE 1800`),
        which counts nothing this repository contains.

  D006  A permanent doc must not cite a work issue.
        Scope: docs/architecture/**, docs/decisions/**, docs/glossary/**.
        The dependency runs one way, from permanent to ephemeral. A
        contract that points at a working queue cannot outlive it, and
        the issue closes while the contract stands. State the permanent
        fact instead. An issue is caught by its URL only: a bare `#N`
        names a pull request as often, and a merged one is history a
        record may cite.

  D008  A cross-reference spelled as a quoted path must resolve.
        Scope: every tracked markdown file.
        These docs name a sibling doc three ways -- from the repo root,
        from the citing file (`../architecture/lir.md`), and from `docs/`
        (`architecture/lir.md`) -- and only the first is a D001 path and
        only a bracketed link is a D002 one. A rename is what breaks a
        cross-reference, and it breaks every spelling equally.

Usage:
  python3 tools/policy/check_docs.py
"""

import re
import sys
from pathlib import Path

SKIP_PREFIXES = (
    "external/", "bazel-", "node_modules/",
    # Slash-command definitions for the coding assistant, not project docs.
    ".claude/",
    # Git-ignored scratch. A check that goes red over a working note nobody
    # will commit teaches its reader to ignore it.
    "tmp/",
)

# Per-workspace scratch, git-ignored, and free to name anything.
SKIP_SUFFIXES = (".local.md",)

# Git-ignored working directories a doc may legitimately name even though
# a fresh clone has none of them.
UNTRACKED_ROOTS = frozenset({"playground", "obj_dir", "build", "out"})

# Rule D002
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

# Rule D001
CITED_PATH_PATTERN = re.compile(r"`([A-Za-z0-9_.][A-Za-z0-9_./-]*)`")

# Rule D003
# "that" is deliberately absent: it reads as a relative pronoun before a
# verb ("a design that cuts one stage"), which is ordinary prose.
CADENCE_PATTERN = re.compile(
    r"\b(?:this|next|later|earlier|previous|following)\s+cuts?\b",
    re.IGNORECASE,
)
TIMELESS_DIRS = ("docs/architecture/", "docs/glossary/")
TIMELESS_FILES = ("README.md", "CLAUDE.md")

# Rule D004
# (index file, indexed directory). An index not listed here makes no
# completeness claim.
INDEXES = (
    ("docs/decisions/README.md", "docs/decisions"),
    ("docs/architecture/README.md", "docs/architecture"),
)

# Rule D005
READER_FACING = ("README.md", "examples/README.md")

# A standalone integer of two digits or more. The lookarounds keep a digit
# bound to a word out of it, so `C++23` and `1800-2023` are not counts.
BARE_COUNT_PATTERN = re.compile(r"(?<![A-Za-z0-9+_-])\d{2,}(?![A-Za-z0-9+_-])")

# A standard's designation names nothing this repository contains, so it is not
# a count of anything and cannot go stale as one. `1800-2023` already falls out
# of the pattern above because the digits are bound by the hyphen; the bare form
# needs the naming word to say the same thing.
DESIGNATION_PATTERN = re.compile(r"\b(?:IEEE|LRM)\s+\d[\d.-]*")

# Rule D006
PERMANENT_DIRS = ("docs/architecture/", "docs/decisions/", "docs/glossary/")
# GitHub matches an owner and a repository by any case, and the project board
# is the same queue seen whole.
WORK_ISSUE_PATTERN = re.compile(
    r"github\.com/(?:hankhsu1996/lyra/issues|users/hankhsu1996/projects)/\d+",
    re.IGNORECASE,
)

# Rule D008
# Narrower than D001's pattern by the `.md` tail, so the rule reaches only
# cross-references and not the source paths, globs, and tool names D001
# covers. The leading character class admits no `~`, which leaves out a path
# into the reader's own home -- nothing this tree can resolve either way.
CITED_DOC_PATTERN = re.compile(r"`([A-Za-z0-9_.][A-Za-z0-9_./-]*\.md)`")


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


# Checks
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


def check_d005(repo_root: Path) -> list[str]:
    errors = []
    for rel in READER_FACING:
        path = repo_root / rel
        if not path.exists():
            continue
        body = strip_inline_code(strip_fenced(path.read_text()))
        body = re.sub(r"\[[^\]]*\]\([^)]*\)", "", body)
        # Same-length so every later offset, and so every reported line
        # number, still points where it did.
        body = DESIGNATION_PATTERN.sub(lambda m: "-" * len(m.group(0)), body)
        for m in BARE_COUNT_PATTERN.finditer(body):
            errors.append(
                f"  {rel}:{line_of(body, m.start())}: D005 bare count "
                f"'{m.group(0)}'; say coverage is measured and point at where"
            )
    return errors


def check_d006(repo_root: Path) -> list[str]:
    errors = []
    for path, rel in iter_docs(repo_root):
        if not rel.startswith(PERMANENT_DIRS):
            continue
        text = path.read_text()
        for m in WORK_ISSUE_PATTERN.finditer(text):
            errors.append(
                f"  {rel}:{line_of(text, m.start())}: D006 cites "
                f"'{m.group(0)}'; state the permanent fact instead"
            )
    return errors


def check_d008(repo_root: Path) -> list[str]:
    tops = repo_top_level_dirs(repo_root)
    docs_root = repo_root / "docs"
    errors = []
    for path, rel in iter_docs(repo_root):
        text = path.read_text()
        for m in CITED_DOC_PATTERN.finditer(text):
            cited = m.group(1)
            head = cited.split("/", 1)[0]
            if "/" not in cited or head in tops:
                continue
            bases = (path.parent, docs_root, repo_root)
            if any((base / cited).exists() for base in bases):
                continue
            errors.append(
                f"  {rel}:{line_of(text, m.start())}: D008 cites "
                f"'{cited}', which resolves against neither this directory "
                f"nor docs/"
            )
    return errors


# Self-tests
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
    m = CITED_PATH_PATTERN.search("see `conformance/cases/` for the corpus")
    ok &= expect(m is not None and m.group(1) == "conformance/cases/",
                 "D001 captures a repo-rooted path")
    m2 = CITED_PATH_PATTERN.search("the `dispatch.hpp` entry")
    ok &= expect(m2 is not None and "/" not in m2.group(1),
                 "D001 leaves a bare filename to be skipped")

    ok &= expect(line_of("a\nb\nc", 4) == 3, "line_of counts newlines")

    # D005 fires on counts and stays quiet on version-like digits.
    ok &= expect(
        BARE_COUNT_PATTERN.search("a corpus of 908 cases") is not None,
        "D005 detects a case count")
    ok &= expect(
        BARE_COUNT_PATTERN.search("claims 457 of the 908") is not None,
        "D005 detects a coverage ratio")
    ok &= expect(not BARE_COUNT_PATTERN.search("A C++23 compiler"),
                 "D005 false-pos: C++23 is a language version")
    ok &= expect(not BARE_COUNT_PATTERN.search("targets IEEE 1800-2023"),
                 "D005 false-pos: a hyphenated standard number")
    ok &= expect(not BARE_COUNT_PATTERN.search("clang-format-20 is used"),
                 "D005 false-pos: a digit bound to a tool name")
    ok &= expect(not BARE_COUNT_PATTERN.search("one of 5 things"),
                 "D005 ignores a single digit")

    # D006 fires on a work issue and leaves history and other projects alone.
    ok &= expect(
        WORK_ISSUE_PATTERN.search(
            "tracked in https://github.com/hankhsu1996/lyra/issues/1487")
        is not None,
        "D006 detects a work issue citation")
    ok &= expect(
        WORK_ISSUE_PATTERN.search("github.com/HankHsu1996/Lyra/issues/12")
        is not None,
        "D006 detects a work issue spelled in another case")
    ok &= expect(
        WORK_ISSUE_PATTERN.search("see https://github.com/users/hankhsu1996/projects/1")
        is not None,
        "D006 detects the project board")
    ok &= expect(
        not WORK_ISSUE_PATTERN.search("merged as #1485"),
        "D006 false-pos: a pull request number is history")
    ok &= expect(
        not WORK_ISSUE_PATTERN.search(
            "https://github.com/rust-lang/rust/issues/133883"),
        "D006 false-pos: another project's issue is evidence")
    ok &= expect(
        not WORK_ISSUE_PATTERN.search("see `../decisions/mir.md`"),
        "D006 false-pos: a decisions citation is allowed")

    # D008 captures the two spellings D001 and D002 both miss.
    relative = "../architecture/lifetime.md"
    m3 = CITED_DOC_PATTERN.search(f"see `{relative}` for it")
    ok &= expect(m3 is not None and m3.group(1) == relative,
                 "D008 captures a file-relative cross-reference")
    m4 = CITED_DOC_PATTERN.search("`architecture/lir.md` holds the contract")
    ok &= expect(m4 is not None and m4.group(1) == "architecture/lir.md",
                 "D008 captures a docs-root-relative cross-reference")
    m5 = CITED_DOC_PATTERN.search("the `lifetime.md` contract")
    ok &= expect(m5 is not None and "/" not in m5.group(1),
                 "D008 leaves a bare filename to be skipped")
    ok &= expect(not CITED_DOC_PATTERN.search("`~/wiki/lyra/map.md`"),
                 "D008 false-pos: a path into the reader's home")
    ok &= expect(not CITED_DOC_PATTERN.search("`src/lyra/value/packed.cpp`"),
                 "D008 false-pos: a source path is D001's")

    return ok


# Main
CHECKS = [
    ("D001 doc cites a path that does not exist", check_d001),
    ("D002 relative link does not resolve", check_d002),
    ("D003 cadence vocabulary in a timeless doc", check_d003),
    ("D004 index omits a document in its directory", check_d004),
    ("D005 reader-facing README states a count", check_d005),
    ("D006 permanent doc cites a work issue", check_d006),
    ("D008 quoted cross-reference does not resolve", check_d008),
]

VIOLATION_HINT = """
DOCUMENTATION POLICY VIOLATION

A doc claim a machine can check is a claim that should have been correct.
The fix is to correct the claim, never to loosen the rule -- and if the
claim keeps going stale, it is written at the wrong layer: mechanism
belongs in a "current implementation" note or in an issue, while an
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
