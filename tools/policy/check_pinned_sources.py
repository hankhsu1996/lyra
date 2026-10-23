#!/usr/bin/env python3
"""Check that the front end's pinned commit stays fetchable.

Rules:
  P001: The slang commit MODULE.bazel names is on the fork's branch for the
        release MODULE.bazel names, `lyra/v<major>`

Lyra reads slang from a fork at one commit. A commit no branch reaches can be
collected, and every Lyra commit naming it then stops building; the fork keeps
one branch per release that only ever grows, so a commit on it stays reachable
for as long as the branch stands. The check fetches that branch's history
without its files and asks whether the commit is in it.

Usage:
  python3 tools/policy/check_pinned_sources.py
"""

import re
import subprocess
import sys
import tempfile
from pathlib import Path

DEP = re.compile(r'bazel_dep\(\s*name\s*=\s*"slang"\s*,\s*version\s*=\s*"([^"]+)"')
OVERRIDE = re.compile(
    r'git_override\(\s*module_name\s*=\s*"slang"\s*,'
    r'\s*commit\s*=\s*"([0-9a-f]{40})"\s*,'
    r'\s*remote\s*=\s*"([^"]+)"'
)


def run_git(cwd: Path, args: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["git"] + args, capture_output=True, text=True, cwd=cwd)


def check(module_text: str) -> list[str]:
    dep = DEP.search(module_text)
    override = OVERRIDE.search(module_text)
    if not dep or not override:
        return ["MODULE.bazel: P001 names no slang release and commit to check"]

    major = dep.group(1).split(".")[0]
    commit, remote = override.groups()
    branch = f"lyra/v{major}"

    with tempfile.TemporaryDirectory() as scratch:
        repo = Path(scratch)
        run_git(repo, ["init", "--quiet", "--bare"])
        fetched = run_git(repo, [
            "fetch", "--quiet", "--no-tags", "--filter=tree:0", remote,
            f"refs/heads/{branch}:refs/heads/line",
        ])
        if fetched.returncode != 0:
            return [
                f"MODULE.bazel: P001 cannot fetch the fork's {branch} for slang "
                f"{dep.group(1)}: {fetched.stderr.strip()}"
            ]
        on_branch = run_git(repo, ["merge-base", "--is-ancestor", commit, "line"])
        if on_branch.returncode != 0:
            return [
                f"MODULE.bazel: P001 slang commit {commit[:12]} is not on the "
                f"fork's {branch}; commit it there, or move the pin to a commit "
                f"that is"
            ]
    return []


def main() -> int:
    repo_root = Path(__file__).resolve().parent.parent.parent
    errors = check((repo_root / "MODULE.bazel").read_text())

    if errors:
        print("Pinned source violations:\n")
        for error in errors:
            print(f"  {error}")
        print(f"\nTotal: {len(errors)} violations")
        print("\nRules:")
        print("  P001: The slang commit MODULE.bazel names is on the fork's branch")
        print("        for the release MODULE.bazel names, lyra/v<major>")
        return 1

    print("The pinned slang commit is on its release branch")
    return 0


if __name__ == "__main__":
    sys.exit(main())
