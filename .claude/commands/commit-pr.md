---
description: Create a commit and open a pull request for review
allowed-tools: Bash(git status:*), Bash(git branch:*)
---

# Commit and PR

Do `/commit`, then `/pr`, in that order.

**This file holds nothing that those two hold.** The branch gate, the pre-commit checks and the
commit message format live in `/commit`; the PR title, description and the push / rebase sequence
live in `/pr`. Keeping a second copy here means keeping two copies correct, and the copies
lose. Read the two skills; do not restate them here, and do not answer from memory.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`

## Sequence

There is no include mechanism between slash commands, so this composition only happens if you make
it happen. Both steps are **tool calls, not references**:

1. Call the Skill tool with `commit`. Follow it in full: branch gate, build, format, lint and
   policy, staging, commit message. Do not start staging before its text is in context.
2. Call the Skill tool with `pr`. Follow it in full: rebase, gate, push, PR body, return the URL.

If you find yourself writing a commit message or a PR body without having invoked both, stop -- you
are working from memory. This file deliberately carries no fallback copy; an incomplete run is the
correct failure, a confidently wrong one is not.

## Only what this composition adds

- **The gate runs once, in `/pr`, on what the rebase produced.** `/commit` builds, formats and
  lints; the tests belong on the tree that will land, and that tree is the rebase's output. Where
  the rebase turns out to be a no-op it is the same single run, arriving one step later.
- **Staging is the user's signal, not yours.** A staged file means it has been reviewed. Never
  stage, unstage, or `git restore --staged` on the user's behalf -- if files are already staged when
  this runs, commit those and add nothing.
- **One commit or several?** Ask only if the staged set spans clearly unrelated work. A fix plus the
  test that proves it is one commit, not two.
