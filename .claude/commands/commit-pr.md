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
- **The trigger stages what the change is.** Asking for a commit and a PR is asking for the work to
  land, so `git add` is part of the job here and not a signal to wait for -- the review the staging
  ordinarily stands for is the one the trigger itself gave. Stage the change whole, by path and
  never `-A`, and leave alone anything you cannot account for as yours: another clone edits shared
  files, and the user edits their own tree while this runs. **Never unstage**, here or anywhere.

  The earlier form said to commit what was already staged and add nothing, which reads as caution
  and is not: it stopped a finished branch one keystroke short of its PR, after the user had asked
  for both, because a cleanup pass had left its own edits unstaged by design.
- **One commit or several?** Ask only if the staged set spans clearly unrelated work. A fix plus the
  test that proves it is one commit, not two.
