---
name: merged
description: Clean up after PR is merged
disable-model-invocation: true
---

# Merged

Clean up local branch after PR is merged.

## Context

- **Current branch:** !`git branch --show-current`
- **PR state:** !`gh pr view --json state --jq '.state' 2>/dev/null || echo "(no PR for this branch)"`
- **PR url:** !`gh pr view --json url --jq '.url' 2>/dev/null || echo "-"`
- **All local branches:** !`git branch`

## Instructions

Read **PR state** above -- it is already resolved, no need to query again.

- Not `MERGED`: tell the user it is not merged yet, give the **PR url**, and stop.
- `MERGED`: clean up in one command (each `&&` gates the next, so a failed step stops before the
  delete):

  ```bash
  git switch main && git pull && git branch -D <branch>
  ```

  Fill `<branch>` with the merged branch (the **Current branch** above) -- named explicitly, not
  auto-detected, so a stray run on the wrong branch cannot silently delete it. `-D` because a squash
  merge requires force; GitHub auto-deletes the remote branch.
