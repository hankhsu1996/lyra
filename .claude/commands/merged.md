---
name: merged
description: Clean up after PR is merged
disable-model-invocation: true
---

# Merged

Clean up local branch after PR is merged.

## Context

- **Current branch:** !`git branch --show-current`
- **All local branches:** !`git branch`

## Instructions

1. Check if the PR for the current branch is merged: `gh pr view --json state --jq '.state'`
   - If **not merged**: tell the user it's not merged yet, provide the PR link (`gh pr view --json url --jq '.url'`), and stop.
   - If **merged**: continue with cleanup below.
2. Switch to main: `git switch main`
3. Pull latest: `git pull`
4. Delete local branch with `-D` (squash merge requires force): `git branch -D <branch>`

The remote branch is auto-deleted by GitHub on merge.
