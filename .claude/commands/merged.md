---
description: Clean up after PR is merged
allowed-tools: Bash(git switch:*), Bash(git pull:*), Bash(git branch:*)
---

# Merged

Clean up local branch after PR is merged.

## Context

- **Current branch:** !`git branch --show-current`
- **All local branches:** !`git branch`

## Instructions

1. Switch to main: `git switch main`
2. Pull latest: `git pull`
3. Delete local branch with `-D` (squash merge requires force): `git branch -D <branch>`

The remote branch is auto-deleted by GitHub on merge.
