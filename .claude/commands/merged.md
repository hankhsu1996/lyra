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

1. Switch to main branch: `git switch main`
2. Pull latest changes: `git pull`
3. Delete the local feature branch (the one we were working on)

The remote branch is auto-deleted by GitHub on merge, so no need to delete it manually.
