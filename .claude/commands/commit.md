---
description: Create a commit with a well-formatted message
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git branch:*), Bash(git switch:*), Bash(clang-format:*), Bash(npx prettier:*), Bash(buildifier:*)
---

# Commit

Create a commit following the project format.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Staged diff:** !`git diff --cached`
- **Unstaged diff:** !`git diff`

## Pre-commit Checks

Before committing, format changed files:

1. **C++ files** - If any `.cpp` or `.hpp` files changed:

   ```bash
   clang-format -i <changed-cpp-files>
   ```

2. **Documentation** - If any `.md` files changed:

   ```bash
   npx prettier --write <changed-md-files>
   ```

3. **Bazel files** - If any `BUILD.bazel`, `.bzl`, or `MODULE.bazel` files changed:

   ```bash
   buildifier <changed-bazel-files>
   ```

Skip formatting if files are already clean (no diff after format).

## Commit Format

```
<Summary starting with verb, 50 chars or less>

- Bullet point explaining what changed
- Another bullet point if needed (2-5 total)
```

Bullet points should describe **what changed**, not background context or why the old state was bad.

**Do NOT add attribution** (no "Generated with Claude Code", no "Co-Authored-By") - these duplicate badly when squash-merging.

## Branch Rules

If on main, create a feature branch first (prefer `switch` over `checkout`):

```bash
git switch -c <type>/<short-description>
```

**Branch name format:** `<type>/<short-description>`

- **Types:** `feature`, `bugfix`, `release`, `chore`, `docs`
- Use kebab-case: `aaa-bbb-ccc`
- Keep short (~5 words max)

**Examples:**

- `feature/user-auth`
- `bugfix/null-pointer-crash`
- `chore/update-deps` (CI changes go here)
- `docs/api-reference`

## Instructions

1. Check context above; create feature branch if on main
2. Format changed files if needed (C++, markdown, Bazel)
3. Stage files with `git add <files>` (do NOT use `git add -A`)
4. Run `git commit` as a separate command (do NOT chain with add)

**Note:** Never use `git commit --amend` if the previous commit has been pushed. If `git status` shows "Your branch is up to date with origin", the last commit is pushed - create a new commit instead.
