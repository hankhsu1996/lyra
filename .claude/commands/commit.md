---
description: Create a commit with a well-formatted message
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git branch:*), Bash(git switch:*), Bash(clang-format:*), Bash(clang-tidy:*), Bash(npx prettier:*), Bash(buildifier:*)
---

# Commit

Create a commit following the project format.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Staged diff:** !`git diff --cached`
- **Unstaged diff:** !`git diff`

## Pre-commit Checks

Before committing, format and lint changed files:

1. **C++ files** - If any `.cpp` or `.hpp` files changed:

   ```bash
   clang-format -i <changed-cpp-files>
   clang-tidy -p . <changed-cpp-files>              # Few files
   run-clang-tidy -p . -j 20 <changed-cpp-files>    # Many files (parallel)
   ```

   Code must be clang-tidy warning-free. Fix any warnings before committing.

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

**IMPORTANT: Do NOT add attribution.** No "Generated with Claude Code", no "Co-Authored-By". These duplicate badly when squash-merging.

**IMPORTANT: Focus on features, not implementation journey.** Don't reference internal tracking like "Phase 1", "Step 2", or planning documents.

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
3. Run linters (clang-tidy for C++) and **fix all warnings before proceeding**
4. Stage files with `git add <files>` (do NOT use `git add -A`)
5. Run `git commit` as a separate command (do NOT chain with add)

**Note:** Never use `git commit --amend` if the previous commit has been pushed. If `git status` shows "Your branch is up to date with origin", the last commit is pushed - create a new commit instead.
