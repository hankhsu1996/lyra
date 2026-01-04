---
description: Create a commit with a well-formatted message
allowed-tools: Bash(clang-format:*), Bash(npx prettier:*), Bash(buildifier:*)
---

# Commit

Create a commit following the project format.

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

4. **C++ lint (optional)** - Run clang-tidy on changed C++ files:
   ```bash
   bazel run @hedron_compile_commands//:refresh_all  # if compile_commands.json is stale
   clang-tidy -p . <changed-cpp-files>
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

Before committing, ensure you're on a feature branch (not main):

```bash
git branch --show-current
```

If on main, create a branch first (prefer `switch` over `checkout`):

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

1. Check current branch; create feature branch if on main
2. Run `git status` and `git diff` to understand the changes
3. Format changed C++ and markdown files (if any)
4. Stage files with `git add <files>` (do NOT use `git add -A`)
5. Run `git commit` as a separate command (do NOT combine with add)
