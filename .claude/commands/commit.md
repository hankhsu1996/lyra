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

## Instructions

1. Run `git status` and `git diff` to understand the changes
2. Format changed C++ and markdown files (if any)
3. Stage files with `git add <files>` (do NOT use `git add -A`)
4. Run `git commit` as a separate command (do NOT combine with add)
