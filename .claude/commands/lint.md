---
description: Format and lint the codebase
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(clang-format:*), Bash(clang-tidy:*), Bash(run-clang-tidy:*), Bash(buildifier:*), Bash(npx prettier:*), Bash(find:*), AskUserQuestion
---

# Lint

Format all files (fast), then run linters on changed files.

## Context

- **Git status:** !`git status --short`
- **Changed files:** !`git diff --name-only HEAD`

## Format (All Files)

Format is fast, so we run it on everything:

```bash
find src include -name '*.cpp' -o -name '*.hpp' | xargs clang-format -i
npx prettier --write "**/*.md"
buildifier -r .
```

## Lint (Changed Files)

### C++ Files (clang-tidy)

For changed files only (faster):

```bash
clang-tidy -p . <changed-cpp-files>              # Few files (~20s each)
run-clang-tidy -p . -j 12 <changed-cpp-files>    # Many files (parallel)
```

For all files (thorough):

```bash
run-clang-tidy -p . -header-filter='^.*(src|include)/lyra/.*' -j 12 src/lyra/
```

**CRITICAL: Fix ALL clang-tidy warnings, including pre-existing ones.** If a file has warnings - even on lines you didn't change - you MUST fix them. Never dismiss warnings as "pre-existing". Leave files cleaner than you found them.

## Instructions

1. Format all files (C++, markdown, Bazel)
2. Run clang-tidy on changed C++ files
3. Fix all warnings before finishing
