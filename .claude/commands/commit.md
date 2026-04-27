---
description: Create a commit with a well-formatted message
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git branch:*), Bash(git switch:*), Bash(git log:*), Bash(clang-format:*), Bash(npx prettier:*), Bash(buildifier:*), Bash(find:*), Bash(python3 tools/policy/*)
---

# Commit

Create a commit following the project format.

## STOP: Check Branch First

**You are NOT allowed to commit on main.** Before doing ANYTHING else:

1. Check the current branch in the Context section below
2. If on `main`, infer an appropriate branch name from the changes (see Branch Rules)
3. Create the branch with `git switch -c <branch-name>` BEFORE any other steps

Do NOT proceed with formatting or staging until you are on a feature branch.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Staged diff:** !`git diff --cached`
- **Unstaged diff:** !`git diff`

## Pre-commit Checks

Before committing, format ALL files and run every check that CI runs.

**Rule of thumb:** Local must mirror CI exactly. If CI runs it, the skill must run it. If a script in `tools/policy/` exists, it likely has a corresponding CI workflow under `.github/workflows/`.

### Format

1. **C++ files** - Format all source files:

   ```bash
   find src include tests -name '*.cpp' -o -name '*.hpp' | xargs clang-format -i
   ```

2. **Documentation** - Format all markdown files:

   ```bash
   npx prettier --write "**/*.md"
   ```

3. **Bazel files** - Format all Bazel files:

   ```bash
   buildifier -r .
   ```

### Format-check (mirrors CI)

After formatting, verify CI's format-check commands pass:

```bash
find src include tests -name '*.cpp' -o -name '*.hpp' | xargs clang-format --dry-run --Werror
buildifier -mode=check -r .
buildifier -mode=check -lint=warn -r .
npm run format:check
```

### Policy checks (mirrors CI)

```bash
python3 tools/policy/check_architecture.py
python3 tools/policy/check_ascii.py --diff-base origin/main
python3 tools/policy/check_cpp_style.py
python3 tools/policy/check_exceptions.py --diff-base origin/main
```

If any of the above fail, fix violations before committing. Do not stage / commit through known violations.

**Sanity check before committing:** `ls tools/policy/check_*.py` and `ls .github/workflows/` and confirm the commands above still match. If a new `check_*.py` exists but the skill does not run it, run it anyway and tell the user the skill is out of date.

## Commit Format

```
<Summary starting with verb, 50 chars or less>

- Bullet under 60 chars
- Another bullet if needed (2-5 total)
```

Bullet points should be **concise** (under 60 chars each) and describe **what changed**, not background context.

**ASCII only.** No special Unicode characters.

**CRITICAL: Do NOT add attribution.** No "Generated with Claude Code", no "Co-Authored-By", no author credits. These duplicate badly when squash-merging. The commit message should ONLY contain the summary line and bullet points.

**IMPORTANT: Describe the outcome, not the process.** The commit message reflects what changed, not how you got there.

## Branch Rules

**Branch name format:** `<type>/<short-description>`

- **Types:** `feature`, `bugfix`, `refactor`, `release`, `chore`, `docs`
- Use kebab-case: `aaa-bbb-ccc`
- Keep short (~5 words max)

**IMPORTANT: Name for the primary feature, not the recent task.** Look at the full diff and identify what the main deliverable is. Don't name the branch after the last thing you happened to work on in the conversation.

**Examples:**

- `feature/user-auth`
- `bugfix/null-pointer-crash`
- `refactor/split-codegen` (behavior-preserving restructuring)
- `chore/update-deps` (CI changes go here)
- `docs/api-reference`

## Instructions

1. **Check branch first** - See "STOP: Check Branch First" section above. Do NOT skip this.
2. Format all files (C++, markdown, Bazel)
3. Run all policy checks - fix any violations before proceeding
4. **Check git status again** - Formatters may modify files beyond your original changeset. Run `git status --short` to see all modified files before staging.
5. Stage files with `git add <files>` (do NOT use `git add -A`)
6. Run `git commit` as a separate command (do NOT chain with add)

**Note:** Never use `git commit --amend` if the previous commit has been pushed. If `git status` shows "Your branch is up to date with origin", the last commit is pushed - create a new commit instead.
