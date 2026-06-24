---
description: Create a commit and PR with auto-merge enabled
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git branch:*), Bash(git switch:*), Bash(git log:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git push:*), Bash(git rev-list:*), Bash(gh pr create:*), Bash(gh pr merge:*), Bash(clang-format:*), Bash(npx prettier:*), Bash(buildifier:*), Bash(find:*), Bash(python3 tools/policy/*), Bash(bazel build:*), Bash(bazel test:*), Bash(ls:*)
---

# Commit and PR

Create a commit and PR in one workflow, with auto-merge enabled.

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
- **Commits on this branch:** !`git log --oneline main..HEAD 2>/dev/null || echo "(new branch)"`
- **Full diff from main:** !`git diff --merge-base origin/main HEAD --stat`
- **Commits behind main:** !`git fetch origin main --quiet 2>/dev/null && git rev-list --count HEAD..origin/main 2>/dev/null || echo "0"`

## Branch Rules

**Branch name format:** `<type>/<short-description>`

- **Types:** `feature`, `bugfix`, `refactor`, `release`, `chore`, `docs`
- Use kebab-case: `aaa-bbb-ccc`
- Keep short (~5 words max)

**IMPORTANT: Name for the primary feature, not the recent task.**

## Commit Format

```
<Summary starting with verb, 50 chars or less>

- Bullet under 60 chars
- Another bullet if needed (2-5 total)
```

- **ASCII only.** No special Unicode characters.
- **No attribution.** No "Generated with Claude Code", no "Co-Authored-By".
- **Describe the outcome, not the process.**

## PR Format

**Title:** Start with verb, capitalized. Do NOT use colon format like "Fix: xxx".

**Body:** Start with `## Summary` as a paragraph (not bullets). Add `## Design` or `## Testing` sections only if they add value.

**Paragraph wrapping:** Keep each paragraph on a single line in the HEREDOC; GitHub markdown wraps automatically. Manually inserting newlines inside a paragraph produces broken-looking text in the rendered PR. Hard line breaks are only correct inside code blocks and bullet lists.

## Instructions

Each tool runs **once**. Formatters run in write mode; rerunning them in check mode is redundant since the write call already left the tree canonical. The only tools that run as a separate check are the ones formatters don't fix (buildifier lint, policy checks).

### Phase 1: Commit

1. **Check branch** - If on main, infer branch name from changes and create it
2. **Build and test:**
   - `bazel build //...`
   - `bazel test //... --test_output=errors`
   - Do not narrow `//...`. Fix failures before continuing.
3. **Format (write mode, once each):**
   - `find src include tests -name '*.cpp' -o -name '*.hpp' | xargs clang-format -i`
   - `npx prettier --write "**/*.md"`
   - `buildifier -r .`
4. **Lint and policy:**
   - `buildifier -mode=check -lint=warn -r .`
   - `python3 tools/policy/check_architecture.py`
   - `python3 tools/policy/check_ascii.py --diff-base origin/main`
   - `python3 tools/policy/check_cpp_style.py`
   - `python3 tools/policy/check_exceptions.py --diff-base origin/main`
5. **Sanity check** - `ls tools/policy/check_*.py` and `ls .github/workflows/`. If a new `check_*.py` exists that the skill does not run, run it anyway and tell the user the skill is out of date.
6. **Check git status** - Formatters may modify files beyond your original changeset. Run `git status --short` to see all modified files before staging.
7. **Stage files** with `git add <files>` (not `git add -A`)
8. **Commit** with `git commit` (this will prompt for permission - your checkpoint to review)

### Phase 2: PR

9. **Rebase if behind main:** If commits behind > 0:
   - `git rebase origin/main`
   - Re-run clang-format (a rebase can drift C++ formatting against upstream):
     - `find src include tests -name '*.cpp' -o -name '*.hpp' | xargs clang-format -i`
10. **Push:** `git push -u origin <branch>`
11. **Create PR:** `gh pr create --title "..." --body "..."`
12. **Enable auto-merge:** `gh pr merge --auto --squash`
13. **Return the PR URL** to the user
