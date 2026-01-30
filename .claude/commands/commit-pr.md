---
description: Create a commit and PR with auto-merge enabled
allowed-tools: Bash(git status:*), Bash(git diff:*), Bash(git add:*), Bash(git branch:*), Bash(git switch:*), Bash(git log:*), Bash(git fetch:*), Bash(git rebase:*), Bash(git push:*), Bash(git rev-list:*), Bash(gh pr create:*), Bash(gh pr merge:*), Bash(clang-format:*), Bash(npx prettier:*), Bash(buildifier:*), Bash(find:*), Bash(python3 tools/policy/*), AskUserQuestion
---

# Commit and PR

Create a commit and PR in one workflow, with auto-merge enabled.

## STOP: Check Branch First

**You are NOT allowed to commit on main.** Before doing ANYTHING else:

1. Check the current branch in the Context section below
2. If on `main`, STOP and ask the user for a branch name using AskUserQuestion
3. Create the branch with `git switch -c <branch-name>` BEFORE any other steps

Do NOT proceed with formatting or staging until you are on a feature branch.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Staged diff:** !`git diff --cached`
- **Unstaged diff:** !`git diff`
- **Commits on this branch:** !`git log --oneline main..HEAD 2>/dev/null || echo "(new branch)"`
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

## Instructions

### Phase 1: Commit

1. **Check branch** - If on main, ask for branch name and create it
2. **Format all files:**
   - C++: `find src include -name '*.cpp' -o -name '*.hpp' | xargs clang-format -i`
   - Markdown: `npx prettier --write "**/*.md"`
   - Bazel: `buildifier -r .`
3. **Exception policy:** `python3 tools/policy/check_exceptions.py --diff-base origin/main`
4. **Stage files** with `git add <files>` (not `git add -A`)
5. **Commit** with `git commit` (this will prompt for permission - your checkpoint to review)

### Phase 2: PR

6. **Rebase if behind main:** If commits behind > 0, run `git rebase origin/main`
7. **Push:** `git push -u origin <branch>`
8. **Create PR:** `gh pr create --title "..." --body "..."`
9. **Enable auto-merge:** `gh pr merge --auto --squash`
10. **Return the PR URL** to the user
