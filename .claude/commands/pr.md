---
description: Create a pull request with a well-formatted description
allowed-tools: Bash(git status:*), Bash(git log:*), Bash(git diff:*), Bash(git push:*), Bash(git branch:*), Bash(git fetch:*), Bash(git rev-list:*), Bash(git rebase:*)
---

# Pull Request

Create a PR following the project format.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Commits on this branch:** !`git log --oneline main..HEAD`
- **Full diff from main:** !`git diff main..HEAD --stat`
- **Commits behind main:** !`git fetch origin main --quiet && git rev-list --count HEAD..origin/main`

## PR Format

**Title:**

- Start with verb, capitalized
- Do NOT use colon format like "Fix: xxx"

**Body:**

Focus on **design decisions and architectural reasoning**, not just what changed. The diff shows what changed; the PR description explains why and how it fits into the system.

Key elements for a good PR:

1. **Design rationale**: Explain the approach taken and why. If the implementation is minimal, explain why existing infrastructure was sufficient. If new abstractions were added, explain why they were necessary.

2. **Integration with existing code**: How does this change leverage or extend existing patterns? What code paths are reused? This shows architectural understanding.

3. **What was added vs what was NOT needed**: Explicitly stating what didn't need to change is often more informative than listing what did. It demonstrates understanding of the architecture and helps reviewers verify correctness.

4. **The "why it works"**: If something works with surprisingly little code, explain the underlying reason. This is often the most valuable part of a PR description.

Adapt content to fit the PR type:

- **Feature PRs**: Design decisions, how it integrates with existing systems, what infrastructure is reused
- **Bug fix PRs**: Root cause analysis, why the fix is correct, any broader implications
- **Chore/docs PRs**: Brief explanation of what changed

Avoid:

- Listing files changed (GitHub shows this)
- Detailed test plans (unless testing approach is non-obvious)
- Internal planning concepts ("Phase 1", "Step 2")
- Restating the obvious from the diff

## Instructions

1. Check context above; ensure working tree is clean
2. If commits behind main > 0, rebase first: `git rebase origin/main`
3. Push if needed: `git push -u origin <branch>`
4. Create PR: `gh pr create --title "..." --body "..."`
5. Return the PR URL to the user

If updating an existing PR, push the new commits and update the PR body with `gh pr edit`.
