---
description: Create a pull request with a well-formatted description
allowed-tools: Bash(git status:*), Bash(git log:*), Bash(git diff:*), Bash(git push:*), Bash(git branch:*)
---

# Pull Request

Create a PR following the project format.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Commits on this branch:** !`git log --oneline main..HEAD`
- **Full diff from main:** !`git diff main..HEAD --stat`

## PR Format

**Title:**

- Start with verb, capitalized
- Do NOT use colon format like "Fix: xxx"
- Example: "Fix cpp-lint CI to use Clang 20"

**Body:**

Include a Summary section. Use prose for cohesive changes; use bullets only when listing unrelated items. Beyond that, adapt the content to fit the PR type:

- **Feature PRs:** Background/motivation, what was implemented, test plan if applicable
- **Bug fix PRs:** Root cause analysis, how it was fixed, any refactoring done
- **Chore/docs PRs:** Brief explanation of what changed

Don't force a test plan if it doesn't make sense. Write what's useful for reviewers.

Don't list files changed - GitHub already shows this in the diff. Focus on conceptual changes.

Code examples should support explanations, not replace them. Include both the "why" and the "what".

**Important:** Never include internal planning concepts like "Phase 1", "Phase 2", or similar milestone labels. PRs describe results, not the journey. Reviewers care about what the code does, not how the implementation was planned or tracked internally.

## Instructions

1. Check context above; ensure working tree is clean
2. Push if needed: `git push -u origin <branch>`
3. Create PR: `gh pr create --title "..." --body "..."`
4. Return the PR URL to the user

If updating an existing PR, push the new commits and update the PR body with `gh pr edit`.
