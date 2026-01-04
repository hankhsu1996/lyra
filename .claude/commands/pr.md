---
description: Create a pull request with a well-formatted description
---

# Pull Request

Create a PR following the project format.

## Pre-PR Checks

1. Ensure all changes are committed
2. Push the branch to remote if not already pushed

## PR Format

**Title:**

- Start with verb, capitalized
- Do NOT use colon format like "Fix: xxx"
- Example: "Fix cpp-lint CI to use Clang 20"

**Body:**

Include a Summary section. Beyond that, adapt the content to fit the PR type:

- **Feature PRs:** Background/motivation, what was implemented, test plan if applicable
- **Bug fix PRs:** Root cause analysis, how it was fixed, any refactoring done
- **Chore/docs PRs:** Brief explanation of what changed

Don't force a test plan if it doesn't make sense. Write what's useful for reviewers.

## Instructions

1. Run `git status` to ensure working tree is clean
2. Check if branch is pushed: `git log origin/<branch>..<branch>`
3. Push if needed: `git push -u origin <branch>`
4. Create PR: `gh pr create --title "..." --body "..."`
5. Return the PR URL to the user
