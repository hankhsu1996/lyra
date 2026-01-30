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

Always start with `## Summary` as a paragraph (not bullet points) describing what the PR does.

After Summary, add sections only if they add value. All sections are optional:

- `## Design` - For non-trivial design decisions, architectural reasoning
- `## Testing` - Only if testing approach is non-obvious or worth highlighting
- Other sections as appropriate for the PR

Simple fixes may need only Summary. Don't force sections that have nothing meaningful to say.

**What makes a good PR description:**

- **Design rationale**: Explain the approach and why. If minimal, explain why existing infrastructure was sufficient.
- **What didn't change**: Often more informative than listing what did. Shows architectural understanding.
- **Alternatives explored**: If complexity was considered and rejected, mention it briefly.
- **Why it works**: If something works with little code, explain the underlying reason.

**Formatting:**

- Summary: paragraph, not bullet points
- Other sections: bullet points, checkboxes, or prose as appropriate
- Use h3 subsections within sections if content is substantial

**Adapt to PR type:**

- **Feature PRs**: Summary + Design with rationale
- **Bug fix PRs**: Summary + root cause analysis
- **Chore/docs PRs**: Summary only, keep brief

**Avoid:**

- Bullet points in Summary
- Listing files changed (GitHub shows this)
- Internal planning concepts ("Phase 1", "Step 2")

## Instructions

1. Check context above; ensure working tree is clean
2. If commits behind main > 0, rebase first: `git rebase origin/main`
3. **Read the full diff** (`git diff origin/main..HEAD`) before writing the PR description. The `--stat` above is not sufficient - you must see the actual code changes.
4. Push if needed: `git push -u origin <branch>`
5. Create PR: `gh pr create --title "..." --body "..."`
6. Return the PR URL to the user

If updating an existing PR, push the new commits and update the PR body with `gh pr edit`.
