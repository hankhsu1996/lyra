---
description: Create a pull request with a well-formatted description
allowed-tools: Bash(git status:*), Bash(git log:*), Bash(git diff:*), Bash(git add:*), Bash(git commit:*), Bash(git push:*), Bash(git branch:*), Bash(git fetch:*), Bash(git rev-list:*), Bash(git rebase:*), Bash(gh pr create:*), Bash(bazel build:*), Bash(bazel test:*), Bash(clang-format:*), Bash(find:*)
---

# Pull Request

Create a PR following the project format.

Assumes the working tree is already clean and commits are made. If you need to format, lint, or commit, use `/commit` or `/commit-pr` instead.

## Context

- **Current branch:** !`git branch --show-current`
- **Git status:** !`git status --short`
- **Commits on this branch:** !`git log --oneline main..HEAD`
- **Full diff from main:** !`git diff --merge-base origin/main HEAD --stat`
- **Commits behind main:** !`git fetch origin main --quiet && git rev-list --count HEAD..origin/main`

## PR Format

**What makes a PR good is your judgment, not this file.** What follows is the house style plus the
facts about GitHub and the shell that are not visible from here. Everything else -- which sections
exist, how the argument is built, what deserves a table or a diagram -- is yours to decide for the
change in front of you.

### House style

- Title starts with a verb, capitalized. No colon format ("Fix: xxx").
- Body opens with `## Summary` as a paragraph, not bullets.
- Backticks for signal, module, and symbol names.

### Facts you cannot see from here

- **A title over ~64 characters is truncated** in GitHub's PR list, which is where people scan. Cut
  qualifiers, keep the subject. Paraphrase code identifiers rather than pasting symbol names.
- **A paragraph must be one line in the source.** GitHub wraps it; a manual newline inside a
  paragraph renders as a hard break. Hard breaks belong only in code blocks and bullet lists.
- **Mermaid renders on GitHub.**
- **Length is the constraint that bites.** Aim for what a reviewer reads before opening the Files
  tab. Past roughly 50 lines, ask which section they would skip, and delete it.

### Leave out

- Files changed, and source the diff already shows -- GitHub renders both better, in colour.
- Internal planning vocabulary ("Phase 1", "Step 2").
- Time-sensitive state: TODOs, follow-up notes, other PRs' status, CI results. The description is a
  permanent record of the change as if already merged, not a snapshot of where the work is.

## Instructions

1. Check context above; ensure working tree is clean
2. If commits behind main > 0:
   - `git rebase origin/main`
   - Re-run clang-format (a rebase can drift C++ formatting against upstream):
     - `find src include tests -name '*.cpp' -o -name '*.hpp' | xargs clang-format -i`
3. **Run the gate** -- this is the tree that will land, and where step 2 rebased, it is one nothing
   has built or tested before:

   ```bash
   bazel build //...
   bazel test //...
   ```

   Do not narrow `//...`. Widen it to `--config=full` only when the change touches what the C++
   backend emits, since that target is the only thing that compiles emitted text. Fix failures and
   amend before pushing.

4. **Read the full diff** (`git diff origin/main..HEAD`) before writing the PR description. The `--stat` above is not sufficient - you must see the actual code changes.
5. Push if needed: `git push -u origin <branch>`
6. Create PR: write the body to a file and pass `gh pr create --title "..." --body-file <file>`.
   Inline `--body` does not survive shell quoting once the text contains a table or a fenced block.
7. Return the PR URL to the user

If updating an existing PR, push the new commits and update the PR body with `gh pr edit`.
