# Progress

Files in this directory track **in-flight features** -- pieces of work large enough to span multiple
PRs, where a checkbox list of sub-steps is needed to coordinate what is done and what is pending
across sessions.

The canonical case: a feature that breaks into ten or so PRs, each PR closing one or more checkbox
items. The file is created when work on the feature begins and is deleted when the last item lands.

## What Belongs Here

- A feature whose implementation breaks into several PRs with identifiable sub-steps.
- A planned workstream with a known target shape that is queued to be picked up next, written out as
  a checkbox list so the entry point is clear when work begins.

## What Does Not Belong Here

- Inventories of supported SystemVerilog constructs. The source of truth for "does X work" is
  `tests/cases/` and the code itself; a parallel inventory rots.
- One-shot bug fixes or single-PR changes. Those land directly without a tracking file.
- Wish lists or roadmaps with no concrete driver and no defined end state.
- Past decisions and their rationale. Those belong in `../decisions/`.

## File Naming

`kebab-case.md`, one file per feature or workstream. The name describes the work, not its phase or
status.

## Entry Contents

Each file contains:

- A short header stating the feature's scope and its defined "done" state.
- The checkbox list of sub-steps, with an item flipped to `[x]` in the same change that lands its
  code -- the working tree before commit included, never a follow-up change.
- Any blockers, open design questions, or cross-references to architecture contracts the work must
  satisfy.

Write each item at the semantic level -- the problem and the target shape, not the field names,
helpers, or keywords, and not the full rationale (that lives in `../decisions/`). A well-written
item reads the same before and after the work, so completing it is flipping `[ ]` to `[x]` and
nothing more -- never a rewrite into a past-tense account of what landed. The resulting behavior is
recorded in the code and the decision doc; that the work happened is recorded in git.

When the last item lands, the file is deleted, and any cross-reference to it from another progress
file is updated in the same change so no dangling pointer remains. Only another progress file can
hold such a pointer: architecture, decision, and glossary docs are forbidden from referencing a
progress file (see `../style.md`), so deletion never strands a permanent doc. The git history is the
record that the work happened.
