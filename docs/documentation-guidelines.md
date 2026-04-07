# Documentation Guidelines

## Core Principles

1. **Concise over complete** - Capture decisions and rationale, not implementation details
2. **No code unless necessary** - If explanation suffices, skip the example. Use minimal effective examples when needed. Include minimal pseudo/code snippets when required to remove ambiguity in contracts or invariants.
3. **Integrate, don't append** - Each edit should reorganize, not just add to the end
4. **One topic per document** - If a document covers unrelated concerns, split it
5. **ASCII only** - Use `->` not Unicode arrows, `|` and `+` for diagrams, no special characters

## Audience

Consider who reads each document:

- **Design docs** (architecture, hir-design, mir-design) - Contributors who need to understand boundaries and constraints
- **Reference docs** (limitations, error-handling) - Anyone looking up specific behavior
- **Process docs** (this file) - Contributors who need to follow conventions

Write for the audience. Design docs explain _why_; reference docs explain _what_.

## Before Adding Content

Ask:

- Is this a decision/philosophy, or implementation detail? (Only the former belongs here)
- Does this duplicate existing content? (If yes, merge or link instead)
- Where does this fit in the current structure? (Never default to appending)

## When Editing

1. Read the entire document first
2. Find the right location for new content
3. Remove or condense redundant sections
4. Reorganize headings if structure has drifted
5. Delete stale content - outdated docs are worse than missing docs

## Cross-References

- Link to other docs rather than duplicating content
- Use relative paths: `[error handling](error-handling.md)`
- If the same concept appears in multiple places, pick one canonical location

## Design Documents

For documents describing system boundaries or layer responsibilities:

- State **hard rules** explicitly - constraints, not suggestions
- Include "what must NOT appear" sections - boundaries matter as much as contents
- Use guiding questions to clarify scope
- Prefer tables for taxonomies and exclusion lists
- **Architecture docs describe the target architecture, not history.** No "current state," "migration," "transitional," or queue references. The architecture is what we are building. Working queues track how we get there -- they reference the architecture docs, not the other way around.

## Architecture vs Working Docs

Architecture docs (`architecture-principles.md`, `compilation-model.md`, `natural-model.md`, `pipeline-contract.md`, `state-layout.md`, `runtime.md`, `module-hierarchy.md`, etc.) define the system we are building. They must not contain:

- "Current state" sections describing what exists today
- References to working queues (`queues/*.md`)
- Migration plans or transitional implementation notes
- Temporal language ("not yet," "remaining gaps," "will be replaced")

Working queues (`queues/*.md`) and investigation docs (`investigations/*.md`) track progress and reference the architecture docs as their source of truth. The dependency is one-way: queues depend on architecture, never the reverse.

## Anti-Patterns

- Appending discussion logs verbatim
- Long code examples that belong in code comments or tests
- Implementation details that will become stale
- Describing "how to implement" instead of "what it means"
- Duplicating content instead of linking
