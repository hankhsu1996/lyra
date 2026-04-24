# Decisions

Logged architectural decisions. Each entry records a decision with its date and rationale. Decisions
are immutable once logged; superseded decisions are linked from the entry that supersedes them.

## File Naming

`YYYY-MM-DD-short-slug.md`, for example `2026-04-22-adopt-query-based-incremental.md`.

## Entry Contents

Each decision entry contains:

- Title
- Date
- Status (accepted, superseded by <link>)
- Context
- Decision
- Consequences

Decisions are the only docs in this directory permitted to describe how a decision was reached.
Architecture docs under `architecture/` describe the resulting permanent shape, not the process.
