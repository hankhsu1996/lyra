# Queues

Canonical location for operational planning documents that drive development priority.

## What is a queue

A queue tracks a multi-step, long-lived development stream that has its own prioritization. It answers: what is the target, what gaps remain, what is next, what is done.

Queues are **operational** -- they drive what gets built and in what order. They are distinct from:

- **Design docs** (`docs/*.md`) -- describe how things work
- **Limitations** (`docs/limitations.md`) -- describe what is unsupported (descriptive, not operational)
- **Scratch notes** (`playground/`) -- rough thoughts, temporary exploration, not yet accepted as project work

**Rule**: if it drives real development priority, it must be a tracked queue here. If it is just exploration, it stays in `playground/`.

## When to create a new queue

Create one when the stream is:

- Multi-step (not a single PR)
- Long-lived (spans multiple sessions)
- Has its own prioritization (items compete with each other, not just with other streams)
- Would otherwise become a messy subsection in another queue

Do **not** create a queue for one small refactor, one isolated bug, or a temporary implementation note.

## Naming rule

Every queue filename must be a **stable engineering area**, not an activity or phase:

- One noun, ideally
- Broad enough to survive growth
- Not tied to a temporary phase or implementation trick

Good: `specialization`, `performance`, `observability`, `frontend`, `diagnostics`

Bad: `migration`, `debug-gaps`, `runtime-performance-gaps`, `specialization-refactor`

## Template

```markdown
# <Engineering Area>

<One-line purpose.>

## North Star

<Target state. What does "done" look like?>

## Current Status

<Where things stand now. Measurements if applicable.>

## Active Gaps

<Ordered by priority. Each gap has: description, cost/impact, next steps.>

## Later

<Known gaps not yet prioritized.>

## Completed

<What has been done, with PR references.>

## Open Questions

<Unresolved decisions.>
```

Sections can be omitted when empty. Do not pad with boilerplate.

## Current queues

| File                | Stream                                     |
| ------------------- | ------------------------------------------ |
| `specialization.md` | Specialization-based compiler architecture |
| `performance.md`    | Runtime simulation performance             |
| `observability.md`  | Debugging, tracing, and inspection tooling |
