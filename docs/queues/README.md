# Queues

Canonical location for operational planning documents that drive development priority.

## What is a queue

A queue tracks a multi-step, long-lived development stream that has its own prioritization. It answers: what is the target, what gaps remain, what is next, what is done.

Queues are **operational** -- they drive what gets built and in what order. They are distinct from:

- **Design docs** (`docs/*.md`) -- describe how things work
- **Language gaps** -- tracked as queue items in `language.md`
- **Scratch notes** (`playground/`) -- rough thoughts, temporary exploration, not yet accepted as project work

**Rule**: if it drives real development priority, it must be a tracked queue here. If it is just exploration, it stays in `playground/`.

## Item convention

Each queue file is a working queue. Items are gaps that need work.

**Progress section at the top.** Every queue starts with a checkbox list. Unchecked = gap. Checked = done. This is the at-a-glance view of the entire stream.

**Finished items get removed.** When a gap is fully closed, remove its section from the file. Keep one checked line in the progress list with a short note (e.g., PR number) so we know it happened. Do not keep detailed "completed" write-ups -- the git history and PR descriptions have that.

**Items describe gaps, not implementations.** Each item says what the gap is, where to look (module or file level), and why it matters. No function names, variable names, line numbers, or code snippets. The item should carry enough context that someone starting fresh can investigate and arrive at the same conclusions.

**No investigation logs.** Investigation notes, profiling breakdowns, and design explorations belong in conversation history or plan files, not in the queue. The queue tracks what remains to be done, not how we figured out what to do.

**Reference data is separate.** If a queue needs profiling baselines or measurement tables for prioritization, put them in a clearly labeled reference section below the working items.

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

## Current queues

| File                | Stream                                     |
| ------------------- | ------------------------------------------ |
| `specialization.md` | Specialization-based compiler architecture |
| `performance.md`    | Runtime simulation performance             |
| `observability.md`  | Debugging, tracing, and inspection tooling |
| `infrastructure.md` | Correctness, code quality, and tooling     |
| `dpi.md`            | DPI-C foreign function interface           |
| `assertions.md`     | SystemVerilog assertions (LRM 16)          |
| `language.md`       | General SV language feature gaps           |
