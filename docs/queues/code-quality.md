# Code Quality

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. Q1, Q2). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for cross-cutting code quality, infrastructure, and refactoring concerns.

## Progress

- [ ] Unify compilation-side output under one driver-owned boundary

## Unify compilation-side output under one driver-owned boundary

The compilation side has no single output boundary. Human-facing output is split across phase/progress logging, per-subsystem stats printers, diagnostic/error printing, backend-side analysis logging, and execution-side progress printing. Lower layers own output policy decisions that belong to the driver. The forwarding-analysis logging in the LLVM backend is one symptom; the broader problem is that there is no canonical compilation output boundary for all human-facing text.

The gap: establish one driver-owned boundary that owns output policy, rendering, and sink emission. All lower layers return structured data or emit structured progress events and never print directly. Diagnostics are part of the same boundary, not a parallel system. The completed shape must also enforce this boundary mechanically: direct human-facing output is forbidden outside the designated driver-owned boundary files.
