# Code Quality

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. Q1, Q2). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for cross-cutting code quality, infrastructure, and refactoring concerns.

## Progress

- [x] Unify compilation-side output under one driver-owned boundary
- [ ] Q1: flush-owned vs shared baseline ownership enforcement in subscription subsystem
- [ ] Q2: typed edge-group location across subscription subsystem
- [ ] Q3: dense subscription removal and backpatch abstraction
- [ ] Q4: unified subscription lifecycle boundary

## Q1: Flush-owned vs shared baseline ownership enforcement

The subscription subsystem has two categories of baseline state that look similar but have different ownership rules. The edge transition baseline is flush-owned and must not be written outside the flush phase. The change detection baseline is a shared snapshot that both flush and refresh legitimately write. These two categories are not enforced by any API or type boundary -- they are flat fields on the same structs. This caused the edge-delivery bug where the refresh path wrote the transition baseline before flush could detect the transition.

Target direction: separate ownership categories at the type or accessor level so the same class of bug cannot recur. Look at the subscription header and the write sites across the install and flush files.

## Q2: Typed edge-group location across subscription subsystem

Edge subscriptions are addressed by a raw 3-coordinate tuple (slot, group index, polarity bucket). This tuple is passed through subscription references, target handles, removal functions, and rebind logic across both the install and flush files. No typed wrapper exists, so coordinate misuse is easy.

Target direction: a typed location wrapper in the subscription header. This spans install, flush, observability, and the subscription type definitions -- not addressable as a single-file cleanup.

## Q3: Dense subscription removal and backpatch abstraction

Four removal functions in the subscription install file implement the same swap-and-pop pattern with subscription-reference backpatching. Two of those also backpatch into the edge target indirection table. The same removal primitive is called from the flush file during rebind group migration. The pattern is replicated rather than abstracted.

Target direction: a shared removal primitive or storage pattern that encodes the backpatch invariant once. This spans the install file, flush file, and storage file.

## Q4: Unified subscription lifecycle boundary

Subscription install, refresh, rebind, removal, and flush are split across three implementation files with shared low-level primitives crossing file boundaries. The file split does not correspond to a clean API boundary -- removal helpers defined in the install file are called from the flush file, and refresh logic in the install file reads flush-owned dirty-slot state.

Target direction: a documented internal contract or explicit API layer that clarifies which operations are shared primitives vs phase-local. This is a subsystem-level boundary question, not a single-file refactor.
