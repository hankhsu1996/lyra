# Documentation Style

Contract for writing and maintaining docs under `docs/`. This file covers content and structure
rules only. Formatting is enforced by Prettier; see the Formatting section at the end.

## Core Principles

1. **Contracts over descriptions.** Architecture docs define what a subject is, owns, and forbids.
   They do not describe what the current code happens to do.
2. **Concise over complete.** Capture decisions and rationale. Skip implementation detail.
3. **One topic per document.** If a doc covers unrelated concerns, split it.
4. **Integrate, don't append.** Each edit reorganizes the document so new content lands in the right
   section. Never bolt a new section onto the end to avoid rewriting.
5. **Target state, not history.** Architecture docs describe the intended permanent shape. No
   "currently", "historically", "not yet", "migration", "transitional", or phase/cut references in
   architecture docs.
6. **Explicit invariants and forbidden shapes.** Every architecture doc states what must hold and
   what is not allowed. Implicit behavior is a defect.
7. **Templates are tools, not laws.** The type-contract template (Purpose / Owns / Does Not Own /
   Core Invariants / Boundary / Forbidden Shapes / Notes) fits subjects whose architecture is well
   described by what types live in a layer and what shapes are forbidden. Behavioral, protocol, or
   decision-cluster subjects find their own structure. Consistency comes from copying close existing
   examples, not from forcing every subject through one section list.

## Architecture Docs Are Contracts

Every architecture doc is a binding contract for its subject: what holds, what is forbidden, what
the boundaries are. Contracts are not narrative.

A doc whose subject is a type-contract layer (HIR, MIR, LIR, hierarchy, identity, ownership) uses
the template in the next section. The template forces the right discipline for those subjects.

A doc whose subject is runtime behavior, a protocol between subsystems, or a cluster of related
decisions finds its own structure. Open with the model, then state the decisions. Each decision
earns its space with its reason; abstract principles without consequences do not.

In both cases the rules under Core Principles still apply: no narrative framing, no "currently /
historically / not yet / migration", no platitudes that are trivially true of any reasonable system.

### Type-Contract Template

A type-contract doc contains, in order:

1. Title
2. Purpose
3. Owns
4. Does Not Own
5. Core Invariants (at least three concrete invariants)
6. Boundary to Adjacent Layers
7. Forbidden Shapes (concrete, not abstract warnings)
8. Notes / Examples

Sections may be short, but none may be omitted. An empty section signals that the contract is not
yet defined and must be filled before merge.

### Behavioral and Decision-Cluster Docs

These docs do not follow a fixed section list. Copy the shape of `architecture/scheduling.md` if the
subject is similar (a runtime mechanism with a handful of load-bearing decisions). Otherwise let the
subject drive the structure: state the mental model up front, then walk the decisions, each with the
reason and any rejected alternative that is non-obvious.

## Architecture vs Other Docs

Architecture docs under `architecture/` are the source of truth for the system's target shape. The
dependency between doc kinds is strictly one-way, from permanent to ephemeral:

- `architecture/` defines the system.
- `decisions/` records dated decisions with rationale. Entries may reference architecture docs and
  other decisions.
- `glossary/` defines terminology used by architecture docs.
- `progress/` tracks the delta between current code and the target. It is the most downstream doc
  kind: it may reference architecture and decisions, never the reverse.

An architecture doc that cites a decision, a queue, or a working doc is a violation of this rule. A
decision or glossary doc that cites a `progress/` file is the same violation: a permanent doc must
not depend on an ephemeral queue. Because nothing permanent points at `progress/`, a completed
progress file can be deleted with no dangling pointer left behind.

## Editing Discipline

- Read the entire document before editing.
- Place new content in the section it belongs to. Restructure the document if no existing section
  fits. Do not add "add-on" sections that bypass the contract template.
- Delete stale content. Outdated docs are worse than missing docs.
- When an architecture contract changes, update the doc in the same change that changes the code.
  Docs and code must not drift.

## Terminology

- Use terms from `glossary/` consistently. Do not invent synonyms.
- Prefer **compilation unit** over "module" when referring to the compilation boundary. Module is
  one kind of compilation unit.
- Prefer **object graph** when referring to hierarchy or navigation.
- Use **compilation-unit-local** (consistently) when referring to local identity scope.
- Use **dump** for the readable textual serialization of HIR or MIR, not "projection" or
  "rendering". Use **backend** for a MIR consumer that emits executable artifacts (e.g.,
  `backend::cpp`). Dump and backend are distinct surfaces; do not conflate them.

## Anti-Patterns

Do not:

- Append notes, logs, or prior-discussion history to a doc instead of restructuring it.
- Describe "how to implement" instead of "what the system is". Design docs explain the contract;
  they do not walk through code.
- Embed large code blocks. Use minimal pseudocode only when it is the only way to remove ambiguity
  in an invariant or forbidden shape.
- Duplicate content across docs. Pick one canonical location and link to it from others.
- Introduce implicit behavior that is not captured in invariants or forbidden shapes.
- Reintroduce a forbidden shape indirectly (under a new name, via a side table, through a wrapper,
  as a "cache" that becomes authoritative).
- Use narrative framing: "we decided to", "historically", "eventually we will".
- Use section separator comments, decorative dividers, emoji, or ASCII art headers.

## Decisions

Entries under `decisions/` record a dated decision with its rationale. They are the only docs
permitted to describe how a decision was reached. Superseded decisions are linked from the entry
that supersedes them.

## Glossary

Entries under `glossary/` are definitions. Each entry gives a single authoritative meaning for a
term. If a term has meaningful nuance, split it into distinct terms with distinct entries. Never
overload a single entry with multiple meanings.

## Formatting

Markdown formatting is enforced by Prettier. The writing and structure rules above are content
contracts; Prettier owns whitespace, wrapping, list alignment, and table formatting.

- Do not manually align tables, spacing, or wrapping.
- Run `npm run format` before committing.
- CI runs `npm run format:check` and rejects unformatted docs.

Writing rules and formatting rules are separate. This document covers content and structure only.
