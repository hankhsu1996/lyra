# Documentation Style

Contract for writing and maintaining docs under `docs/`, and for the reader-facing `README.md` files
that sit above them. This file covers content and structure rules only. Formatting is enforced by
Prettier; see the Formatting section at the end.

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
8. **Every document has a level, and a fact belongs at the level where it stays true longest.** A
   lower-level fact borrowed into a higher-level document is the most common way docs rot, because a
   document is revisited on its own cadence and not on the cadence of whatever it quoted. The test
   before writing a sentence: would an ordinary change one layer down make this false? If so it
   belongs one layer down, and this document should point at it instead of restating it.

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

### A forbidden shape is tested per item, never per bullet

Each doc states the test its own forbidden shapes answer to -- which property of the identity in
Purpose does this break. Apply it to **each thing a bullet names**, not to the bullet. A bullet that
names several things and gives one reason bans whatever the reason justifies plus whatever happened
to be listed beside it, and the difference is invisible afterwards: a reader meets one sentence and
the ban reads as settled.

This is not hypothetical, and it is the common shape rather than the rare one -- of the forbidden
shapes across `architecture/`, better than a third name more than one thing in their opening
sentence. One banned drop elaboration alongside move semantics and borrow checking, under the reason
that the layer does not reconstruct a source language's ownership discipline. True of the other two;
drop elaboration is how any compiler decides where to release storage, and both IRs that doc names
as its peers do it. It stood for months and was found only when it blocked work.

So: if the reason cannot be re-derived for a listed item on its own, that item belongs in its own
bullet with its own reason, or it does not belong in the list.

### When status genuinely has to be stated

Principle 5 bans "not yet" from an architecture doc, and a contract for something unbuilt still has
to say so somewhere or it reads as a description of the code. The resolution is placement, not
vocabulary: **the admission goes in one paragraph directly under the title, scoping the whole
document, or it does not appear at all.** `architecture/incremental_build.md` is the worked example
-- it opens by saying the compiler does not realize the contract yet and that the rules bind every
layer regardless, then never mentions status again.

What rots is the other shape: one status clause buried in a bullet, in a section about something
else. Nothing forces a reader past it, so it survives every refactor that falsifies it. A doc that
needs more than its opening paragraph to describe what is unbuilt is describing progress, and
progress is tracked in issues.

The same placement rule covers mechanism. An architecture doc names file layouts, type spellings,
and command names only under Notes / Examples, prefixed as the current implementation -- never in an
invariant or a forbidden shape, which must survive the next rename.

### What is machine-checked

`tools/policy/check_docs.py` settles the claims a machine can settle: a repo-rooted path cited in
any doc exists, a relative link resolves, an index lists every document in its directory, no cadence
vocabulary (`in this cut`, `a later cut`) appears in a timeless doc, no reader-facing README states
a count, and no permanent doc cites a work issue. It deliberately checks nothing about whether a
contract is the right contract or a stated capability is real -- that needs a reader, and it is what
a reader's attention is for.

Its known gaps, all of the same kind: a bare filename (`test.yml`) is not repo-rooted, so no rule
confirms it exists; the other words principle 5 bans ("currently", "transitional") carry legitimate
uses; and a count spelled as a word ("two sample designs") reads exactly like a structural one ("two
paths out of MIR"), which is a distinction only a reader can draw. Each is a place where the regex
would cost more in false alarms than it saves.

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

### A Doc Owes a Diagram Where Its Subject Is a Graph

Prose is linear, so a reader rebuilds it into whatever shape it describes. Where that shape is also
linear -- a sequence of stages, a classification, a list of rules -- the rebuilding costs nothing
and a diagram only says it twice. Where the shape is a **graph**, and which edges exist is the
content, the rebuilding is work every reader repeats and gets wrong: what a reader needs is which
arrows are there and which are not, and that is what prose states worst.

So a doc whose subject is a set of things with edges among them owes a picture of those edges, and a
doc whose subject is an order, a taxonomy, or a rule owes none. Where the doc already draws the same
structure another way -- pseudocode of the same recursion, a table of the same relation -- it has
paid the debt, and adding a second picture states one thing twice.

A diagram is held to the rule below on what a picture carries: the edges, never a judgement about a
box. It is also held to being true. A picture of a structure the code produces is checked against
what the code produced, not against the prose beside it -- an edge left out reads as an edge that
does not exist, which is the one error a reader cannot catch.

## Architecture vs Other Docs

Architecture docs under `architecture/` are the source of truth for the system's target shape. The
dependency between doc kinds is strictly one-way, from permanent to ephemeral:

- `architecture/` defines the system.
- `decisions/` records dated decisions with rationale. Entries may reference architecture docs and
  other decisions.
- `glossary/` defines terminology used by architecture docs.
- The delta between current code and the target is tracked in GitHub issues. It is the most
  downstream kind: an issue may cite architecture and decisions, never the reverse.

An architecture doc that cites a decision, a queue, or a working doc is a violation of this rule. A
decision or glossary doc that cites a work issue is the same violation: a permanent doc must not
depend on an ephemeral queue. It states the permanent fact ("not yet implemented") instead. A merged
pull request, or another project's issue, is history rather than a queue, and a record may cite one
as the evidence it is.

### The README is the top of that stack

A reader-facing `README.md` sits above every doc kind above, which by principle 8 makes it the
document least free to borrow. These belong to lower layers and never appear here:

- **A count of anything the repository contains.** Case totals, backend coverage ratios, file
  counts. Such a number is wrong the next time someone adds one of the thing counted, nothing forces
  its update, and publishing it as a measure of scope makes it a target, which collides directly
  with the rule that cases group by feature rather than by assertion. State that coverage is
  measured and point at where.
- **An inventory of supported constructs.** `conformance/` and the code are the source of truth for
  what is supported, and a parallel inventory rots. A capability table at the top goes stale on
  every feature merge.
- **SystemVerilog at the syntax level.** Naming a specific keyword or net type in the README is a
  detail-layer fact in the highest-level document. Write about what the tool is and what it handles
  in categories; leave the constructs to the corpus. The source a demonstration runs is the one
  exception, since it is the input to a command and claims nothing about what is supported.
- **A description of what a directory contains, when that directory has its own README.** Link to it
  and say why a reader would go there. Listing its contents duplicates a document whose whole job is
  to describe itself, and the copy is the one that goes stale.

What does belong is what stays true across releases, written for someone deciding whether to try the
tool: what it is, how to build it, the first commands they run and what those print, what it reads,
where to ask, and where to look next. What the project optimizes for and the shape of its pipeline
are for someone working on it, and the README links them in the architecture docs rather than
stating them. A tool with no site of its own has only its README for a front door, so it opens as a
user's tool does (uv, Deno, Yosys), not as a toolchain whose README is a signpost to a site (LLVM,
Rust).

A diagram is held to the same rule. It shows structure, so it carries only what changes when the
structure changes. A judgment about a component -- which one is the product, which is transitional,
which is deprecated -- changes on its own schedule, and putting it in a box makes the diagram rot on
a strategy call rather than on an architectural one. Such judgments go in the prose beneath, where
one sentence is cheap to revise and a reader already expects an opinion. The same goes for grouping
boxes: group only when every box groups on one axis, since two taxonomies drawn as peers read as
one.

A demonstration shows the workflow, never the language: the commands a user runs on a design of
their own, each with the output it really prints, taken from a run rather than written by hand. An
example that is about the tool does not read as a toy at any size -- Deno's is three lines, uv's is
a console session -- while one that teaches the input language would; so the source is as small as
the workflow allows, and a design at the project's real level is linked in `examples/` rather than
shown.

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
  "rendering". Use **backend** for a MIR consumer that emits executable artifacts, named in prose as
  the C++ backend and the LLVM backend rather than by their namespace spelling. Dump and backend are
  distinct surfaces; do not conflate them.

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
