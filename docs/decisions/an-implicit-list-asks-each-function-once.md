# An implicit list asks each function once

Date: 2026-10-04 Status: accepted. Revises [read-set-inference](read-set-inference.md) for a
procedure's own list, and with it the second immediate change of
[front-end-semantic-boundary](front-end-semantic-boundary.md); extends
[a-function-reports-what-a-call-of-it-reads](a-function-reports-what-a-call-of-it-reads.md) to the
implicit list.

## Context

An `always_comb` or `always_latch` is sensitive to what its block reads and to what "any function
called within the block" reads, less what the block or those functions write, and the clause
analyzes a hierarchical function call and one from a package "as normal functions" (LRM 9.2.2.2.1).
Each function therefore contributes three things -- what it reads, what it writes, which functions
it calls -- and none depends on the values of its arguments, since a class object contributes
nothing to the list. The list is fixed.

The front end answers that list for the whole elaborated design at once, reading every called
function's body wherever it is declared. That is the answer the procedure's unit compiled, so its
artifact depended on another unit's body, which units compiled on their own may not (north star 4
and 5). It also could not be routed: the front end states a read inside a called function as the
storage it landed on in one elaboration, named in the function's own text, and a function of another
instance -- reached through an interface port or by a name climbing out of the procedure's instance
-- stands somewhere that text says nothing about from the procedure's side. Those calls were
refused, while the same call in an `@(...)` worked, because a wait asks the function instead.

Two conditions shape the answer:

- **Which instance a hierarchical or port-reached call lands on is fixed only once the design is
  built**, not when any one unit compiles.
- **The list does not move afterwards.** A wait collects its leaves again on every candidacy because
  it follows class handles, which change; an implicit list follows none.

## Decision

**A procedure states what its own text reads and writes, less what that text declares, and each
function call it makes. Once, ahead of its first run, each call reports what its function reads and
writes where the call reaches it; what anything wrote is taken out of what was read, in that one
place, each place is listed once, and the procedure waits on what is left from then on. Every
implicit list is collected this way; where the procedure writes and calls nothing, what is collected
is what its text reads.**

- This holds wherever the function is declared, its own instance included. Keeping the front end's
  whole-procedure list where every function stood in the procedure's own instance was a second
  analysis answering the same question, and the reason to keep it -- that collecting costs something
  -- does not hold for a list collected once.

- A function's report is the one a wait already asks, now stating what the function writes beside
  what it reads, and keeping apart what is reached through a handle: a class object, a variable of
  the instance a virtual interface holds, and everything a call made on either reports. A wait
  watches both and ignores the writes (LRM 9.4.2 leaves nothing out); an implicit list takes only
  what was read directly. So the clause's "method calls of class objects do not add anything" holds
  however deep the call is, in the block or in a function it calls, and no text has to know which of
  the two is asking.
- A write is stated only where it compares exactly with a read: a run of a packed value's bits, or
  the whole of anything else. A write of one element of an unpacked array is not stated, because the
  array is watched whole and taking the whole away would miss a change to an element nothing wrote.
  A write of a whole place takes out every read of it; a write of some bits takes those bits out of
  a read naming bits.
- The survey: slang composes a per-function summary at the caller with the whole design in memory
  (`AnalysisManager::getFunctionValUses`); Verilator orders one flattened netlist (`V3Order`); LLVM
  ThinLTO has each module write a summary per function into its own output and combines summaries,
  never bodies, at link (`ModuleSummaryIndex.h`, `FunctionSummary`). Ours is ThinLTO's shape,
  combined when the design is built rather than at link, because that is when a call's target is
  fixed.

## Rejected

- **The procedure's unit reading the callee's body** -- the front end's whole-procedure list.
  Another unit's body in this unit's artifact, and not routable from the procedure's side; kept only
  for functions of the procedure's own instance, it is a second analysis of one question.
- **The callee publishing what it reads in its signature.** A signature is derived from
  declarations, and a body edit would then re-emit every unit calling it.
- **Collecting the report every time the procedure waits**, as a wait on an expression does. That
  pays for following handles the list does not follow.

## Consequences

- Every such procedure collects its list once at time zero, then waits on it as any other implicit
  list does.
- Measured on Ibex running its hello program to the software's `$finish` (execution backend,
  `--release`, callgrind): 14,960,817,187 instructions before, 14,092,511,014 after (5.8% fewer),
  with the same console output, `$finish` at 26548 and an identical instruction trace. Listing each
  place once is what makes it a saving: a first measurement without it was 15,187,749,797, a place
  two reads reached being tested twice on every write.
- A call through a virtual interface inside such a block adds only its arguments, as a call made on
  a class object does; the front end treats it the same way. A list collected once could not follow
  the handle as it changes in any case.
