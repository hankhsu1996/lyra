# A part of a variable is named by the steps that reach it

Date: 2026-10-10 Status: accepted. Revises
[update-events-are-per-variable](update-events-are-per-variable.md) D1 and D4: an update is still
raised at the declared variable, and it now says which part of the variable it reached. The bits
rule of [a-write-compares-what-it-reached](a-write-compares-what-it-reached.md) stands inside a
packed value.

## Context

Two `always_comb` procedures that each give a different element of one unpacked array a default and
then override it never settled:

```systemverilog
always_comb begin p[0] = '0; p[0] |= a; q[0] = p[0]; end
always_comb begin p[1] = '0; p[1] |= b; q[1] = p[1]; end
```

LRM 9.2.2.2 allows it by name, and LRM 9.2.2.2.1 puts on each list the longest static prefix of what
the block reads, leaving out what it also writes, so neither procedure waits on any of `p`. Only a
part of a packed value could be stated to the run, as bits. A read of `p[0]` was a read of `p`, and
a write of `p[0]` could not be taken out of it, so each procedure waited on the whole array and each
default was a change the other was woken by. Stages held in one array, each reading the one before
it, fail the same way without any part being both read and written.

An event expression compares its own result (LRM 9.4.2) and is unharmed by being reevaluated more
often. An implicit sensitivity has no result to compare: being woken runs its body, and a body that
gives a default first writes a change each time it runs.

Cost is the second half. Every wait on a variable stood on one list, walked whole by every write. A
chain of `always_comb` stages through one array, driven 200 times, took 0.02 s at 256 stages, 0.52 s
at 1,024 and 8.37 s at 4,096 on the execution backend.

## The test each choice answers to

1. A process waiting on part of a variable is woken by a write exactly when the write reached some
   of that part.
2. A write pays for what it wrote and for the waits on what it wrote.

## Decision

**D1. A part of a variable is named by the path of steps that reach it from the variable.** A step
is a member of an unpacked structure or an element of an unpacked array (LRM 7.2, 7.4), and it is
stated as what the step already has: the member's index, the element's ordinal. The empty path is
the whole variable. Two paths are about the same storage exactly where one is a prefix of the other.
A wait, a write, a reference bound to a part (LRM 13.5.2) and a procedure's report of what it reads
and writes all name a part this way.

**D2. Inside a packed value a part is a range of its bits.** A packed value is contiguous bits, and
two selects of one may overlap partly (LRM 7.4.1, 11.5.1), which a range decides and steps do not.
So a path ends at a packed value, and what is named inside it is the bit range it always was. A
slice of an unpacked array (LRM 7.4.6) is named the same way one level up: the path ends at the
array, and what is named inside it is a range of its elements, so two slices of one array overlap or
do not as two part-selects of one vector do.

**D3. No layer states where a part lies.** HIR states the select the source wrote. MIR states a
place as a variable and steps; a wait's leaf and a report entry are handed the steps, and a step
taken within a write or on a reference is handed nothing more than it needs to reach its part.
Neither backend numbers anything, and the two need not agree on a layout.

**D4. A path is a value of bounded size.** It holds a fixed number of steps. A step past that bound,
one too large to hold, and a select no step stands for -- an index into an associative array (LRM
7.8) -- end the path there and make it inexact: the part meant is the one the path reaches or
something inside it. An inexact path only ever covers more than the source wrote, so a wait named by
one misses no write and a write named by one misses no wait. What it costs is the other direction: a
procedure's own write is taken out of its reads (LRM 9.2.2.2.1) only where its path is exact, so
past the bound a procedure is run once more by a write of its own that the standard leaves off its
list, where that write lands after the procedure has stopped, as a nonblocking assignment's does. A
blocking one lands while the procedure runs, when it waits on nothing, and runs it no further. A
reference into a queue is inexact from the queue on for another reason: the element it names keeps
no ordinal while others are added and removed around it (LRM 7.10).

**D5. A variable keeps its waits on a tree shaped like its parts.** The waits on the whole variable
are at the root, and the waits on a part at the node its path leads to, each node made when a wait
first needs it and kept. A write that changed something tells the waits on the part it reached what
it did to that part, tells the waits on everything the part lies in which element or member of
theirs it went into, and tells the waits on everything lying in what it changed only that they were
reached. Waits on any other part are not visited. A variable no wait names a part of holds one list
and an empty place for the tree, which is one pointer more than it held before.

## Rejected

- **Number every part on one line and compare intervals.** One test would answer a part, what it
  lies in and what lies in it alike, and nothing in the runtime would change shape. But a line has
  to exist. Each step needs the extent of its type, which the library's own code has no types to
  read it from; an element of a queue or a dynamic array has no place on the line; and where a
  backend holds a large array apart from the variable holding it, the line is a coordinate kept
  beside storage that nothing else reads. Linux's mmu interval notifiers and nvc's signal nexuses
  take this shape because what they divide is one run of addresses or of scalars.
- **One list per variable, each wait carrying what it reads.** What stood. It answers the first test
  and fails the second, as measured above.
- **Cut the variable into pieces at the boundaries waits use, each piece with its list** (nvc's
  `split_nexus`). A wait on the whole of a variable cut into k pieces joins k lists.
- **Leave the granularity and take only the block's own writes out.** It settles the reported case
  and leaves stages in one array unsettled.
- **Compare a procedure's writes when it ends and report only the net change.** A blocking
  assignment is an update event when it is made (LRM 4.9.3), and a process waiting on the part is
  owed the value in between.

The shape taken is the one the field uses where the parts are a hierarchy: the DOM's mutation
observers are listed on the node they name and a mutation walks its target's ancestors
(dom.spec.whatwg.org, "queue a mutation record"); PostgreSQL's predicate locks are held at one level
of relation, page and tuple and a write probes every level (`predicate.c`,
`CheckForSerializableConflictIn`); rustc names a place as a local and a list of projections and
tests two for conflict by walking them from the root (`places_conflict`). The condition that differs
is that a write here may cover a whole part with waits inside it, so the walk also goes down.

## Consequences

- A write to one element of an array visits the waits on that element, on the array and on whatever
  the element holds, and no other. The chain above runs in 0.00 s, 0.03 s and 0.12 s.
- A write through a reference bound to a part lands where a write naming the part would.
- A whole assignment that changed one element reaches every wait inside the variable, because the
  whole write compares the whole.
- The parts of a variable that some wait named stay kept for the variable's life, and from the first
  such wait on every write to the variable asks whether anything waits where it landed.
