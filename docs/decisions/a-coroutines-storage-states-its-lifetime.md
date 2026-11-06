# A coroutine's storage states its lifetime

Date: 2026-11-01. Status: accepted. Revised 2026-11-05: calls that owe the same on the way out now
share a landing, so the exception slot has a writer per landing rather than one per call; the
markers this record decides do not depend on that.

## Context

The execution backend hands a process body to LLVM as a coroutine, and LLVM's coroutine split
decides which of the body's stack slots live across a suspension. LIR states when a frame's bytes
come into and go out of use, and the LLVM backend dropped those statements. LLVM then answered from
the slot's uses: for every pair of a slot's users it asked whether a path between them crosses a
suspension.

A slot read at every statement of a long body, or written by every landing, has one user per
statement, so the answer costs the square of the body. A process of 10,000 `if` statements, 50,071
blocks, took 237.93 s at `-O0`, nearly all of it in that check, and a test holding four chains of
3,000 operands was the longest in the merge gate.

The same design through the C++ backend took 10.59 s, most of it the front end, through the same
pass.

## Decision

**Lyra hands LLVM the lifetimes LIR states, and does not make LLVM work them out.** A coroutine's
slot that names storage realizes `storage_live` and `storage_dead` as `llvm.lifetime.start` and
`llvm.lifetime.end`, whatever the optimization level.

- **Every slot states its lifetime.** A slot comes into use where it is made, whatever its type, and
  a scope entry of its own owes its bytes from there. The end of a value it holds is a second entry
  above that one, as rustc schedules a local's storage and its value as two drops. So a way out
  while the slot is being initialized ends its bytes and no value, and a value moved out leaves the
  bytes owed.
- **Storage no way out ends has a start and no end.** The slot a landing writes what is unwinding
  into, and the storage a body's variables live in, last the whole body. LLVM keeps a slot with no
  end in the frame, which costs a pointer written only on the exceptional path.
- **A value a transient holds in frame bytes states nothing yet.** Its bytes come into use with the
  instruction making it, and clang marks a temporary there too. It has the few uses of its
  statement, so LLVM's own answer costs it nothing, and nothing has needed the marker.
- **What a handler receives states nothing yet.** Its slot has the few uses of its handler, so
  LLVM's own answer costs it nothing.
- **A plain function states none of this.** Clang does at any level but `-O0`, and no pass here
  needs it.

A `lifetime.end` is a promise, not a hint: LLVM may reuse the bytes after it. What LIR says about a
slot is therefore what the program's memory does, and a value still read after its slot's end is a
miscompile rather than a lost optimization.

## Survey

- **Clang** forces the markers for a coroutine: `CodeGenFunction::GenerateCode` reads "Coroutines
  always emit lifetime markers." That is why the C++ backend never paid this. Clang also shares one
  landing pad among the calls of a scope, so the slot it takes what is unwinding into has a writer
  per landing pad rather than one per call.
- **LLVM** `SpillUtils.cpp`, `AllocaUseVisitor`: a slot with lifetime starts is judged by whether a
  start reaches a suspension without passing an end, and the pair of users is the fallback when it
  has none.

## Rejected

- **Splitting coroutines in the execution backend, as rustc's MIR transform does.** Derived and
  reviewed for several rounds, on the reading that LLVM's split was quadratic in a way nothing could
  avoid. It replaces the coroutine lowering and its unwind and destroy contracts, adds a dominator
  analysis and a frame of our own, and departs from what clang does with the same program; the
  measured cost turned out to be one fallback that clang's output never takes. The rule that the
  execution backend does what clang does with the C++ rendering had the answer, and comparing the
  two compilers' output found it.
- **Fixing the pair of users in LLVM first.** A correct fix, and worth having, but nothing Lyra's
  output needs once it states lifetimes.
- **Merging landing pads as clang does, as the answer to this cost.** Calls owing the same do now
  share a landing, which saves code and writers of the exception slot, but the markers do not depend
  on it.

## Limits

- LLVM's split keeps, for every block, a bit set as long as the function, so its memory grows with
  the square of a coroutine's blocks: 627 MB at 50,071 blocks. Its time does not suffer from this. A
  process is a coroutine only if it stops somewhere other than at the head of its loop, so what
  reaches this is long code that also waits; Ibex's largest coroutine has 125 blocks.

## Consequences

- The 10,000 `if` process took 2.55 s with lifetimes, before it stopped being a coroutine at all
  (`a-process-stopping-only-at-its-loop-head-is-called.md`). On Ibex nearly every coroutine's frame
  grew by the one pointer of the exception slot, and the total frame bytes fell.

## Cross-references

- `a-process-stopping-only-at-its-loop-head-is-called.md` -- which processes are coroutines.
- `a-wait-is-storage-of-its-activation.md` -- where a wait's storage stands.
