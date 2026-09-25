# A value lives in the frame of whoever made it, and ends where clang would end it

Date: 2026-09-24 Status: accepted. Supersedes the lifetime half of
[jit-value-realization](jit-value-realization.md) and the transient half of
[activation-frame-and-transient-scope](activation-frame-and-transient-scope.md).

## Context

On the execution backend every value a runtime entry answered with was allocated by the runtime into
a region scoped to one runtime-to-generated call, and the region was released only when that call
returned -- for a process, at its next wait. So the memory a running program held grew with the work
it did between two waits, not with what it could still read. Measured on the LLVM backend: a loop of
function calls held 189 MB at 1e5 calls and 1.85 GB at 1e6, and a combinational block reading a
1024-element array held 582 MB for one pass and 930 MB for two. The C++ backend, running the same
programs, held 4.4 MB, because the C++ compiler puts each temporary in the caller's frame and ends
it at the end of its full-expression.

A cell read also copied the whole value into that region before anything selected from it, so
reading one element of a large array allocated the array.

## Decision

**A value is an object in the frame of whoever made it, built there by the callee, and ended where
the source's own structure says it stops being readable -- the rule C++ gives a temporary, carried
out the way clang carries it out.**

- **An entry that answers with a new value builds it in storage the caller passes.** The caller
  gives the storage and the entry constructs its result there and answers with that address, which
  is `sret` in all but name. A generated body answering a value takes the same trailing storage, so
  a call to a body of this program and a call to the library are one shape.
- **The caller's storage is a slot in its own frame, sized from the runtime object the value is.**
  Every value domain realizes one runtime type whatever source type it stands for, so a value's size
  and alignment are known per domain, stated once beside the domains, and asserted against the
  runtime's own types. The slot is allocated where the body opens, so a loop reuses it.
- **A value's end is stated below MIR, at the end of the full-expression that made it** -- an
  expression statement, a condition, a loop step, a declaration's initializer, a return -- and on
  every edge that leaves the extent early, a departure included. It is the cleanup stack a C++
  compiler keeps, and it is the same stack that already carried a guarded body's cleanup, so there
  is one mechanism for everything owed on the way out.
- **A frame slot of an owned type holds the object, as a C++ local of class type does.** A store
  into one ends the object it held and moves the new value in; the slot ends its object with the
  extent it was declared in. A value that is handed to a slot or returned leaves nothing to end
  where it was built; one that was only lent is copied first, so what is handed on is always owned.
- **A parameter the caller lends is copied into a slot of the callee's**, so the body may write it
  (LRM 13.5.1) without touching the caller's value.
- **What an entry answers with is a property of the entry**: a new value, the object it acted on (a
  guard that yields what it guards), or a part of that object. Only the first is owned and ended by
  the caller.

## Rejected

- **Keeping the region and releasing it at finer marks.** It keeps the runtime as the owner of every
  value the generated side makes, which is the shape `architecture/lifetime.md` rules out: a value's
  end read off the region it was allocated from rather than stated by the compiler. Every refinement
  then needs special cases for what the region cannot see -- a returned value outliving its mark, a
  value bound by a declaration outliving the statement that made it.
- **Frame slots holding a pointer to the temporary that initialized them.** The temporary's storage
  is reused the next time its instruction runs, so in a loop the slot and the next temporary are one
  object: an assignment building its new value would overwrite the old one before ending it.
- **Copying at the caller, as a C++ call by value does.** It is the better shape in one respect: a
  temporary argument then passes with no copy at all, where the callee's copy is paid for every
  owned argument of every call. It needs a parameter to be storage the body reaches through the
  argument rather than a slot of its own frame, which is a second way of binding a local, and on
  2026-09-24 the cost it would recover was small beside the whole-value copy every read of a
  variable made. Worth revisiting once reads stop copying.

## Consequences

- Memory follows what the program can read. The loop of calls holds 4.5 MB at a million calls as at
  a hundred thousand, and the combinational block 5.4 MB at two hundred passes against 5.6 MB at
  one.
- The runtime allocates no value on the generated side's behalf. What survives a suspension still
  lives in the activation's variables
  ([cross-suspension-value-storage](cross-suspension-value-storage.md)), which this decision leaves
  as they are.
- Both call directions share the out-storage convention: the runtime calls a closure's value body
  and an integral constant's initializer with storage of its own, and takes the value from there.
- A value boxed into the erased representation for one call lives in the caller's frame until that
  call returns, on both edges where the call is one a departure can leave.
- A null of a handle type is built like any other value and holds nothing, so its storage going away
  is the whole of its end.

## Cross-references

- `architecture/lifetime.md` -- an automatic value's end is emitted by the compiler, on every exit.
- [storage-owns-its-value](storage-owns-its-value.md) -- the same answer for declared storage.
