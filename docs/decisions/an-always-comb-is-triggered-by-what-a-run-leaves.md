# An always_comb is triggered by what a run leaves

Date: 2026-11-15. Status: accepted.

## Context

An `always_comb` that writes a default for each output and then its value changes that output twice
in one run. Every change of a variable is an update event, and a process sensitive to it when it is
executed is scheduled (LRM 4.3, 4.5, 4.9.3). So two such procedures that each read an output the
other defaults wake each other: each run moves the other's input away and back, and the other is
waiting when it does.

```systemverilog
always_comb begin            // reads dirty_rd_en, writes evict
  evict = 1'b0;
  if (ack) evict = 1'b1;
  if (dirty_rd_en) stat = 2'b01;
end
always_comb begin            // reads evict, writes dirty_rd_en
  dirty_rd_en = 1'b0;
  if (evict) buffered = 2'b10;
  if (req) dirty_rd_en = 1'b1;
end
```

No bit of either output depends on the other's, and the style -- a default for every output at the
top of the procedure -- is ordinary RTL. CVA6's write buffer has this shape, and OpenPiton stopped
on it.

The standard does not settle whether this program settles. LRM 4.7 lets the statements of two
processes interleave: in a schedule where neither procedure is waiting while the other writes,
neither sees the other's default, and the two settle. A schedule running each procedure to its stop,
which is how Lyra runs every process, has each waiting while the other writes, and they wake each
other for ever. Both are legal.

## Decision

**The wait of an `always_comb` or an `always_latch` is triggered by what an event's execution leaves
changed of what it reads, not by each write the execution makes.** An event's execution is what the
engine runs of one event before it gets control back (LRM 4.5): a process evaluated up to its stop,
one deferred update, one timer firing. A variable such a wait watches keeps, before an event's
execution first writes a part of it, the value the part held; once that execution ends, the value is
compared with what the part holds, and a wait its writes reached while the procedure was parked is
woken only where a part it reads was left changed. A value written and written back wakes nothing,
whatever its type. The two procedures above then settle, as they do in the interleaved schedule.

- **Each event's execution is judged on its own.** A change one process's execution makes and
  another's undoes still triggers the procedure, because the first left it changed; each nonblocking
  update is an event of its own too (LRM 4.6).
- **The value is kept by the write, at the part it lands on.** Only the typed storage a write lands
  on can copy and compare its value, and only before it writes does it still hold the old one. So
  the part a write lands on is kept -- an element, a member, each element a slice writes, the whole
  variable for a whole store -- once per event's execution and only where an `always_comb` or
  `always_latch` watches the variable, so what is kept costs what the write touched. A write to a
  variable nothing such watches asks one question at each part it lands on, and keeps nothing. A
  write that lands where no read reaches, past the last element (LRM 7.4.5), keeps nothing either.
- **A write of bits keeps the words they lie in.** A packed value is its bits (LRM 7.4.1), so a
  write of some of them -- an element of a memory declared packed, as older Verilog had to -- keeps
  the words of each plane those bits lie in, as one run of words that grows as a later write of the
  same event's execution reaches words outside it; a word outside the run was not written in that
  execution, so it is taken as it stands. A memory declared packed and written an element at a time
  then costs about what one declared unpacked does, writes far apart in one execution keeping the
  words between them, and the change is told by its bits. A value narrower than a word is that word,
  kept whole.
- **What is kept is found by its path.** The parts kept are ordered by their paths, so whether a
  write is held by one already, and which lie inside a part being kept, are each one look-up, and an
  event's execution writing many parts pays for each what it touched. A write of several elements --
  a slice -- is judged where each element it wrote is held, one look-up for each.
- **A part kept already holds what lies inside it.** A later write inside a kept part keeps nothing.
  A part whose own parts were kept earlier in the event's execution is kept with those put back for
  as long as it is copied, so it is kept as it stood before the execution; a part judged against a
  copy taken in its middle would read its own writes as a change, and two procedures each writing
  their output part by part and then whole would wake each other again.
- **An array whose size changes is kept whole.** A dynamic array, an associative array and a queue
  move and end their elements as they grow and shrink (LRM 7.5, 7.8, 7.10), so a write into one --
  of an element, of a slice, of its shape by any of its methods -- keeps the whole array, once per
  event's execution, and nothing inside it is kept apart. A procedure reading one of its elements
  reads it whole (LRM 11.5.3 names the element; the array is what is watched), so a change of any
  element is a change of what it reads. Such arrays are no part of synthesizable logic, so this is
  paid only by a procedure reading one, and only once per event's execution that writes it.
- **A kept value points only where a variable's fixed steps lead.** A member, an element of a
  fixed-size array and a packed value lie where they are for as long as the variable does, so they
  are kept with a pointer to where they lie; an array whose size changes lies where it is itself.
- **A write through a reference keeps the part its path names.** A reference to an element of a
  dynamic array, an associative array or a queue goes on naming that element after the array is
  resized, assigned or loses it, an outdated reference whose storage lasts only until the call it
  was passed to returns (LRM 13.5.2). It holds no position in the array, so its path is inexact
  (`a-part-of-a-variable-is-named-by-its-steps.md` D4) and a write through it keeps nothing. A
  member of an unpacked union is no storage of its own (LRM 7.3), so a write of one keeps the union.
- **Where nothing names the part, being reached decides.** A path ends at a bounded depth
  (`a-part-of-a-variable-is-named-by-its-steps.md` D4), what a solver writes is named by no path,
  nor is what a reference into an array whose size changes writes, and a net's driver keeps nothing,
  since each evaluation of it writes the net's value once. Such a write wakes the procedure as each
  write did before. A class object's properties are no part of any procedure's list (LRM 9.2.2.2.1),
  so their writes reach none of these waits.
- **A procedure stopped from outside after a write reached it is owed its run.** Under the per-write
  reading it was already runnable, so started again it runs, whatever the rest of the event's
  execution wrote (LRM 9.7); one stopped before any write reached it waits afresh.
- **A wait begun during an event's execution is woken by being reached.** One started again in the
  middle of one -- `x = 1; p.resume(); x = 0;` -- or moved by a force to another place has nothing
  from the execution's start to be measured by, so a write of it reaching the wait after that wakes
  it, as each write did before; `x = 0; p.resume(); x = 1; x = 0;` runs it once. Starting a process
  again from outside is rare, and what it is owed (LRM 9.7) it still receives.
- **A kept part is compared whole.** A part the execution left changed is told so, by the bits that
  moved where it is a packed value; a wait on a part inside it is told only that it was reached, so
  a structure or an array written whole and left changed anywhere wakes every procedure reading any
  of it. That costs a run of such a procedure with its inputs as they were, which changes nothing it
  writes and so wakes nothing further; a write of one member or element keeps only that part, and
  wakes only its readers.
- **Only these two procedures.** They are the constructs the standard gives for combinational logic
  and latches (LRM 9.2.2.2, 9.2.2.3), whose outputs are a function of what they read. An event
  control keeps the standard's reading, an `@*` among them, since a general procedure may count or
  print what wakes it: an armed `@(a)` or `@(posedge a)` is woken by `a = 1; a = 0;` in every legal
  schedule. So do a continuous assignment's sensitivity, a named event, a covergroup on its clocking
  event, an expect statement's clock and forced storage following its source.
- **Storage is written at once.** Only the telling of these waits waits for the end of the event's
  execution, so a write is seen by every reader immediately (LRM 4.9.3, 13.5.2).
- **The run holds two lists.** The variables keeping values for the event's execution under way, and
  the waits its writes reached or that began watching during it, are each linked once, and each
  leaves its list as it ends. At the execution's end each kept value is compared, what moved is told
  where it moved, and the value is dropped; then the objects that expired meanwhile are told; then
  each wait is decided. What expiry runs belongs to the execution, and dropping a kept value can let
  an object expire, so the three go in turn until none has anything left.
- **A kept value may hold a share of an object until the event's execution ends.** When weak
  references to an object are cleared is the memory management system's to determine (LRM 8.30.1),
  so an object last referenced by a variable an `always_comb` watches may be cleared at the
  execution's end, and a weak reference read later in the same execution can still reach it.

This is a choice beside the standard's letter. It differs from every legal schedule only where an
evaluation of the procedure with unchanged inputs could be seen: a side effect in its body, or the
glitches such a run would write being counted by an armed event control elsewhere.

## Survey

- **SystemC** (`sc_signal_t::write` and `update`, `sysc/communication/sc_signal.h`): a write keeps
  the new value and requests one update, and the update compares it with the current value and
  notifies the value-changed event only where they differ, so a value written back before the update
  is no event. A SystemC signal's value is not seen until the update; a SystemVerilog variable's is
  seen at once, so here only the telling is held, and the boundary is one process's run rather than
  the whole evaluation phase, since another process's run is an event of its own (LRM 4.5).
- **VHDL** (recalled): an event occurs on a signal only where its effective value changes in the
  simulation cycle, and the last of several assignments a process makes in one run is the one that
  counts, so the same default style settles there by construction.

## Rejected

- **Holding every value-change wait until the event ends.** It loses events the standard requires:
  an armed `@(a)` counting changes reads zero for `a = 1; a = 0;`, where every legal schedule reads
  at least one, and a zero-width pulse is no longer a positive edge.
- **Judging at the end of a region pass.** It merges two processes' runs into one change, which the
  standard keeps as two events.
- **Keeping only the standard's reading.** Legal, and every design written in this style then stops
  with the time slot unsettled.
- **A baseline for each leaf of the implicit list, taken at every stop.** The same answer for every
  write within one run, paid at every stop of the procedure for every leaf it reads, rather than
  only for the places some run writes; `y = mem[idx]` makes the whole of `mem` a leaf.
- **Merging each write's description of what it changed, at the wait.** Only a packed value's write
  describes the bits it moved; for every other type the old value is gone once the write lands, so a
  `real` or an unpacked array written and written back still looped.
- **Keeping the whole variable at its first write.** Every write is opened on the variable, but a
  write to one element of a fixed-size memory an `always_comb` reads -- the memory of RTL -- then
  copies and compares the whole memory, once for each nonblocking update; a write pays for what it
  wrote (`a-part-of-a-variable-is-named-by-its-steps.md`, `a-write-compares-what-it-reached.md`),
  and a whole kept value also tells every reader of any of its parts that the whole moved.
- **Keeping a class handle by the identity of its object, holding no share.** A kept value has to be
  able to go back into its part while an enclosing part is copied, and an identity can be put back
  only while its object still exists; the share is what lets it, and LRM 8.30.1 leaves the moment
  weak references are cleared to the tool.
- **Measuring a wait begun during an event's execution from when it began.** Each such start opened
  an interval of the execution, and every first write kept what a part held for each open interval,
  so that `x = 0; p.resume(); x = 1; x = 0;` left the procedure as it was. It was built, and it cost
  a second index into everything kept, paid on every write of every execution with such a start, for
  a process started again from outside in the middle of another's run, which designs do not do.
- **Keeping an element of an array whose size changes by its position.** Built so that a write of
  one element of a queue kept one element, it had to say, for every change of shape, which positions
  the change made or ended and how a part kept whole later saw them as they were; three defects in a
  row came from that bookkeeping, one of them a reference losing the element it named (LRM 13.5.2).
  No synthesizable design writes such an array, and keeping it whole is exact.
- **Carrying, in a reference, the array whose size changes its path stepped into, so a write through
  it kept that array whole.** It was built, so that nothing kept pointed into storage the array
  could end. Every reference held where that array lay, how to copy it and how many steps led to it,
  which grew every reference and every open write, and one of its defects was a reference past the
  bounded depth keeping under the wrong part. All of that served a call writing by reference into
  such an array that an `always_comb` reads, which no synthesizable design does; such a write is
  judged by itself. Keeping the element by its position instead points into storage an outdated
  reference holds only until its call returns (LRM 13.5.2), which the execution's end then reads.
- **Keeping a packed value whole for a write of some of its bits.** A memory declared packed then
  copied and compared the whole memory on every element written, many times what one declared
  unpacked cost.
- **Comparing a part kept whole down to the member or element a wait reads.** It was built, so that
  a structure written whole woke only the readers of what moved. It walked each kept value by the
  wait's path, by calling each type's members in the runtime library for the C++ backend and by
  reading the type's description at run time for the execution backend -- two walks of one question,
  the second a description the runtime reads, which the per-type operations MIR states for each type
  are meant to replace. What it saved is a run of a procedure whose inputs did not move, which
  changes nothing and settles.
- **Writing a class property through a reference to it, so the write is kept and compared.** No
  procedure's list holds a property (LRM 9.2.2.2.1), so nothing would read what it kept, and every
  property write would build a reference and open a write on it.
- **A record of a queue's edits, so a `push_front` keeps no copy.** The execution's end still
  compares each position a wait reads, which is what the copy costs, so the record buys nothing.
- **An option the caller turns on.** Every design written in the ordinary style would then stop
  until its author found the option, for a difference that only an `always_comb` with side effects
  or a counted glitch can show.

## Consequences

- `tests/choices/09_processes/always_comb_is_evaluated_on_what_a_run_leaves` holds the choice; the
  conformance case `09_processes/a_change_undone_in_the_same_run_is_still_an_event` holds the
  standard's reading where it stays.
- A procedure that keeps changing what it reads still keeps its time slot going round its regions
  until the slot's bound ends the run.
- An event's execution's first write to a part of a variable an `always_comb` or `always_latch`
  reads copies that part, and the execution's end compares it. A write of bits of a packed value
  copies the words they lie in; a write of an array whose size changes, or of anything inside it by
  the variable's own steps, copies the array.
- A change a kept packed value shows is told by its bits, so a wait reading only bits that moved
  back is not woken. A structure or an unpacked array written whole wakes a reader of any member or
  element of it where any of it moved; a member of an unpacked union is told as the union, and a
  reader of one element of an array whose size changes is woken by a change of any.
- Two procedures defaulting each other's inputs through a part nested deeper than a path follows, a
  reference into an array whose size changes, a solver's write or a wait begun during an event's
  execution are judged by each write, as before, and do not settle.

## Cross-references

- `a-wait-is-storage-of-its-activation.md` -- a wait's memberships stand for its life, and a stop
  only marks the activation parked.
