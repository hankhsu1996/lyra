# Combinational updates are coalesced where a build asks

Date: 2026-11-15. Status: accepted. Revised 2026-11-18: only a run of combinational logic is judged
by what it leaves (Rejected, "Judging the run of every process"). Revised again 2026-11-18: every
update is an event unless the build asks for coalescing (Rejected, "Coalescing by default"). Revised
2026-11-19: each library is built in the dialect it states; the shape is restated as change
detection over an interval on the write path; the rewrite stating a write made once is removed.

## Context

An `always_comb` that writes a default for each output and then its value changes that output twice
in one execution. Every change of a variable is an update event, and a process sensitive to it when
it is executed is scheduled (LRM 4.3, 4.5, 4.9.3). So two such procedures that each read an output
the other defaults wake each other: each execution moves the other's input away and back, and the
other is waiting when it does.

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
top of the procedure -- is ordinary RTL, which synthesis reads as logic without a loop. CVA6's write
buffer has this shape, and OpenPiton stops on it.

LRM 4.7 lets the statements of two processes interleave: in a schedule where neither procedure is
waiting while the other writes, neither sees the other's default, and the two settle. A schedule
running each procedure to its stop, which is how Lyra runs every process, has each waiting while the
other writes, and they wake each other for ever. Both are legal, so the design depends on a choice
the standard leaves to the tool, and its author is relying on one the standard does not promise.

## Decision

**By default every update is an event, as the standard reads.** A design whose procedures wake each
other for ever ends with the time slot reported unsettled, naming the procedures that kept it going.
Where one of them is an `always_comb` or `always_latch`, a note says how to build the design so that
it settles.

**A build may ask for combinational updates to be coalesced: `--coalesce-comb-updates`, or
`[compile] coalesce_comb_updates = true` in a library's `lyra.toml`.** In a library built so, an
`always_comb` or `always_latch` is a per-interval observer: where another such procedure writes what
it reads, it is triggered by the net change that procedure's execution leaves, not by each write. A
value written and written back wakes none of them, whatever its type, and the two procedures above
settle, as they do in the interleaved schedule. A write by any other process -- an `initial`, a
general `always`, an `always_ff`, a nonblocking update, a timer, a force -- triggers every wait as
it lands, a per-interval observer's included.

- **It is a dialect of the language, chosen where a library is compiled.** It changes what a program
  computes wherever an evaluation of a procedure with unchanged inputs could be seen: a side effect
  in the procedure's body, or the writes of such an evaluation counted by an armed event control
  elsewhere. That makes it a choice of what the program means, so it is made where the program is
  compiled, as `-fwrapv` is: clang reads the option of the translation unit being generated and
  chooses, at each signed addition, an addition whose overflow is undefined or one that wraps
  (`ScalarExprEmitter`'s overflow behaviour in `clang/lib/CodeGen/CGExprScalar.cpp`), and two
  translation units of one link may differ. Here a unit's dialect is its library's: HIR-to-MIR
  builds each such procedure's wait as the one of its dialect, so MIR states which, and every layer
  below translates it. A library stating nothing is written for the standard and is read so whatever
  uses it; the command line's answer, `true` or `false`, is every library's.
- **Three concepts, each with one job.** Change detection compares a part of a variable at two
  moments, with one comparison for both spans: a write compares the part it reached before and after
  it lands (`a-write-compares-what-it-reached.md`), and an interval compares the part's original
  with what it holds when the interval ends. An observer says which it wants, by the wait it is: a
  per-change observer is told each write, a per-interval observer the net change. An interval is the
  time span of one net change, and the writer declares it: one execution of a procedure whose
  dialect coalesces is one interval. Neither side knows the other; they meet at the write and at the
  interval's end. So a mixed build needs no rule of its own: a coalescing writer's writes reach a
  per-change reader as they land, and a per-change writer opens no interval, so its writes reach a
  per-interval reader as they land.
- **A modification is announced immediately before it lands, and says whether the interval
  accumulated it.** The original of a part exists only until the write lands, so every way a write
  modifies a variable -- a whole store, a landing at a part, each element of a slice, the words of
  some bits, a step that can reshape an array, a write through a reference -- announces the part
  first. Where an interval is open and a per-interval observer watches the variable, the
  announcement keeps the part's original, unless a part holding it is kept already, and answers that
  the interval accumulated the modification; an accumulated write is told to every per-change
  observer as it lands and to the per-interval observers only by the interval's end. Accumulated
  belongs to each modification and is carried from the announcement to the telling, never looked up
  afterwards: a process run at an occurrence inside the interval -- a covergroup sampled on an edge
  it makes -- runs in no interval, so its write is told at once even where the interval kept that
  part. Every modification one write announces is the part it tells of or a part holding it, so its
  answers agree, and a write whose answers differ stops as a compiler bug. A variable no
  per-interval observer watches answers at its first question, and that question and the store are
  all a store to it is, so it is folded into whatever writes.
- **No code of the design runs inside a write.** The legalized form of MIR evaluates every operand
  of a write -- each index a step takes, the value -- before the write opens, so between the
  announcement and the landing nothing can end the interval, and a write never spans two intervals.
- **An inexact part is accumulated only through what holds it.** A path ends at a bounded depth, a
  reference into an array whose size changes names no position in it, and a member of an unpacked
  union is no storage of its own (`a-part-of-a-variable-is-named-by-its-steps.md` D4, LRM 7.3,
  13.5.2); what such a path names is more than the part, so it keeps no original itself. It is
  accumulated where the interval keeps the original of an exact part holding it -- the union, the
  array written whole first -- and told as it lands otherwise. What a solver writes is named by no
  path, and no combinational procedure drives a net (LRM 10.3, 10.4), so those writes are told as
  they land. A class object's properties are no part of any procedure's list (LRM 9.2.2.2.1), so
  their writes reach none of these waits.
- **The interval ends when the execution ends, and whenever a per-interval observer changes how it
  listens.** Its originals are shared by every per-interval observer listening while it runs, which
  is sound only while the set of them does not change. So the interval ends, telling each observer
  the net change so far, and a new one begins: when the execution ends, its process waiting again or
  ending; when a per-interval observer begins listening -- arming its wait, or started again (LRM
  9.7), which waits afresh; when one stops listening, stopped from outside while waiting; and when a
  force or a release moves one to other storage (LRM 10.6.2). The observer does this itself, as its
  wait begins and is withdrawn, so stopping or starting a process knows nothing of the dialect.
  Nothing else ends it. Killing a process changes nothing a wait could show, since a killed process
  runs nothing whether it was told first or not; a disable reaches no per-interval observer, since
  such a procedure is never blocked inside a block and starts no process; and a status query changes
  nothing. Within an interval the execution is one evaluation with no timing control in it (LRM
  9.2.2.2), and the process making it is running rather than waiting, so a change it makes is no
  event for its own wait (LRM 9.4.2); a procedure that writes an element of an array and reads it by
  a variable index reads the whole array (LRM 9.2.2.2.1), and is still not woken by itself.
- **The interval ends for every observer at once, and that is a stated limit of the dialect.** An
  observer that begins or stops listening ends the interval for the others too, so one may be told a
  change that the whole execution cancels, and run once more. That run is not invisible: a procedure
  with side effects runs once more than the interval's net change asks, and a suspend or a resume
  inside a loop of wakes can recur on each pass. It happens only where a design stops, starts or
  forces a combinational procedure while another one's execution is under way.
- **Each interval is judged on its own.** A change one execution makes and another undoes still
  triggers the procedure, because the first left it changed.
- **Only one interval is open at a time.** Only one process runs at a time, and the one way a
  process runs inside another's execution -- one woken at an occurrence -- never runs a
  combinational procedure: every execution one starts takes a context stating no dialect, and only
  an `always_comb` or `always_latch` states one. So each variable keeps the originals of the one
  open interval itself, and their storage is reused from one interval to the next. A telling that
  raises at an interval's end ends the run, so no later interval meets what it left kept.
- **What is kept lies in storage the simulation already holds.** An original is made in the bytes
  the storage for deferred effects hands out and given back when the interval ends, and the
  originals a variable keeps are listed in a vector it keeps from one interval to the next, so an
  interval asks for no storage once one has; the words a write of bits keeps lie in the list itself
  where they fit.
- **A write of bits keeps the words they lie in.** A packed value is its bits (LRM 7.4.1), so a
  write of some of them -- an element of a memory declared packed, as older Verilog had to -- keeps
  the words of each plane those bits lie in, as one run of words that grows as a later write reaches
  words outside it; a word outside the run of words was not modified in the interval, so it is taken
  as it stands. A memory declared packed and written an element at a time then costs about what one
  declared unpacked does, writes far apart keeping the words between them, and the change is told by
  its bits. A value narrower than a word is that word, kept whole.
- **What is kept is found by its path.** The originals are ordered by their paths, so whether a
  modification is held by one already, and which lie inside a part being kept, are each one look-up,
  and an interval modifying many parts pays for each what it touched.
- **A part kept already holds what lies inside it.** A later modification inside a kept part keeps
  nothing. A part whose own parts were kept earlier is kept with those put back for as long as it is
  copied, so its original is what it held when the interval began; a part judged against a copy
  taken in its middle would read its own writes as a change, and two procedures each writing their
  output part by part and then whole would wake each other again.
- **An array whose size changes is kept whole.** A dynamic array, an associative array and a queue
  move and end their elements as they grow and shrink (LRM 7.5, 7.8, 7.10), so a modification of one
  -- of an element, of a slice, of its shape by any of its methods -- keeps the whole array, once,
  and nothing inside it is kept apart. A procedure reading one of its elements reads it whole (LRM
  11.5.3 names the element; the array is what is watched), so a change of any element is a change of
  what it reads. Such arrays are no part of synthesizable logic.
- **An original points only where a variable's fixed steps lead.** A member, an element of a
  fixed-size array and a packed value lie where they are for as long as the variable does, so they
  are kept with a pointer to where they lie; an array whose size changes lies where it is itself.
- **A kept part is compared whole.** A part the interval left changed is told so, by the bits that
  moved where it is a packed value; a wait on a part inside it is told only that it was reached, so
  a structure or an array written whole and left changed anywhere wakes every procedure reading any
  of it. That costs a run of such a procedure with its inputs as they were, which changes nothing it
  writes and so wakes nothing further; a write of one member or element keeps only that part, and
  wakes only its readers.
- **Only these two procedures.** They are the constructs the standard gives for combinational logic
  and latches (LRM 9.2.2.2, 9.2.2.3), whose outputs are a function of what they read. An event
  control keeps the standard's reading in a design built either way, an `@*` among them, since a
  general procedure may count or print what wakes it. So do a continuous assignment's sensitivity, a
  named event, a covergroup on its clocking event, an expect statement's clock and forced storage
  following its source. What that storage then writes is written inside the execution that moved its
  source, so in an interval it is kept and judged with the rest of that execution's writes.
- **Storage is written at once.** Only the telling of these waits waits for the interval's end, so a
  write is seen by every reader immediately (LRM 4.9.3, 13.5.2).
- **The interval holds one list.** The variables keeping originals for the open interval are linked
  once each, and each leaves the list as it ends. When the interval ends each original is compared,
  what moved is told where it moved, and the original is dropped, however the telling ends. When the
  execution ends, the objects that expired meanwhile are told too; what expiry runs belongs to the
  execution, and dropping an original can let an object expire, so the two go in turn until neither
  has anything left.
- **An original may hold a share of an object until the interval ends.** When weak references to an
  object are cleared is the memory management system's to determine (LRM 8.30.1), so an object last
  referenced by a variable such a procedure watches may be cleared when the interval ends, and a
  weak reference read before it can still reach it.

## Survey

- **Compiler dialect options.** `-fwrapv`, `-fno-strict-aliasing` and
  `-fno-delete-null-pointer-checks` each give a program the meaning its author relied on where the
  language leaves it undefined or to the implementation, are chosen per build and default to the
  standard's reading, and are documented as a promise about the program rather than as an
  optimization. The coalescing option is the same kind of promise and takes the same place.
- **SystemC** (`sc_signal_t::write` and `update`, `sysc/communication/sc_signal.h`): a write keeps
  the new value and requests one update, and the update compares it with the current value and
  notifies the value-changed event only where they differ, so a value written back before the update
  is no event. A SystemC signal's value is not seen until the update; a SystemVerilog variable's is
  seen at once, so here only the telling is held, and the boundary is one execution's interval
  rather than the whole evaluation phase, since another process's execution is an event of its own
  (LRM 4.5).
- **VHDL** (recalled): an event occurs on a signal only where its effective value changes in the
  simulation cycle, and the last of several assignments a process makes in one run is the one that
  counts, so the same default style settles there by construction.
- **rustc's incremental compilation** (`compiler/rustc_middle/src/dep_graph/graph.rs`, the colour a
  re-executed query is given): a query run again whose result has the fingerprint it had before is
  green, and what depends on it is not run again -- the cut-off is made where a producer's work
  ends, against what it produced before. Here the producer is the combinational execution and what
  it produced before is what its writes found; a fingerprint would let a collision drop a run the
  design needs, so the parts themselves are compared.
- **Verilator** (`V3SenExprBuilder.h`, `getPrev`): the previous value of a trigger expression is a
  variable the compiler makes, compared at every evaluation, for the triggers that need one. A
  SystemVerilog write reaches a part of a variable that may be a whole memory (LRM 7.4, 11.5.1), so
  a whole previous value per variable would cost the variable at every execution; what is kept here
  is the part each execution wrote.

## Rejected

- **Coalescing by default.** The form of this decision before 2026-11-18: every design paid for it.
  A fanout of 64 combinational readers, built `-O2 --lto`, counted 42% more instructions per cycle
  than before any coalescing, and the rewrite brought that back to 3.5% only where each run writes
  each variable once. It also made Lyra answer for a design relying on a schedule the standard does
  not promise, on behalf of every design that does not, and departed from the standard's reading
  wherever a procedure's side effects show it. One real design needs it; the option serves that
  design at no cost to the others. Revisit if designs needing it turn out common rather than one.
- **Coalescing a slot only once it fails to settle.** Running the slot again coalesced when its
  bound is reached gives a design two meanings depending on whether it loops, and what ran before
  the switch has already been told each write.
- **A run-time option of the simulation.** What triggers a procedure is what its wait is, which the
  compiled program states; a switch read at run time would have every wait ask it at every write,
  and a program's meaning would no longer be the program.
- **An option named for the effect a designer sees.** No word of hardware design says what this
  does: a glitch is a hardware event and a latch is what a missing default infers, and neither is
  what changes. Coalescing updates is the scheduler's own term for it, as SystemC's update phase is.
- **Holding every value-change wait until the event ends.** It loses events the standard requires:
  an armed `@(a)` counting changes reads zero for `a = 1; a = 0;`, where every legal schedule reads
  at least one, and a zero-width pulse is no longer a positive edge.
- **Judging at the end of a region pass.** It merges two processes' executions into one change,
  which the standard keeps as two events.
- **Excluding the writer's own wait when its execution's end is judged.** Built first: the process
  parked again before its writes were told, so they were told to every wait but its own. Telling
  them before the process waits again needs no exception and also covers a process control the
  execution makes.
- **A baseline for each leaf of the implicit list, taken at every stop.** The same answer for every
  write within one execution, paid at every stop of the procedure for every leaf it reads, rather
  than only for the places some execution writes; `y = mem[idx]` makes the whole of `mem` a leaf.
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
- **Judging the execution of every process.** The first form of this decision judged each event's
  execution, whoever wrote: a value an `initial` wrote and wrote back woke no `always_comb`. That
  drops a run every legal schedule makes where the reader is idle (LRM 4.3, 4.5), for no design that
  needs it, and it kept a copy at every write of every process to a variable a combinational
  procedure reads: a fanout of combinational readers counted 80% more instructions per cycle, and a
  loop of scalar stores with nothing waiting 17% more, as every store carried the keeping.
- **Measuring a wait begun during an execution from when it began.** Each such start opened an
  interval of the execution, and every first write kept what a part held for each open interval, so
  that `x = 0; p.resume(); x = 1; x = 0;` left the procedure as it was. It cost a second index into
  everything kept, paid on every write. A cheaper form keeps a start of its own only for the
  observer that begins or stops listening, and only for the parts the interval already kept, so
  writes pay nothing; it still needs a second store of originals, keyed by observer, and a second
  comparison when the interval ends, for operations no workload has shown to matter. Ending the
  interval for every observer answers the same case with nothing kept twice, at the cost stated as
  the dialect's limit.
- **A library stating nothing takes the root's answer.** A library would then mean something
  different depending on what uses it, which no other choice of how a library is compiled does; a
  translation unit's language options in clang are its own whatever the rest of the link says.
- **Refusing a library whose dialect differs from the root's.** The first form of this decision, as
  one declaring another language version is refused. That refusal is the front end's, which reads a
  build under one version; nothing here reads the build under one dialect, since each unit's waits
  are built in its own, and a mixed build needs no rule of its own.
- **Deciding whether a write was accumulated when it is told.** Built first: the telling looked up
  whether the interval kept the part written. It read a write made by a process run inside the
  interval as accumulated, though that process opens no interval, so its write was told only by the
  net change; and it asked once more on every watched write what the announcement had already
  answered.
- **Stating a write an execution makes once, so it keeps nothing.** An optional rewrite of MIR
  counted, for a process stopping only at the head of its loop, the writes no path through one pass
  could repeat, and stated each with a write entry of its own that kept nothing and was told as it
  landed: on the fanout of 64 readers it cut a build with the option from 148.3M instructions to
  107.6M. Removed until a design that needs the dialect shows the cost matters, because it was a
  second family of write entries beside every write, and because it said the wrong thing: proving a
  write is the only one lets its original go unkept, and says nothing about when it may be told. If
  it returns, it states that the write's change cannot be cancelled within the interval: such a
  write keeps no original and is not compared, and is still told when the interval ends.
- **An epoch on the originals, so a write that spans an interval's end is told at once.** No code of
  the design runs inside a write, so none spans one.
- **Ending the interval in two phases, all compared and then all told.** What can raise while an
  interval ends is a telling failing for want of resources, which ends the run here; what is needed
  is that each variable's list is left empty however the telling ends, which it is.
- **The words an execution's writes moved, kept by where each word lies: what it held before the
  first write to it, or the exclusive or of each write's old and new value.** The difference of a
  word written several times is the exclusive or of each write's old and new value, with every value
  in between cancelling, so what an execution left moved is what remains -- with no copy of what the
  word held. It needs every write to say what it moved, which only a write of bits does; a copy
  taken before the first write needs only that the write is about to land, which every write says.
  Keyed by where a word lies rather than by a path, either form also could not tell apart two parts
  narrower than a word lying in one, which nest by path and not by address.
- **A procedure none of whose writes an execution repeats is judged by each write.** One fact per
  procedure rather than per write: a procedure writing one output twice would then keep every output
  it writes, where stating it per write keeps only the output written twice.
- **A copy of each output of each combinational procedure, compared when it stops.** It costs every
  output the procedure declares at each of its executions, where keeping what an execution wrote
  costs what it wrote; a procedure writing one element of a large array would compare the array.
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
  reference holds only until its call returns (LRM 13.5.2), which the interval's end then reads.
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
- **A record of a queue's edits, so a `push_front` keeps no copy.** The interval's end still
  compares each position a wait reads, which is what the copy costs, so the record buys nothing.

## Consequences

- The conformance case `09_processes/a_change_undone_in_the_same_execution_is_still_an_event` holds
  the standard's reading, an `always_comb` assigning a default and then a value among its writers;
  `tests/coalesced_comb_updates` holds what a design built with the option does, and the command
  line tests build it on both backends and build libraries stating different dialects.
- A design built either way whose procedures keep changing what they read still keeps its time slot
  going round its regions until the slot's bound ends the run.
- A design built without the option keeps nothing: whether a write announces anything is settled
  once, where the write opens, by whether a per-interval observer watches the variable at all, and a
  combinational procedure waits as it did before any coalescing. In one built with it, an interval's
  first modification of a part of a variable a per-interval observer reads copies that part and the
  interval's end compares it. The fanout of 64 combinational readers, 2026-11-19, `-O2 --lto`,
  counted 101.9M instructions built without the option against 103.7M before any coalescing, and
  144.9M built with it. A write of bits of a packed value copies the words they lie in; a write of
  an array whose size changes, or of anything inside it by the variable's own steps, copies the
  array. A write by any other process keeps nothing.
- In a design built with the option, a change a kept packed value shows is told by its bits, so a
  wait reading only bits that moved back is not woken. A structure or an unpacked array written
  whole wakes a reader of any member or element of it where any of it moved; a member of an unpacked
  union is told as the union, and a reader of one element of an array whose size changes is woken by
  a change of any. Two procedures defaulting each other's inputs through a part nested deeper than a
  path follows, a reference into an array whose size changes the execution did not write whole, or a
  solver's write are judged by each write, and do not settle.
- OpenPiton is built with the option in the external design suite
  (`tests/external_design_build_options.yaml`).

## Cross-references

- `a-wait-is-storage-of-its-activation.md` -- a wait's memberships stand for its life, and a stop
  only marks the activation parked.
- `project-file.md` -- what `[compile]` declares, and which of it every library of a build states
  alike.
