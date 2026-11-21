# A write compares what it reached

Date: 2026-09-25 Status: accepted. Revised 2026-11-06: a slice write says which elements it changed.
Completes, for a write through a capability wrapper,
[owner-transition-and-observation](owner-transition-and-observation.md) D3 and D4 on both backends,
over the place steps of
[a-part-of-storage-is-reached-where-it-lies](a-part-of-storage-is-reached-where-it-lies.md).

## Context

A write into part of a variable something waits on kept a copy of the whole variable from before the
write, so that when it ended the variable could say whether it changed (LRM 4.3). For an unpacked
array that is every element, whatever the write touched. A clocked 4096-entry memory written with
`<=` and read by an `always_comb` ran 200,000 cycles in 8.7 s on the execution backend and 3.07 s on
the C++ backend (2026-09-25, `-O2`), against 0.43 s for the same design with 16 entries. On the
execution backend 94% of the run was that copy, its comparison and its destruction.

The copy is taken only while a wait is parked on the variable, and a wait that fires is unlinked, so
it is paid once per wake rather than once per write. A memory read by a combinational process pays
it every cycle.

The variable has to answer exactly. An event control compares its own expression against its own
baseline and filters a wake that changed nothing (LRM 9.4.2), but an implicit sensitivity -- an
`@*`, a `wait`, a continuous assignment -- is woken by being reached and has no value of its own to
compare, and in a design built to coalesce combinational updates the wait of an `always_comb` or
`always_latch` is decided, for a write of an execution of such a procedure, by what the part it
wrote holds when the execution's interval ends
(`combinational-updates-are-coalesced-where-a-build-asks.md`), which is only as exact as what the
write says it reached. A wake with no change runs its body again.

## Decision

**A write keeps the value of the part it lands on, not of the variable, and a step that forms the
part tells the write what forming it did.**

- **MIR states the write as a designation.** A write is opened on the wrapper, the whole of the
  wrapper's contents is designated within it, and each step into a part that is storage of its own
  is taken within the same write, so the write reaches the part through its own steps:
  `*x.open_for_write().designate_whole().designate_element(i).designate_component<k>() = v`. Each
  such step is an entry of its own, named where the source is read and paired with the step into
  storage it stands beside (an element's write form, a component's field step), because it answers
  with a designation of the part rather than with the part and it is heard by the write. Where the
  write lands is where the designation is dereferenced, which is what lets a backend rendering one
  node at a time keep the part rather than the whole. A string's character is written after that by
  the string's `putc` on the place landed on, since it lies inside the string's value (LRM 6.16).
- **A union's member is designated too, and the path the write reports stops at the union** (revised
  2026-10-15). A member lies where every other member does (LRM 7.3), so a write of one leaves every
  byte outside it as it was, and the union changed exactly where the member did: the write keeps and
  compares only what it lands on below the member. No step of a path stands for a member, since a
  wait on one member reads bytes another member's write moves, so the path stops at the union as it
  does at an associative array's entry, and the change is reported whole there, reaching every
  member's waits. A designation is the write's own handle on where it lands and binds nothing the
  program keeps, so LRM 13.5.2, which keeps a reference off a member, does not reach it. This
  revises the earlier rule that landed a member write on the union and took the member as a field
  step, which kept and compared the whole union for a write of one member.
- **An element step taken within a write reports to it.** Forming an element can change the variable
  whatever is then written -- an associative array entry allocated by being written (LRM 7.8.7), a
  queue element appended at `$+1` (LRM 7.10.1) -- or reach no part of it at all, where an invalid
  index makes the write ignored (LRM 7.4.5). The element cannot say which, so the step does, and the
  write takes the answer from there without comparing anything. A component is formed by nothing, so
  its step has nothing to tell.
- **A slice write within a write answers what it changed, and the write is told that answer and
  nothing else** (revised 2026-11-06). The slice write compares each element before writing it, so
  it alone knows which took a different value; it answers the positions from the first of them to
  one past the last, the same position twice where none did, and an answer of no elements settles
  the write unchanged. Its operands are where the slice starts and the values, handed on unread,
  since how many elements it takes is its type's; a queue's slice write is handed instead the two
  positions it clamps (LRM 7.10.1) and where the write was written, since it reports a value of
  another count. This revises the earlier answer, whether any element moved, beside which the write
  was told the slice's start and count read back off the call's operands -- a second reading of
  operands the call had already handed over, and a run wider than what changed. A slice write is no
  step: its work is the array type's own operation rather than the library's, so below MIR it is the
  part the designation names, that operation on it, and the telling.
- **The write and a place designated within it are two types.** The write is held by the writer and
  ended with the full-expression, which is when it reports, and it names no place itself; a
  designation -- of the whole, or of a part -- borrows the write and ends with nothing to do. Every
  step and every landing is taken on a designation, so there is one kind of receiver for them.
- **Below MIR, each MIR step is one library call on a designation, taken once, and what the step
  does to the write is the library's.** An element step's entry tells the write what forming the
  element did and a component step's entry does not, exactly as the C++ library's designation
  methods do; landing is an entry answering with where the part lies, which is then written through
  that address. No lowering below MIR distinguishes the kinds of step: that is the C++ rendering of
  the same MIR compiled by clang, one call for one call, and the only difference is that a prebuilt
  library keeps the part's value from before the write in the write rather than in the designation.
  Measured 2026-09-28 on the clocked 4096-entry memory, `-O2`: 0.42 s on the execution backend and
  0.26 s on the C++ backend.
- **Otherwise the part landed on is compared with its value from before the write.** That value is
  kept only where something reads the answer and no step has given it already.
- **Bits of a packed value are a slice designated within the write** (revised 2026-10-05). The bits
  of a packed value lie contiguously (LRM 7.4.1) and some of them are reached where they lie, at the
  cost of those bits ([a-value-is-its-machine-data](a-value-is-its-machine-data.md) D4, D7), so a
  bit-select, a part-select or a packed member a write names is designated within the write, and the
  write lands on those bits rather than on the whole. It keeps only the words they lie in, and it
  tells the waits which bits it reached and what they held: a wait reading none of them is passed
  over without a comparison, and one reading some compares only those it shares with the write. The
  bits are written through the reference to them the value's `slice_mut` answers taken on the
  designation, and a part of a part through `slice_mut` taken on that reference, so the write hears
  each part they are reached through, outermost first. No part is read out at its own type to write
  a part of it, so a write to one bit of a two-state member of a four-state structure leaves the
  member's other bits what the vector holds, x included (LRM 7.2.1, 11.5.1). A whole-value store
  into a packed variable reaches every bit and keeps its words the same way. This revises the
  earlier rule that a packed variable, having no parts that are storage, is landed on whole: that
  rule kept a copy of the whole value for any write into it, which designating the bits removes.
- **Every way a write reaches its part is a landing.** An assignment, a compound assignment or an
  increment, and a method changing its receiver in place each land the write before anything is
  written, and each reaches the part the way a write does, so an element a write would form is
  formed for any of them.
- **A whole-value store compares the new value with the old one before writing it.** It keeps no
  copy, except the old bits a packed variable's waits are passed over by, and the old value where a
  wait watches a part inside it (below).
- **A write tells each wait on a part inside the part it reached whether that part changed**
  (revised 2026-11-20). The write is what holds the value it replaced and the one it left, so it
  answers once for every wait, and a wait on storage needs no copy. Where the part reached is not a
  packed value, the change it tells is the two values, each read where it lies while the waits are
  told, and the walk down the waits on parts inside asks the type for the part one step names -- a
  fixed-size array's element by its position, a structure's member by its index, each answered by
  that kind's own description in the type's record, as Swift's generic code reaches a field through
  its type's metadata -- comparing that part's two values: a part left as it was is passed over with
  everything inside it, and an integral one is told which of its bits moved. The value replaced is
  kept for that only where a wait watches a part inside: a whole-value store copies it then, which
  costs what the store costs anyway; a landing already holds it; a slice write answers the positions
  it changed (above); an element a step makes is told as that one element. This revises the rule
  that a wait on a part inside the part written was told only that it was reached, which ran an
  `always_comb` reading one member again for a write of the whole structure that left that member as
  it was (LRM 9.2.2.2.1). No type states a step to a member of a tagged union, nor a string to a
  character, nor into an array whose size the run sets, whose elements a whole write moves and ends,
  so a wait on any of those is told that it may have changed whenever what holds it did. An entry of
  an associative array has no position, so a wait on one watches the whole array.
- **What a write tells says which observers it may affect, never that a value is another**
  (2026-11-20). Where the write kept no value to read at a part -- a part inside one it replaced
  with nothing watched there, an element of an array whose size the run sets written through a
  reference naming no position, a part inside an element made or ended, a member of a union -- it
  can say only that the part may have changed, and an account of what it did is only as good as
  every kind of write giving it. So an observer that answers for a change of value itself compares:
  an event control its expression (LRM 9.4.2), and a foreign callback on a value, a size or which
  object a class variable names what it reads now with what it last read (LRM 38.36.1,
  [foreign-code-is-called-back-where-its-reason-places-it](foreign-code-is-called-back-where-its-reason-places-it.md)
  D4), the write's telling deciding only that it looks. An `always_comb` takes the telling as it
  comes, an extra run costing time and no value. What every write still owes is to tell each
  observer it may have affected, since one left untold is never asked. This replaces two readings of
  the same day: that a callback took every change told as its object's own, and that it took only
  one the write marked as proven, which several kinds of write marked wrongly before any was found.
- **A net driver gives the whole of the part it drives.** Its net resolves again only over the
  positions that driver drives (LRM 6.5, 6.6.7), so a driver of a net resolving positions opens no
  write. The one driver of an unresolved net is written into as a variable is, and its net's waits
  are told what moved. Revised 2026-11-17: a driver heard which of its positions a write moved and
  its net resolved again over those, which was finer than the part it drives only while a driver's
  contribution held the whole net.

## Rejected

- **Every observation keeps its own copy of what it reads, compared each time it is evaluated.**
  Verilator's answer (`V3SenExprBuilder`), which its static schedule affords. An implicit
  sensitivity reads the longest static prefix of what it selects (LRM 9.2.2.2.1), which is the whole
  array for a variable index, so the cost moves from the write to every waiter. This is about an
  observation of storage, which a write can answer for; an observation of an expression's result,
  which no storage holds, keeps that result itself
  ([update-events-are-per-variable](update-events-are-per-variable.md) D2), and the two rules are
  one rule applied to two kinds of observation. A foreign callback on a value or a size keeps one
  all the same, compared only where a write tells it, since it answers for the value itself
  (2026-11-20).
- **Notify with the part written and let each waiter decide.** Icarus Verilog's answer
  (`__vpiArray::set_word` notifies with the address unconditionally). An implicit sensitivity has
  nothing to decide with, so a write that changed nothing would run it again.
- **Compare at the part and nothing else.** The part cannot tell an element made by the write from
  one that was there, nor an element from the place an invalid index lands; both answer wrongly.
- **Keep the dereference where the write is opened and take the steps on the storage.** The steps
  are then the storage's own and nothing the write sees, so a backend rendering one node at a time
  can keep only the whole; the other backend could do better only by working out what that one
  cannot, and the two would no longer do the same work.
- **Take a step within a write through the entry that steps into storage, telling them apart by the
  receiver.** That is one entry read two ways, which
  [value-descent-as-named-calls](value-descent-as-named-calls.md) rejects: the operation is named
  where the source is read. Rust's `*map.entry(k).or_insert(0) *= 2` has the same shape, a
  designation built by calls of its own and dereferenced where it lands.
- **Below MIR, fold the steps into a place rooted at the write, and let the code generator choose
  each step's entry from the root and the access.** It was built first. The code generator then
  worked out again which step each place step had been, a fact MIR had already stated, and a place
  is resolved once per instruction naming it, so a compound write took every step three times -- a
  read, a landing and a store -- where the C++ rendering takes each once.
- **Below MIR, carry a designation as two operands, the write and the part's address, and have the
  lowering choose per step whether the write is handed on.** It was built second. It takes each step
  once, but the lowering then decides what each kind of step does to the write, which in the C++
  rendering is the library's business and no compiler layer's -- so the two backends make that
  decision in two places, held in step by nothing. clang, Rust's `Entry` and Swift's `_modify`
  accessors all leave it to the callee; the compiler only calls.
- **Let the write itself be the designation of the whole.** C++ can, by deriving the write from the
  step methods, but then the write carries a second copy of the landing beside the designation's,
  and a library compiled once can take only one kind of receiver per entry -- so the write had to
  lay a designation out as its first member and be handed where one was taken, sound only while its
  layout kept that member at its own address. A step designating the whole gives every step and
  landing one receiver, and costs one call per write that did not show in the measurement above.

## Relation to existing decisions

- [owner-transition-and-observation](owner-transition-and-observation.md) D2 and D3 -- the
  designation and its formation reporting its transition are what this realizes. Its D5 left the
  write in progress to each backend; it is narrowed to say why that write is stated in MIR instead.
  [a-part-of-storage-is-reached-where-it-lies](a-part-of-storage-is-reached-where-it-lies.md)
  already put the write in progress in MIR as an object, and this entry takes the descent's steps on
  it, because where the write lands has to be stated for both backends to do the same work.

## Consequences

- The clocked 4096-entry memory, 2026-09-25, `-O2`: **0.39 s on the execution backend and 0.25 s on
  the C++ backend**, from 8.7 s and 3.07 s.
- A part lent to a `ref` formal is not a place a write lands on: the write would report when the
  call's full-expression ends, and a write through a reference is due when it lands.
  [a-lent-part-carries-its-variable](a-lent-part-carries-its-variable.md) lends it as a reference
  that carries the variable.
