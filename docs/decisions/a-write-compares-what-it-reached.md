# A write compares what it reached

Date: 2026-09-25 Status: accepted. Completes, for a write through a capability wrapper,
[owner-transition-and-observation](owner-transition-and-observation.md) D3 and D4 on both backends,
over the place steps of
[a-part-of-storage-is-reached-where-it-lies](a-part-of-storage-is-reached-where-it-lies.md).

## Context

A write into part of a variable something waits on kept a copy of the whole variable from before the
write, so that when it ended the variable could say whether it changed (LRM 4.3). For an unpacked
array that is every element, whatever the write touched. A clocked 4096-entry memory written with
`<=` and read by an `always_comb` ran 200,000 cycles in 8.7 s on the execution backend and 3.07 s on
the C++ backend (2026-09-25, `--release`), against 0.43 s for the same design with 16 entries. On
the execution backend 94% of the run was that copy, its comparison and its destruction.

The copy is taken only while a wait is parked on the variable, and a wait that fires is unlinked, so
it is paid once per wake rather than once per write. A memory read by a combinational process pays
it every cycle.

The variable has to answer exactly. An event control compares its own expression against its own
baseline and filters a wake that changed nothing (LRM 9.4.2), but an implicit sensitivity -- an
`always_comb`, an `@*`, a `wait`, a continuous assignment -- is woken by being reached and has no
value of its own to compare. A wake with no change runs its body again.

## Decision

**A write keeps the value of the part it lands on, not of the variable, and a step that forms the
part tells the write what forming it did.**

- **MIR states the write as a designation.** A write is opened on the wrapper, the whole of the
  wrapper's contents is designated within it, and each step into a part that is storage of its own
  is taken within the same write, so the write reaches the part through its own steps:
  `*x.open_for_write().designate_whole().designate_element(i).designate_component<k>() = v`. Each
  such step is an entry of its own, named where the source is read and paired with the step into
  storage it stands beside (`element_ref`, `component_ref`, `slice_ref`), because it answers with a
  designation of the part rather than with the part and it is heard by the write. Where the write
  lands is where the designation is dereferenced, which is what lets a backend rendering one node at
  a time keep the part rather than the whole. A step into a part that is a view is taken after that,
  on the value landed on, because a view is written by writing its whole.
- **An element step and a slice step taken within a write report to it.** Forming an element can
  change the variable whatever is then written -- an associative array entry allocated by being
  written (LRM 7.8.7), a queue element appended at `$+1` (LRM 7.10.1) -- or reach no part of it at
  all, where an invalid index makes the write ignored (LRM 7.4.6). The element cannot say which, so
  the step does, and the write takes the answer from there without comparing anything. A slice write
  compares each element before writing it and says whether one moved. A component is formed by
  nothing, so its step has nothing to tell.
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
  Measured 2026-09-28 on the clocked 4096-entry memory, `--release`: 0.42 s on the execution backend
  and 0.26 s on the C++ backend.
- **Otherwise the part landed on is compared with its value from before the write.** That value is
  kept only where something reads the answer and no step has given it already. A packed variable has
  no parts that are storage, so a write into one lands on the whole, and its waits go on being
  passed over by the bits they read.
- **Every way a write reaches its part is a landing.** An assignment, a compound assignment or an
  increment, and a method changing its receiver in place each land the write before anything is
  written, and each reaches the part the way a write does, so an element a write would form is
  formed for any of them.
- **A whole-value store compares the new value with the old one before writing it.** It keeps no
  copy, except the old bits a packed variable's waits are passed over by.
- **A sampled variable keeps its Preponed value at the first write of a time slot** (LRM 16.5.1),
  whether or not that write changes it: nothing in the slot has changed the variable before its
  first write, so what it holds then is that value.
- **A net driver hears which of its positions a write moved.** Its net resolves again only over the
  runs whose contribution moved (LRM 6.5).

## Rejected

- **Every observation keeps its own copy of what it reads, compared each time it is evaluated.**
  Verilator's answer (`V3SenExprBuilder`), which its static schedule affords. An implicit
  sensitivity reads the longest static prefix of what it selects (LRM 9.2.2.2.1), which is the whole
  array for a variable index, so the cost moves from the write to every waiter.
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

- The clocked 4096-entry memory, 2026-09-25, `--release`: **0.39 s on the execution backend and 0.25
  s on the C++ backend**, from 8.7 s and 3.07 s.
- A part lent to a `ref` formal is not a place a write lands on: the write would report when the
  call's full-expression ends, and a write through a reference is due when it lands.
  [a-lent-part-carries-its-variable](a-lent-part-carries-its-variable.md) lends it as a reference
  that carries the variable.
