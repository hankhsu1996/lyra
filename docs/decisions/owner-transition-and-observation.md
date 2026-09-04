# A mutation reports its owner's transition; an observation compares its own expression

## Date

2026-09-02

## Status

Accepted

## Why this decision matters

A write to observable storage discards what it wrote. The cell then reconstructs two separate facts
from one whole-value comparison: whether the variable changed, and whether a watching process should
wake. Both reconstructions are wrong, in different ways, and they are the same defect seen twice.

**The cost.** A partial write copies the owner's whole value, mutates the copy, compares the copy
against the stored value, and stores it back. Writing one element of a 32768-element unpacked array
costs 1.55 ms against 1.16 us to read one element back; the value is 168 bytes, so 5.5 MB moved in
1.55 ms is 3.5 GB/s, the measuring machine's memory bandwidth. The write is a copy of the whole
array running at the speed a copy runs, so the cost of a write tracks the size of the array it lands
in rather than the size of what it writes.

**The correctness.** A subscription names a bit window in the observable's flat bit address space,
which is the shape a packed value has and no aggregate has. An aggregate subscription therefore
collapses to "the whole variable", and `always @(mem[3])` wakes on every write to `mem[4]`. IEEE
1800-2023 9.4.2 forbids exactly this: "a change of value in any operand of the expression without a
change in the result of the expression shall not be detected as an event."

## Findings that shaped the decision

### F1. Only the packed family has positionally addressable, totally ordered parts

An integral type is a bit width, a signedness, and a state domain (LRM 6.11.1); a packed array is a
contiguous bit plane and multidimensional access is offset arithmetic on it (LRM 7.4.1). So any two
parts of a packed value either overlap or do not, and overlap is interval intersection.

No other value family has this. Unpacked arrays, queues, dynamic arrays and associative arrays have
coordinates rather than intervals, positions that move, or no positional stability at all. Taking
one family's shape as the vocabulary every observable uses is what forced the aggregate case to
collapse.

### F2. The standard separates a variable's update event from an expression's implicit event

LRM 4.3 makes every change in state of a net or variable an update event, and processes sensitive to
it are "considered for evaluation". LRM 9.4.2 makes an implicit event a change in the value of an
_expression_, and its operand-versus-result sentence is coherent only if the two are distinct.

Different constructs use different amounts of that structure: `always_comb` (LRM 9.2.2.2.1) is
sensitive to its read variables and has no event expression, while `@(mem[3])` uses the variable's
event to become a candidate and the expression's value to decide.

### F3. Forming a writable designation can itself change the owner

LRM 7.8.7: an associative entry is allocated "when it is used as the target of an assignment or
actual to an argument passed by reference", "with its default or user-specified initial value before
any reference to that element", and explicitly so for a read-modify-write such as an increment.

So a task taking `ref int x`, called as `foo(aa["x"])`, changes `aa` even when its body does
nothing: `num()` and `exists("x")` both move, and no write ever occurs. A model in which only an
applied write can transition the owner cannot express this.

LRM 7.8.6 completes the picture: a _read_ of a nonexistent entry warns and returns the nonexistent
entry value without allocating. Formation timing therefore follows the binding mode, not the
designation: a `ref` actual allocates at call entry, while an `output` or `inout` actual allocates
at its writeback, because that writeback is an assignment.

### F4. Every designation that outlives its application has pure formation

LRM 10.4.2: "It shall be illegal to make nonblocking assignments to automatic variables or to
elements of dynamically sized array variables."

The associative array is the only family whose formation allocates, and it is a dynamically sized
array. So a nonblocking assignment -- the one construct that evaluates a designation now and applies
it later -- can never designate a place whose formation has an effect. This is a property of the
standard rather than of any implementation, and it is what makes one designation concept sufficient.

### F5. The descent is already realized in place; only the root is not

The C++ backend renders a value-projection write as a chain of in-place accessors over the result of
opening a mutation scope on the owner. The accessors return references and already carry the LRM
7.4.5 and 11.5.1 corner cases. What copies is the scope: it snapshots the owner's whole value on
entry and commits it on destruction.

The same scope serves a whole store, so a whole assignment pays the partial-write price -- a
snapshot read purely to be overwritten, then a comparison and a copy.

The scope is also where the C++ spelling of an access is written by hand. Reading, storing and
lending each format a method call of their own -- a receiver, a name, arguments -- at a site that is
not the one every other call's spelling comes from. A method name that lives in two places is the
shape `backend_contract.md` invariant 3 exists to prevent, and it is what makes the store's spelling
diverge from the way `Set` would be spelled if it were reached as a call. D5 removes two of the
three.

### F6. A retained reference into a snapshot silently discards concurrent writes

A `ref` actual bound to an interior lends a reference into the snapshot rather than into the
storage, and the scope commits that snapshot when the calling expression ends. Anything else written
to the same variable while the callee runs is therefore overwritten by a value read before the call:

```systemverilog
int a[0:3];
task automatic t(ref int x);
  a[0] = 42;
  x    = 7;
endtask
initial begin
  a = '{1, 2, 3, 4};
  t(a[1]);          // '{42, 7, 3, 4} is required; '{1, 7, 3, 4} is produced
end
```

LRM 13.5.2 requires the opposite: "any changes made to the argument, within either the caller or the
subroutine, shall be visible to each other", and "changes are seen outside the subroutine
immediately (before the subroutine returns)". So the defect is a lost update, not a stale read, and
it needs no aggregate to reproduce.

### F7. A retained reference into a variable-size family denotes an element, not a coordinate

LRM 13.5.2 gives an element passed by reference a lifetime of its own: it "shall continue to exist
within the scope of the called subroutines until they complete", and if the element is removed
first, later writes through the reference "shall not be visible outside" -- the reference has become
an _outdated reference_.

LRM 7.10.3 then fixes which queue operations do this, and one entry rules out the obvious
realization: `insert`, `push_back` and `push_front` "can never give rise to outdated references",
while a whole-queue assignment outdates every element reference and a removal outdates only what it
removed. A reference held across a `push_front` must therefore still name the element it named
before, though every position has moved. A retained designation into these families cannot be an
owner plus a coordinate resolved at write time.

## The decision

### D1. An observable owner's transition is the one boundary every mutation reports through

Every semantic operation on observable storage answers one question: did the observable owner
transition. Publication is a function of that answer alone.

Forming a designation, applying a designated write, a container operation such as a queue push or a
dynamic-array resize, and a net's re-resolution all reach publication through this one boundary.
They share no mutation machinery -- only the question. This is what lets a container operation
participate in publication without being a store, which keeps the standing constraint that a cell's
whole-value store never grows semantic-store behaviour
([unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md)).

### D2. A write or reference target is one semantic designation

A designation is the observable owner plus the evaluated selector information identifying the
target, as [value-projection-designator](value-projection-designator.md) already states it. It can
be retained where the language's timing requires it: a nonblocking assignment evaluates its
coordinates where the statement executes and applies them in the update region, and a `ref` actual
keeps it live for the callee's lifetime.

A retained designation names the storage, never a copy of it. A write through one is visible to
everything else reading that variable at the moment it happens, and a write to the variable by
anything else is visible through it (LRM 13.5.2); a realization that lends a reference into a
snapshot satisfies neither and loses whichever write commits second (F6).

What the selector information has to identify follows the family. Where positions are fixed, a
coordinate identifies an element for as long as the designation lives. Where they are not -- the
dynamic array, the queue, the associative array -- a retained designation identifies the element
itself, because operations that renumber positions are required to leave a reference naming what it
named before, and only removal of that element makes it outdated (F7).

One semantic concept covers every case. Its realization has a formation step, which is D3.

### D3. Forming a designation performs its domain's formation effects and reports the resulting transition

For a domain whose formation is pure, that transition is "no change". For the associative array it
is the allocation of F3, and it publishes where it happens rather than being folded into a later
write.

By F4, every designation that a nonblocking assignment defers has pure formation. That guarantee
comes from the standard, so no separate "addressing information" and "materialized place" concepts
are needed; a designation is one thing whose formation is a per-domain operation.

### D4. Applying a write through a designation computes the transition from only the state the operation can affect

A partial write never compares portions of the owner it cannot have reached. A whole assignment is
the same operation over an empty selector path and pays the whole value, which is what a whole
assignment costs.

The comparison belongs to the value-domain operation, not to a caller that projects first and
compares afterwards. A partially out-of-range part-select writes only its in-range bits (LRM
11.5.1), so the bits to compare are the bits the write will actually reach, which only the domain
knows; and under F3 a projection can allocate before any comparison could be taken. The domain owns
the whole state transition, not merely the projection.

Other semantic mutations are not stores. A queue push, a dynamic-array resize and a delete are
operations of their own domains that report transitions through D1.

### D5. Reading an observable wrapper's storage and replacing the whole of it are calls; naming it is not

[storage-access-as-place-formation](storage-access-as-place-formation.md) D2 holds that reaching
represented storage is never a call, because "its entire content is the place and the fact that the
storage is wanted, both recoverable from the node and its type". Which of the ways of reaching it
that describes is what D1 through D4 above settle, and it turns out to describe one.

Naming the storage as an lvalue is the one. It yields nothing of its own -- it hands the storage to
whatever will operate on it, whether that is a callee taking it by reference or a designator
descending into the value it holds -- so the node and its type do carry the whole of it, and it
stays a dereference. Those two consumers are why the answer is one entry rather than one per
consumer.

A load does not. What it yields is not the wrapper's place but the contents of whatever storage the
wrapper currently stands for, and which storage that is depends on the wrapper's own state -- a
reference bound to one of several backings answers from the binding, not from its type. Deciding
that is an operation on the wrapper. The same entry's D6 requires it independently: reads and writes
carry one rule together, so making the write a call and leaving the read an access would put a
single wrapper's protocol in two places, which is exactly the duplication a one-site access protocol
exists to prevent.

A store does three things this entry establishes as semantic. It performs its domain's formation
effects, which can allocate (D3). It computes the owner's transition from the portion it could
affect, which the domain owns rather than a caller (D4). And it publishes that transition to
whatever is watching (D1). That last one acts on the wrapper as an object -- its place in the object
graph -- which is precisely what the same entry's D3 classifies as a call. A store is therefore an
operation on the wrapper, not a way of naming storage, and it is stated as an ordinary call whose
operands are the destination and the value.

**What the forbidden shape was actually about.** `mir.md` enumerates `Call(set, [cell, value])`
beside "a proxy call a consumer must recognize to recover the destination it stands for", and gives
one reason for both: stating a realization "obliges every other consumer to decode that realization
back into the destination". That reason holds of the proxy and not of the store. A mutation proxy
returned a handle that stood for a destination, so every consumer had to decode it -- and the
execution backend did exactly that, which is the evidence the superseded entry cites. A store names
its destination as an operand and yields nothing to decode. The enumeration was broader than the
reason it rests on, and is narrowed to the shape the reason describes.

**The proxy does not survive as a destination.** No scoped handle stands for a place a consumer must
decode back: a whole replacement names the wrapper among its operands, and a partial write names a
designation. What a backend does behind the designation, including a scoped handle that publishes
when the descent ends, is its own realization and is recognized by nobody above it
([value-projection-designator](value-projection-designator.md) D8).

**What stands unchanged.** That entry's D1 -- a wrapper's place and the storage it represents are
distinct, so rebinding and writing-through are different programs -- stands and is sharpened:
rebinding is a store into the bare place and replacing the whole of what it holds is a call, which
are different node kinds rather than the same node at different dereference depths. Its D3, the
dividing question of what an operation acts on, is what decides this case.

**Where this leaves a whole store and a partial one, which is not where that entry left them.** Its
D5 held that the two are one operation at different path lengths. Under D3's dividing question they
are not: replacing the whole of what a wrapper holds acts on the wrapper, while writing a part acts
on the value the wrapper holds and reaches the wrapper only to name where that value lives. So they
are two node kinds -- a call, and an assignment to a designation whose owner is a dereference -- and
the unity that entry claimed is given up rather than kept. What is gained for it is that the first
no longer needs a place-access answer of its own.

**Consequence for a backend.** The C++ spelling of a load and of a whole store comes from the same
table every other call's spelling comes from, reached through the one call render; no per-access
entry formats a method call of its own. The place-access dispatch is left with the single question
of how the storage behind a place is named as an lvalue, which a by-reference binding and a
designator's owner both ask.

### D6. Event detection belongs to the armed observation, and compares consecutive values it observed

An event control arms an observation when execution reaches it, taking its expression's current
value as the baseline, and disarms when the observation fires. A variable's transition makes an
observation a candidate; the observation decides by comparing its expression's value against the
baseline, with the edge rules of LRM 9.4.2 Table 9-2 applied to the expression's least significant
bit.

A candidate that does not fire advances the baseline. Otherwise comparisons are not between
consecutive observed states: an observation armed on `posedge clk` while `clk` is 1 sees a
transition to 0 that does not fire, and a later transition back to 1 would then compare 1 against 1
and miss a posedge.

The baseline, and any state a future optimization keeps beside it, belong to the armed observation
instance and are destroyed when it disarms. That is what makes LRM 9.7 resensitization fall out:
while a process is not waiting at an event control, no observation exists there, so a change during
that window is missed.

Sensitivity that names variables rather than an expression -- `always_comb` and `always_latch` (LRM
9.2.2.2.1) -- arms an observation whose detector fires on any candidate. It is one alternative of
the same detector set, not the absence of one.

### D7. Information that narrows which observations to evaluate is optimization only

A mutation may carry domain-specific information about what it could have affected, and the runtime
may use it to prove an armed observation unaffected. **Such information may only eliminate candidate
observations; it may never be required to reach a correct answer.** Failure to prove means evaluate.

Precision is therefore backend-specific and monotonic, and reporting nothing beyond "the owner
transitioned" is the reference implementation. A backend that can descend in place may narrow; one
whose value realization materializes intermediates need not.

Any state used for this purpose belongs to the armed observation and must advance whenever
evaluation changes that observation's effective dependencies -- an observation of `mem[idx]`
narrowed to element 3 must move to element 7 when `idx` does, or a write to element 7 is wrongly
eliminated.

**What a write states about the portion it reached is not this, and the two are told apart by
whether an answer of "I do not know" is available.** LRM 9.4.2 decides an edge from the transition
at the expression's least significant bit, so a write to part of a packed value has to say which
bits it reached: a waiter on a bit the write did not touch has not seen an edge, and one on a bit it
did needs that bit's transition. Nothing may answer "I do not know" there without answering the
standard's question wrongly, which makes it part of D4's transition rather than an instance of the
narrowing above. The narrowing is optional in the strong sense -- always answerable with nothing,
always still correct.

- **A mutation scope that borrows the owner's whole value has no role.** A designation with
  formation and application replaces it, and the four constructs that reach an interior -- a
  blocking assignment, a nonblocking assignment, a `ref` or `output` or `inout` actual, and a
  compound assignment -- become one shape rather than four.
- **A `ref` bound to an interior becomes the live alias it is specified to be**
  ([value-projection-designator](value-projection-designator.md) D4), because the write reaches the
  owner where it happens rather than when an enclosing scope commits.
- **A projection reference must be able to designate an element that does not exist**, with
  allocation at the writeback, because an `output` or `inout` actual binds once at call entry and
  writes back at completion (F3).
- **A whole assignment stops paying the partial-write price**, because it is the empty-path case of
  the store call rather than a lend that reads what it is about to replace (D5, F5).
- **A store's target-language spelling stops being written by hand.** It comes from the same table
  every other call's spelling comes from, reached through the one call render, so the receiver's
  form and the argument order are decided once for every call rather than again per access (D5).
- **Both backends consume one node.** A store is a call in MIR, so the C++ backend renders it as a
  call and the execution backend lowers it as one, through the paths each already has for every
  other call. Neither recognizes a shape peculiar to storage access, which is what the superseded
  arrangement required of the execution backend.
- **The representation of D7's narrowing information is deliberately absent.** Neither defect this
  entry addresses needs it: a write that compares and mutates only what it reaches is what removes
  the cost, and an observation that compares its own expression is what removes the wrong wakeups.
  The representation is designed when a measured consumer exists, together with the matching side
  that would interpret it.

## Rejected alternatives

- **Make the written region a semantic event granularity.** A write would report the region it wrote
  and the cell would match regions against subscriptions. Rejected: LRM 4.3 has update events on
  variables and LRM 9.4.2 has detection on expressions, and there is no third level between them; a
  region granularity invents one. Matching regions also requires comparing coordinate paths at
  runtime, which is the selector vocabulary below LIR that
  [value-projection-designator](value-projection-designator.md) already rejects. Under D6 and D7 the
  same information is available as an elimination hint with no semantic standing, which is where it
  belongs.
- **Give every value family one flat position space so a subscription and a write are both
  intervals.** Rejected: it works for the packed and fixed-shape families and fails for the
  variable-shape ones, whose positions move or do not exist, so those families would keep the
  whole-variable subscription this entry exists to remove.
- **Hold change detection on a design-global table of sensitivity expressions, evaluated on a
  schedule.** Rejected: assigning table indices at compile time requires a whole-design view, which
  `north_star.md` invariants 2 and 5 forbid, and polling every entry per region iteration needs a
  static schedule with a convergence loop that an event-driven engine does not have.
- **Give each container a write-proxy type that reports upward what it touched.** Rejected for the
  reason [runtime-shape-and-default-value](runtime-shape-and-default-value.md) D4 gives: a
  per-container proxy carrying a validity flag and forwarding the compound-operator set duplicates
  that operator set on every container. A per-domain operation that performs one designated write
  and reports a transition forwards no operator set and is not this shape; compound assignment stays
  a read, a computation on the element's own type, and an application.
- **Split the designation into evaluated addressing information and a materialized writable place.**
  Rejected: F4 shows the case that would require it cannot arise, because the only formation with an
  effect belongs to a family a nonblocking assignment may not target. Recorded as a load-bearing
  dependency on LRM 10.4.2: were that restriction lifted, the split becomes necessary.

## Relation to existing decisions

- [value-projection-designator](value-projection-designator.md) -- D1 to D4 there define the
  designation this entry realizes; D7's single writeback and whole-cell reactivity are unchanged;
  D8's in-place recovery is what D4 here makes the domain's own operation rather than a projection a
  caller composes.
- [value-projection-write](value-projection-write.md) -- the functional whole-value update stands as
  the semantics; D4 here fixes what computes the transition, not what the write means.
- [storage-access-as-place-formation](storage-access-as-place-formation.md) -- **D5 here supersedes
  its D2 for a read and for a whole store**, and with it those answers of its D4; its D1 and D3
  stand, and D3 is what decides the supersession. **Its D5 is given up**: a whole store and a
  partial one are not one operation at two path lengths, because D3's own question separates them.
  D4 here makes the cost follow the path length, which that entry did not.
- [jit-value-realization](jit-value-realization.md) -- invariant 6 is why D7's precision is
  backend-specific: a backend holding handles a copy may alias cannot mutate in place, so it reports
  the transition without narrowing.
- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- its constraint that a
  cell's whole-value store never grows semantic-store behaviour is what D1's separation of the
  transition boundary from mutation machinery preserves.
- [activation-registration](activation-registration.md) -- an armed observation's state lives on the
  record the activation owns, which is where D6 places the baseline.
- [read-set-inference](read-set-inference.md) -- the read set decides when an observation becomes a
  candidate; D6 keeps that separate from whether its expression changed.
