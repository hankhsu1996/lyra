# A declared local is storage

Date: 2026-09-16 Status: accepted

## Context

The lowering into the execution IR gave a body's local one of two treatments. Either the local
became storage -- a frame slot, written and read through -- or it became no storage at all: the
value its declaration produced was substituted at every mention, and the variable appeared in the
lower IR nowhere.

Which of the two a local got was decided by a pass over the whole body, run before any of it was
lowered, marking every local that something writes. A local nothing writes needs no storage, so
substituting its value preserves behaviour and saves a slot.

That mark recognized a write naming the local outright: an assignment to it, an increment of it, its
address taken, a method applied to it that changes what it is applied to. A write that designates a
**part** of the local -- a member of an unpacked structure, an element of an array, a field of a
packed value -- names the designation rather than the local, so it marked nothing. The local was
then substituted, and the write, which needs the storage the changed whole value goes back into, had
none. Four conformance cases stopped there, each of them ordinary SystemVerilog: a whole-structure
assignment, an array assignment pattern, an aggregate argument passed by value, and a structure
returned from a function.

## Decision

**D1. A local has storage because the source declared a variable.** Nothing about what the body does
with the variable is consulted, and no state exists in which a local has none. This is what
`../architecture/storage.md` invariant 1 already says of every variable; the lowering was deciding
it a second time, from worse information.

**D2. Where that storage lives follows the local's own lifetime, and nothing else.** A local lent by
reference gets a cell, because that is the one storage a reference can name; a value-typed local of
a suspending body gets a cell of the execution's own store, because its value has to outlive the
stretch that made it; every other local gets a frame slot. Whether those three should be one is a
question this does not settle; what it settles is that "no storage" is not among them.

**D3. Keeping a variable out of memory is a saving taken by whoever sees the whole function.** A
step that translates one node at a time cannot answer whether a variable needs to exist, because the
answer depends on every other node. Where a saving of this kind is taken upstream, its failure mode
is a refused program; where it is taken by an optimizer, its failure mode is a slower one. Only the
second is acceptable, and the first is what this replaces.

## Why the mark was not repaired instead

Teaching the mark to follow a designation down to the local it bottoms out in fixes the four cases
and leaves the shape. The mark would still be a second statement of which entities a write reaches,
kept in step with the sites that reach them by nothing, and every later construct that names storage
a new way reopens the same hole with no signal that it has. The saving it buys is one slot per
never-written local. Removing the decision removes the class of defect; repairing it removes one
instance.

## Survey

Both established answers put this below the semantic IR, which is where ours already sat; what
differed was the rule.

- **Clang does not decide at all.** It emits a stack allocation for every mutable local and leaves
  promotion to the optimizer's `mem2reg`. LLVM's own frontend guidance states the technique and
  names clang as its user, recommending it "unless there is an extremely good reason not to"
  (<https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl07.html>).
- **rustc decides in code generation, and starts from the write.** Its analysis marks a local as
  needing memory for any mutating use carrying any projection at all -- "if there are _any_ then we
  need a place to write. (For example, `_1 = Foo()` works in SSA but `_2.0 = Foo()` does not.)" --
  and a projection it does not recognize falls to memory rather than to an error
  (`rustc_codegen_ssa/src/mir/analyze.rs`).

The condition that looks like it separates us from clang is that this backend runs no optimization
pass, so the promotion clang relies on does not happen here. That is a gap rather than a condition:
it stops being true the moment such a pass is added, and differing from the field on account of one
keeps a decision upstream in order to accommodate a layer nobody has filled in -- which is also what
stops that layer being worth filling in. A missing pass is a thing to build, never a reason.

## Rejected alternatives

- **A semantic layer stating which locals need an address.** The fact is not a semantic one. Every
  variable has storage; "nothing writes this one" is a summary of a body, useful only for deciding
  whether to emit the storage, which makes it an optimization result. Putting it in a semantic layer
  gives every consumer of that layer a fact it must not act on.
- **Keeping the substitution and treating the refusal as a missing case.** This is the reading that
  produced the four cases: each new form of write arrives as a gap to be added rather than as
  evidence that the question is being asked in the wrong place.

## Consequences

- A local's possible homes lose one alternative, so a consumer asking where a local lives has one
  fewer answer to handle, and the state that produced the refusal cannot be spelled.
- The whole-body pass that ran before lowering keeps one job: marking the locals something lends. A
  local it misses is refused by name where the lending is attempted, rather than silently given the
  wrong home.
- Reaching a local's storage cannot fail for want of storage. What remains is a local whose value
  crosses a suspension, which lives in the execution's own store and is reached by the calls that
  read and write it, so it has no address for anything that needs one to point at; that refuses and
  says so.
- Every local now costs a slot in the generated frame. Nothing measures this path's speed today --
  the benchmark corpus drives the C++ backend -- so the cost is stated rather than measured, and the
  layer that would remove it is the one named above.

## Cross-references

- `../architecture/storage.md` -- what has storage identity, and why left-hand-side position is not
  the test for it.
- `storage-owns-its-value.md` -- a storage entity owns the representation of its current value, and
  a component the language gives identity is itself storage.
- `reference-binds-a-cell.md` -- why a lending requirement must not decide how a storage is
  represented, which is the remaining half of D2.
- `jit-value-realization.md` -- the opaque-handle baseline, and the three-regime observation this
  reduces by one.
