# A reference binds a cell, and every referent is one

Date: 2026-08-27 Status: accepted, reopened 2026-09-03 (see the closing section)

## Context

MIR fixes what a reference is. It is a data type, not a direction
([reference-as-data-type](reference-as-data-type.md)); it is a capability wrapper, so the storage it
stands for is named one dereference further and the protocol realizing that access comes from the
type ([storage-access-as-place-formation](storage-access-as-place-formation.md) D1, D4); and it is
not a borrowed pointer, because a write through it fires the destination's update event where a
write through a raw address does not.

The execution backend realized it as the address of whatever storage the referent happened to be.
That is one realization for what are, on this backend, several kinds of storage. A procedural local
of an ordinary body is a frame slot holding a value handle, and its address is a slot address a load
and a store reach directly. A signal is a runtime-owned cell, and reaching its contents is the
cell's own protocol, because writing it has to raise the update event LRM 9.4.2 requires. Lending a
signal was therefore refused, and the refusal was load-bearing: without it the callee reads the
cell's own bytes as if they were the value.

What forces one answer rather than two is the callee. A subroutine has one formal, lowered once, and
its type cannot vary with the storage a caller happens to lend it -- so whatever the reference is,
it is that for every referent.

## Decision

**A reference is the address of a value cell, and every storage a reference binds is one.**

1. **A reference's LIR type names the cell, not the value.** `mir::RefType{pointee = T}` becomes a
   reference to a cell of `T`, so the place a reference names is two steps: opening the reference
   reaches the cell, and reaching through the cell names the value. A signal member's place is
   already the cell, so lending it is taking its address and nothing more.

2. **A local whose storage is lent by reference lives in a cell.** MIR-to-LIR already had to know
   which locals need an address; those become cell locals instead of frame places. The cell is built
   where the local is declared, so each entry to that declaration begins a fresh variable, and the
   declaration's initializer is the write that installs the cell's representation.

   **What kind of storage a local gets and how long that storage lives are separate questions.**
   Being lent decides the kind, and it decides it for every local; how long it lives is the
   declaring scope's, because that is what an automatic local's lifetime is. So the cell is a slot
   of the body's own frame, begun where the declaration runs and ended on every way out of the
   scope, including the one the driver takes when it ends the execution instead of resuming it.
   Fusing the two questions is what made a lent local of a suspending body unreachable: it was given
   the storage whose lifetime it needed, and with it a kind no reference can name.

   **This is where the reopening below landed, and 2026-09-16 is where it ended.** Unfusing the two
   questions was the right move and stopped one step short: being lent still decided a kind, so two
   kinds still existed and a variable needing both still had none. What
   [a-declared-variable-is-one-storage](a-declared-variable-is-one-storage.md) settles is that being
   lent decides nothing either -- a declaration is one storage, and a reference binds the storage
   the variable already is.

3. **That cell is the signal cell, not the non-observable procedural one.** The two share a storage
   core but not their spelling at the boundary: a cell's address crosses as one `void*`, every cell
   entry reads it as the same type, and the trigger entry already requires that address to serve as
   the observable. One cell kind behind a reference is what keeps that `void*` meaning one thing.
   Nothing subscribes to a procedural local -- no lowering registers one as a signal -- so the
   update event a write to it raises wakes nobody.

4. **The cell type is reached from two places and must be one type.** A reference's type is built
   where a declared `ref` is translated, and again where the lowering gives a lent local a cell, and
   a reference built either way has to be the type the formal was declared with. Both spell it
   through one builder, so the shape is decided once; that the two results are one identity is what
   a type pool keyed by content answers, and [lir-type-interning](lir-type-interning.md) is where it
   is settled.

## Invariants

1. A reference names a cell. There is no reference to a value's own storage, and no second reference
   realization to tell apart at a use site.

2. Reaching a value through a reference is opening the reference and then reaching through the cell.
   The second step is the cell's access protocol, which is what makes a write through a reference an
   update event when the cell is a signal's.

3. A local lent by reference is a cell local. Its own reads and writes go through the same cell
   protocol as a signal's, so lending it adds no second way to reach it.

## Rejected

- **The address of the referent's own storage** -- the shape this replaces. It is one realization
  for storage kinds whose reads and writes are different operations, and the callee cannot tell them
  apart: the formal's type is fixed, so a cell address arriving where a slot address is expected is
  read as the value's bytes. Its only defence was the refusal that kept a signal from ever being
  lent, which is a gap standing in for a model.

- **A runtime reference object that records which storage it views**, the way the C++ backend's
  reference does with a cell pointer beside a plain one. It is correct, and it is what a backend
  with real stack storage needs, but here it adds an object per bind whose lifetime then has to
  outlive whatever captures the reference -- and it buys nothing this decision does not get from
  making the referent a cell.

- **A polymorphic storage core, so a reference could name either cell kind.** The honest version of
  the previous alternative, and the one that would let a reference bind the non-observable
  procedural cell too. It cannot be reached from a `void*`: the boundary hands a cell's address as
  one pointer and the trigger entry already reads that pointer as the observable, so a second base
  at a different offset in the same object has no spelling there.

- **Giving the lent local the non-observable procedural cell.**
  [cross-suspension-value-storage](cross-suspension-value-storage.md) rejects reusing the signal
  cell for a procedural local, on the ground that a procedural write is not an update event and the
  observable base is not needed. That reasoning stands where it was made -- a local that merely
  crosses a suspension keeps the procedural cell. It does not reach a local whose storage is lent,
  because that local's cell has to be the one kind a reference can name at all, and the previous
  alternative is why there is only one.

## Consequences

- A `ref` / `const ref` formal reaches a signal, so a write through it lands in the caller's
  variable and wakes a process sensitive to it. A nonblocking assignment's destination, an increment
  of a signal, and a strobe's captured destination are all the same bind: the first two run, and the
  strobe waits on the file service that renders it rather than on anything about references.

- A local lent by reference costs a cell-sized slot of the frame its body already has, begun and
  ended by the compiler the way a C++ local's constructor and destructor are. It is not a new
  lifetime class and it accumulates nothing: a declaration reached many times uses the one slot.

- A suspension therefore has a second successor. The driver may end an execution that is parked
  rather than resume it, and that is a way out of every scope the body has open -- so each
  suspension names the block that ends what those scopes began, which the destroy path runs before
  the frame goes. No statement of the body runs there; being ended is not something the body
  observes.

- Storage that is not a cell still cannot be lent, and each remaining case is a question about where
  a value lives rather than about what a reference is: a class property is a member owning its value
  rather than a cell holding it, and a part of a value aggregate has no independent storage to lend
  at all. The second is not answered by giving it storage --
  [value-projection-write](value-projection-write.md) D4 fixes it as an owner-relative projection
  reference, which names no interior pointer, and what that costs here is a second constitution for
  a reference rather than a second kind of cell.

- A formal that lends what it was lent hands on the alias it holds rather than binding storage
  afresh, because a referent that is already a reference of the formal's own type denotes the
  storage at the end of the chain and never the reference in between (LRM 23.3.3.2).

## Reopened: this settled how a place is lent by settling how every place is represented

The contract this decision answers a question from -- each backend states, per place type, how a
load through it is realized, how a store through it is, and how it is lent -- has the place deciding
all three. What this decision did instead was let the third determine the first two:

> A constraint on how a place is lent was allowed to determine how the place itself is represented.

That is backwards, and the cascade it produced is the reason to say so rather than leave it. Every
referent became one cell kind; a plain local therefore acquired a cell's lifetime; that lifetime had
no obvious owner, so it was approximated by whichever region was nearby -- the per-call one, then an
execution-lifetime one when the first was wrong across a suspension, then a slot of the frame when
neither could give a stable address. Three storage regimes for one concept, none of them a property
of the variable.

The driver was the one-word reference: a single pointer cannot say which of two layouts it names.
That is true of a single pointer and not of a wider reference, and the two nearest rejections above
do not reach the wider one. "A polymorphic storage core" rejects one pointer over two layouts. "A
runtime reference object that records which storage it views" is rejected for adding an object per
bind, which assumes a heap object -- the C++ backend's is a pair of raw pointers, passed by value,
allocating nothing -- and for buying nothing, which is the part that does not hold: it buys not
turning every plain local into a cell, and that is the root of the cascade. The cascade had not
happened yet when this was written.

The same rejection names its own reopening condition -- "it is what a backend with real stack
storage needs" -- and that condition now holds: a value's storage can live in a frame slot sized by
what the runtime states, with the compiler emitting both ends, on every way out including the one a
driver takes when it ends a parked execution rather than resuming it.

**What replaces it is a contract rather than a layout**: a reference carries the referent's address
plus enough erased place-class information to perform the three place operations -- load, store, and
re-lend -- without exposing value representation. Plain-versus-signal is not the permanent set; an
array element, a class property, an interface member and force/release-aware storage are all place
classes this compiler will meet, so a two-valued tag would force the ABI open again for each.

That contract is not written yet, because what "the referent's address" points at is not yet
decided. Two questions were fused here and only the first is settled:

- **Settled.** A lending requirement must not decide a place's representation.
- **Open, and now answered.** Whether a place owns a value's representation, or holds a handle to an
  independently lived one. The second is already written down --
  [jit-value-realization](jit-value-realization.md) invariant 6 makes a value handle aliasable by a
  copy and every apparent mutation functional, which is what leaves a procedural local's
  representation ownerless. What is new is that the same backend does the first everywhere else: a
  signal's storage holds its value by value, and so does a lent local's. The two answers want
  different things from a reference -- the address of a slot holding a handle, or the address of the
  representation itself. [storage-owns-its-value](storage-owns-its-value.md) takes the first: a
  storage entity owns its value's representation, and a reference is pointer-like to that storage.

This decision stands as the description of what the execution backend does, and not as the reason it
should. What replaces it is the contract its reopening asked for, which the answer above makes
designable but does not itself write.

## Cross-references

- [reference-as-data-type](reference-as-data-type.md) -- a reference is a data type at MIR, and is
  not a borrowed pointer because a write through it fires the destination's update event.
- [storage-access-as-place-formation](storage-access-as-place-formation.md) -- a dereference of a
  wrapper's place names the storage it represents, and each backend supplies the protocol from the
  place's type, including how a place is lent by reference.
- [cross-suspension-value-storage](cross-suspension-value-storage.md) -- the procedural cell, and
  the rejection this decision bounds rather than reverses.
- [jit-value-realization](jit-value-realization.md) -- the opaque-handle baseline that makes a
  cell's address one `void*` with one meaning.
- [lir-type-interning](lir-type-interning.md) -- why a cell type reached from two places is one
  type, which this decision needs and does not itself settle.
