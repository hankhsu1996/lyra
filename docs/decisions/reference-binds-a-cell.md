# A reference binds a cell, and every referent is one

Date: 2026-08-27 Status: accepted

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
   where the local is declared, so each entry to that declaration is a fresh variable with its own
   storage, and the declaration's initializer is the write that installs the cell's representation.

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

- A local lent by reference costs a cell for the enclosing generated call. That is the same
  accumulation any transient in a loop already has under the opaque-handle value model, not a new
  lifetime class.

- Storage that is not a cell still cannot be lent, and each remaining case is a question about where
  a value lives rather than about what a reference is: a suspending body's local lives in the
  activation frame, a class property is a member owning its value rather than a cell holding it, and
  a part of a value aggregate has no independent storage to lend at all.

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
