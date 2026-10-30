# What copying, moving, writing over and ending a value of a type does is stated in LIR

Date: 2026-10-30. Status: accepted. Revises who builds the steps of a value's life in
[a-tuple-is-laid-out-by-its-type](a-tuple-is-laid-out-by-its-type.md),
[a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md),
[a-fixed-array-is-laid-out-by-its-type](a-fixed-array-is-laid-out-by-its-type.md) D9,
[a-fixed-arrays-operations-are-stated-per-unit](a-fixed-arrays-operations-are-stated-per-unit.md),
[an-object-model-fixed-at-compile-time](an-object-model-fixed-at-compile-time.md) D9 and
[an-operation-over-many-types-is-built-by-the-lowering](an-operation-over-many-types-is-built-by-the-lowering.md)
D3.

## Context

MIR states who owns each value and until when, and no node per copy or end
([../architecture/mir.md](../architecture/mir.md)). The lowering to LIR places every copy, hand-on
and end that follows. What each of those does for a value of a given type -- a structure's members
in turn, an array's elements in a loop, a tagged union's one live member, what an owning pointer
points at, a closure's captures -- and how an object of a class is created and ended, its base's
parts and its own members begun and ended in order, was decided by the LLVM code generator, which
built a function per type and step and per class, and asked again in several places whether ending a
value of a type does anything. The C++ backend takes the same rule from the host language's implicit
special members, so nothing held the two backends' answers in step, and a backend was deciding
operations ([../architecture/backend_contract.md](../architecture/backend_contract.md)).

## Decision

**D1. Each step of a value's life is stated once per type, in LIR, by the lowering from MIR.** The
steps are the ones the runtime's table of a type asks: copy into storage holding nothing, move out
of a value whose holder still ends it, write over a value already there, and end. One query of LIR
answers how a step of a type is carried out: the value's bytes, the library's entry for what the
value is held as, a function the unit states, the definition the value opens with, or, for an owning
pointer's end, what it points at, ended where the pointer ends and its storage then given back; and
whether ending a value of a type does anything is LIR's answer, which the code generator reads where
it lays a value out and states nowhere itself. Whether ending storage of the library's -- a
variable's cell, a net -- does anything is the library's statement of that storage.

**D2. Where the answer is a function, the lowering builds it from the type's parts**, as rustc
builds drop glue and clone shims: a product's components in order, an array's elements in a loop
counted by a machine integer, the member a tagged union's tag names, and a closure's captures. A
copy and a move answer the value they build in the body's return place, built part by part there. A
value laid out by its type has a function for every step it has, its bytes or not, because the
library's table names one.

**D3. Each class this unit defines that a value is built of has its creations and ends stated the
same way**: the parts it places begun -- what it extends, then its own members -- and ended, last
begun first; the object created whole in storage of its own and given its class before any
constructor runs (LRM 8.17 NOTE 2); and ended whole, its storage given back. Each is a function
under the symbol a class extending it, or an object of it, reaches it by.

**D4. The code generator translates.** It carries out a step as the query answers -- the bytes at
the type's layout, the library entry, a call -- and realizes beginning and ending storage at a
place, and giving an object its class, by where the layout puts each. How a call into a type's table
crosses the runtime's C interface stays its own, since that is the target's spelling of a call
([../architecture/lir.md](../architecture/lir.md) Does Not Own).

## What the field does

- **rustc** builds drop glue and clone shims per type as MIR bodies (`build_drop_shim`,
  `build_clone_shim`, `compiler/rustc_mir_transform/src/shim.rs`); its MIR is the layer Lyra's LIR
  plays. Its code generator resolves the glue and emits nothing where the glue is empty
  (`compiler/rustc_codegen_ssa/src/mir/block.rs`), and never walks a type itself. A `Box`'s drop is
  the exception: drop elaboration writes it out where the box is dropped, its contents dropped and
  then its storage freed (`open_drop_for_box_contents`,
  `compiler/rustc_mir_transform/src/elaborate_drop.rs`), so no function is made per boxed type.
- **Swift** states a class's member destruction, its superclass's deinit and its deallocation in SIL
  (`emitIVarDestroyer`, `emitDestroyingDestructor`, `emitDeallocatingClassDestructor`,
  `lib/SILGen/SILGenDestructor.cpp`). A struct's per-field copy and destroy is in IRGen, because a
  type may be resilient and its fields unknown to the module compiling against it.
- **clang** defines a class's implicit special members in Sema and emits member destruction in
  CodeGen (`Sema::DefineImplicitCopyConstructor`, `CodeGenFunction::EnterDtorCleanups`), having no
  layer between its AST and LLVM IR.

Where Lyra differs: every type a unit names is declared there with its parts, so the condition that
keeps Swift's value steps in IRGen does not hold, and Lyra has the layer between that clang lacks.
rustc's answer is taken.

## Rejected

- **The bodies in MIR.** MIR states no copy or end, and the C++ backend gets each from the host
  language, so a MIR body would be a second statement of what the target already supplies, which a
  render either ignores or writes out
  ([../architecture/backend_contract.md](../architecture/backend_contract.md) invariant 9).
- **The bodies in the code generator**, which is what this replaces: a backend deciding what an
  operation does, and the one answer to whether an end does anything asked again at each site.
- **Each call site naming the function it calls.** A product built, a union built, a value put into
  storage and a value written over each carry a step of their parts implicitly, as rustc's `Drop`
  names drop glue implicitly, so the step stays a statement of LIR and the query resolves it.
- **The adapters a type's table holds, given bodies in LIR.** Each drops the type the library hands
  first or converts an integral value to the scalar or stream the library's interface takes; both
  are how a call crosses the runtime's C interface, which is below LIR. That LIR should declare the
  table and each adapter -- its interface and its target -- with the code generator supplying the
  body, holds by the rule that a target writes no declaration the IR does not state; it covers the
  whole table rather than the steps alone, so it is a subject of its own.
- **Letting a value be made wherever a parameter points**, so that a tagged union is written over by
  ending it and then initializing its storage. Where a pointer came from says nothing of whether
  what it points at holds a value, so that would stop checking every write made through one; the end
  and the rebuild are instead one statement, which no other step can come between.

## Consequences

- LIR holds a function per laid-out type and step, per closure, and four per class, each named from
  the type or the class; an owning pointer's end has none, so a value boxed only past a size adds no
  function. Every laid-out type the unit names gets its functions, whether or not a table or a step
  of it is then emitted, where the code generator built one only when something asked for it: a
  declaration brings its operations ([../design-process.md](../design-process.md)), and what an
  unused one costs is its compile. LIR's verification admits a value built part by part in a body's
  return place -- the shape a construction takes, not a proof that each part is made once -- and LIR
  states a value ended and built anew where it lies as one statement.
- A value laid out by its type that holds an owning pointer has no copy, no write over it and no
  move, only an end: a move leaves what it took for its holder to end, which would give back what
  the pointer moved into the new value owns. No lowering makes one -- a value too large for a frame
  is boxed whole, never as a part -- so a step asked of one stops as a compiler error rather than
  freeing twice. A tagged union's move copies its live member.
- A tuple's default, which no IR states, is still composed by the code generator.

## Cross-references

- [../architecture/lir.md](../architecture/lir.md) -- what LIR owns of a value's steps.
- [../architecture/mir.md](../architecture/mir.md) -- what MIR states of ownership.
