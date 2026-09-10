# A dispatch position is a coordinate in a lineage, not a number in a table

Date: 2026-09-09 Status: accepted

## Context

A call may name a behavior instead of a body, and which body runs is then decided by the value the
call is made on (LRM 8.20). The semantic layer already states this: a method introduces, takes over,
or finalizes a slot, and a dynamic call names a receiver together with the slot's canonical
identity, which is the declaration that introduced it. What no layer stated is where that identity
lands in something a machine can index.

The C++ backend never had to answer it. It renders the `virtual` keyword and lets the host compiler
match names and signatures, so the stated relation was written and never read. The execution backend
is the first consumer that has to turn the identity into a coordinate.

## Decision

**D1. A class states what it adds to its lineage and nothing about the lineage itself.** A class
carries the behaviors it introduces, in the order it introduces them, and the ones it takes over,
each naming the behavior taken over. It restates nothing its bases already state -- exactly as it
carries its own members and not its base's. What a value holds and what a value answers are both
read from the lineage, which is what keeps one declaration's meaning independent of what extends it.

**D2. A behavior is named by the declaration that introduced it and an ordinal within that
declaration.** The ordinal counts introductions inside one class, so naming a behavior reads that
class and nothing else. Flattening a lineage into positions is a layout question, answered where the
whole lineage is in hand rather than where a call is written -- which is what lets a unit name a
behavior whose position it could not have counted, its base having been declared elsewhere.

**D3. Allocation of the answer is the runtime's; entering the body is the asking code's.** The
runtime answers with the address of the body a value's class holds at a position; the generated code
enters that address with the arguments it already has. What class a value is, is the only half the
generated side cannot know, so it is the only half that crosses -- which keeps the object's layout
and the class record's layout out of generated code entirely. This is the construction split (see
[entering-a-class-construction](entering-a-class-construction.md)) read on the code axis.

## Why the coordinate is two parts

An absolute position -- counted from the whole lineage and carried as one number -- is the shape a
reader reaches for first, and it is wrong here for a reason that is checkable rather than aesthetic.
Assigning it requires walking a class's bases, which requires a memo so several classes extending
one base do not settle it twice, and an absent state threaded through every signature for the
lineage that leaves the unit. The two-part coordinate needs none of those: it fits, unchanged,
inside the step whose stated contract is that it reads one declaration and waits on nothing.

It also matches how the storage axis already works, which is the stronger argument: a member is
named by its declaration and its position within it, and flattened where it is used. One rule
expressed two ways is what makes a codebase hard to read, and the two axes are one rule.

## Where other systems put this

Every system with dynamic binding and a fixed hierarchy assigns positions at compile time, and every
system whose hierarchy can change between compile and load assigns them later, from a name. Clang
computes vtable indices in a layout query beside its record-layout query, consulted at code
generation; rustc computes them in a query consumed by codegen; a Java compiler computes none at all
and emits a symbolic reference its runtime resolves when the class links, as does a CLR compiler.
Objective-C never builds an index: a class holds a table from selector to implementation, because a
class can gain methods while the program runs.

SystemVerilog has no dynamic class loading and states its cross-unit dependencies explicitly, so the
hierarchy is fixed when a unit compiles and the compile-time answer is available. What the survey
settles beyond that is placement: in every system that computes positions at all, they come from a
layout component asked at code generation, never from a field carried in the mid-level IR.

## Rejected alternatives

- **An absolute position assigned while lowering.** Needs a base walk, a memo, and an absent state
  the neighboring code does not have, and states in the IR what a layout query answers. The
  machinery it requires is the evidence against it.
- **A record listing every body a class declares, indexed by a call.** Cannot be indexed: it lacks
  the behaviors an ancestor introduced and the class does not take over, and it holds bodies that
  answer no behavior at all.
- **Generated code reading the class record itself.** Makes the record's layout a fact every code
  generator must reproduce, which is the object-ABI boundary
  [generated-behavior-boundary](generated-behavior-boundary.md) rejects.
- **One table collapsing a scope's lifecycle entries with a class's behaviors.** They share a
  representation and are separate concepts; the closed set the engine calls at fixed phases does not
  evolve with the open set a program writes.
- **Naming a behavior by the source name of the method that introduced it.** A name is what a
  by-name boundary uses across units; inside one unit it is a second identity for something the
  registry already identifies.

## Consequences

- A behavior another unit introduced, and a call on a value whose class extends another unit's, wait
  on that unit promising the class -- not on its promising a position. A position is never carried
  across the boundary: the promise states an order, both sides count it, and where a class's own
  behaviors land is settled with the whole lineage in hand.
- Building a consumer that reads the stated relation found that relation being stated wrongly: a
  method declared as an `extern` prototype and defined out of block (LRM 8.24) carries its override
  link on the prototype, and reading only the definition recorded every such override as introducing
  a behavior of its own. No backend had read the relation before, so nothing had contradicted it.
- Interface-class dispatch stays out, and is refused by name rather than left to fall through. A
  class conforming to several interfaces answers behaviors that several unrelated declarations
  introduce, and two classes conforming to one interface need not order them alike; that is a second
  coordinate system, and [interface-conformance-realization](interface-conformance-realization.md)
  defers the representation it needs. Staying out is not the same as being absent: a call through an
  interface handle does name a behavior, and given a lineage coordinate for it, that coordinate
  lands on whatever the object's own lineage put at that position.
- That refusal belongs where the coordinate is required and nowhere earlier. A backend that recovers
  a method name and lets the target language dispatch needs no position at all and answers such a
  call correctly, so stating the limit in a lowering both backends share does not withhold an answer
  -- it withdraws one that already worked. The question is asked once, of a class this unit declares
  and a class it read a promise about alike, at the step that turns a behavior into a position.
