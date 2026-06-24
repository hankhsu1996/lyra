# MIR

## Purpose

MIR is a generic software programming language IR. Its conceptual peers are C++, Rust, and Python --
high-level programming languages with rich type systems, first-class functions, structured control
flow, and object-oriented features. MIR is not a hardware description language IR and not a
machine-execution model; it sits in the middle of the pipeline, expressing the program as software
in a generic programming-language vocabulary.

MIR's vocabulary is what those languages share:

- Expressions and statements.
- Function and method calls, including coroutines and closures.
- A type system covering value types, object types, and composing wrappers (owning pointer, vector).
- Object-oriented features: classes with members, member access through an explicit receiver
  expression.
- Structured control flow: `if`, loop, sequence, return.

SystemVerilog is one source language that flows in through HIR; SV does not shape MIR's vocabulary.
SV-specific concepts -- signals as observable storage cells, NBA scheduling regions, event-control
semantics, generate as elaboration -- are translated at HIR-to-MIR into MIR's generic vocabulary:
variables with library wrapper types, method calls against those wrappers' APIs, runtime callables
the scheduler invokes, constructor-time logic that builds the object graph. Once MIR sees a value it
sees a programming-language expression with a programming-language type; it does not know or care
that the source was SystemVerilog. The same MIR could in principle express the same program reached
from any source language with comparable semantic surface.

MIR is also not the machine-execution model. Control-flow graphs, basic blocks, storage placement,
register allocation, and instruction-level effect ordering belong to LIR. MIR is high-level
structured code; the program is still readable as software at this layer.

The C++ backend renders MIR to C++ source, and the LIR-then-LLVM backend lowers MIR to a machine
model. Both consume the same MIR. Because MIR is a generic programming language, "the same MIR"
means the same program; the two backends differ only in how they represent each MIR construct (a
method call rendered as C++ method-call syntax versus lowered to an LLVM call instruction), never in
what the construct means.

## Owns

- Objects, members, and member access as first-class entities.
- A member as a `(name, type)` pair. The type is the sole carrier of the member's storage shape and,
  for a member that owns a child, of its child-scope kind and cardinality.
- The type system: value types (integral, real, string, event, ...); object types in two forms -- an
  intra-unit object (a class of this unit) and an external-unit object (another compilation unit,
  named); two composing wrappers, owning pointer and vector; and the tuple, a heterogeneous product
  of component types (the generic-language product type, distinct from the homogeneous vector).
- Callables: one concept -- callable code (a signature plus an internal body or an external symbol)
  and a callable value (code plus a bound environment). Every callable body's first binding is
  `self`, a pointer to the enclosing class; the body reaches every value it needs through its
  parameters and bound environment, never through implicit context. `callable.md` is the canonical
  contract.
- The identity of each object and member within a compilation unit.
- Lightweight structured control flow inside a callable body: `if`, loop, and sequence. No basic
  blocks at this layer.
- A primitive expression set: literals, references, unary / binary / conditional operators, calls,
  conversions, closures, member access through an explicit receiver expression, access primitives
  for element and range selection, and value-build primitives for aggregate construction. The set is
  closed under what a generic programming-language AST needs to express; it does not grow to model a
  particular backend's storage realization or runtime library shape.
- Action shapes for constructs that bind behavior to schedule events (always blocks, continuous
  assignments, deferred assertions, concurrent assertions).
- A textual dumper that serializes MIR for inspection, validation, and golden testing. The dumper is
  not a backend; its output is not executable.

## Does Not Own

- Control-flow graphs, basic blocks, branches, or phi nodes.
- Storage placement, offsets, or memory layout.
- Scheduling, dirty tracking, or wakeup filtering.
- The frontend's symbol or string identity. MIR carries only MIR's own ids.
- SystemVerilog source-level syntactic sugar. Sugar collapses to MIR's primitives at HIR-to-MIR.

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose. The body of an
invariant should be derivable from the identity; an invariant that cannot be re-derived is the
suspect, not the analysis (`lowering_organization.md` states this discipline in general).

1. Every compiled entity is exactly one of: an object, a member of an object, a callable, or an
   action bound to a callable. _Programming-language consequence: the entities a program defines are
   types, members, functions, and registered effects -- nothing else._
2. All hierarchy is expressed as object ownership. A hierarchical reference is navigation on the
   object graph, not a lookup in a side table. _Programming-language consequence: containment is
   expressed in the type system, not by a parallel registry._
3. Generate is lowered to constructor-time construction logic. A module's constructor builds its
   children via `if`, loop, and sequence over construction actions. _Programming-language
   consequence: generate is not a meta-language at MIR; it is ordinary constructor code._
4. MIR ids are the single source of identity at this layer. Earlier-layer ids are not carried
   through. _Programming-language consequence: a layer's names are owned by the layer._
5. MIR admits a pure textual dumper that is lossless with respect to MIR identity and structure. The
   dumper is the golden-test surface at this layer; it is not a compilation path. _Programming-
   language consequence: the program is readable as text at the layer where it is meaningful as
   software._
6. MIR does not reconstruct topology or identity from side tables. A member is owned by exactly one
   object; a callable by exactly one owner. _Programming-language consequence: ownership is
   structural, not name-based._
7. A member is a `(name, type)` pair. An owned child -- a module instance or a named generate scope
   -- is a member whose type is an owning pointer to an object type. An array of children is a
   member whose type is a vector of that owning pointer; a multidimensional array nests the vector
   wrapper. No member carries a classification flag beside its type; the type is the classification.
   _Programming-language consequence: the type system carries every fact about a member; no parallel
   discriminator exists._
8. An object type is exactly one of two forms: intra-unit, naming a class of this unit, or
   external-unit, naming another compilation unit. This single distinction is the owned child's
   runtime scope kind -- a named generate scope versus a module instance. Nothing else encodes scope
   kind. _Programming-language consequence: a class reference is either local to this translation
   unit or names an external one; there is no third kind._
9. Instance multiplicity is the vector wrapper, a property of the member's type, and is orthogonal
   to whether the owned object is an intra-unit scope or an external unit. The same wrapper composes
   over either object form and to any depth. _Programming-language consequence: cardinality and kind
   compose through generic type wrappers._
10. Every semantic decision is explicit in MIR's structure, expressed in MIR's existing primitive
    set. MIR is consumed by more than one backend; each chooses only how to represent MIR's stated
    semantics in its target, by a fixed function of the node kind. A backend reads a semantic fact
    from a MIR node or reference; it never re-derives or re-decides one, and it never expects MIR to
    grow a node kind that carries a backend storage realization, a runtime library's shape, or
    source-language sugar -- a fact of that kind is already expressible by combining existing
    primitives, so if it is missing, HIR-to-MIR is incomplete and should emit that combination
    instead. The primitive set does grow, but only for a genuinely new generic-language concept,
    never to model a backend / library / sugar shape (see Owns). _Programming-language consequence:
    the program text is the program; consumers do not invent vocabulary the language does not have._
11. Every callable body's first binding is `self`, a pointer to its enclosing class --
    `body.vars[0]` is a local declaration of that pointer type, named `self`. Access to any class
    member -- a member variable, a service call, a child instance -- flows through `MemberAccess`
    whose receiver expression reaches the class by traversing from `self`. A callable has no
    implicit access to its enclosing state; the receiver is reached through an explicit binding,
    never through an expression that means "look around and figure it out". `self` is uniform across
    every callable: it is the code's first parameter, read as `vars[0]` the same way in every body,
    and a callable value binds it like any other environment field. `callable.md` is the canonical
    callable contract.

    _Programming-language consequence: methods take `self` explicitly. This is the C++ `this`, the
    Python `self`, the Rust `&self`. Languages that hide it behind keyword sugar at source level
    still expose it explicitly in their IRs (LLVM IR's first parameter, Python's
    `__call__(self, ...)`); MIR does the same._

## Boundary to Adjacent Layers

- Consumes HIR. HIR-to-MIR is the layer where SV-specific concepts get translated into MIR's generic
  programming-language vocabulary. Anything SV-specific that does not appear in MIR's vocabulary is
  either translated at this boundary (an event-control delay becomes a coroutine suspension; a
  non-blocking assignment becomes a deferred closure submission) or rejected as unsupported.
  HIR-to-MIR has the SV knowledge; MIR does not.
- Produces input to LIR. MIR-to-LIR is the layer where the programming language gets translated into
  a machine-execution model. CFG structure, storage placement, and execution-oriented details are
  introduced at this boundary; MIR does not predict them.

## Forbidden Shapes

These are the patterns that violate the identity. Each is the inverse of a property the identity
implies; the diagnostic for any new forbidden shape is "what identity property does this break".

- Basic blocks, successor lists, or phi nodes in MIR nodes. (MIR is not the machine-execution
  model.)
- Storage offsets, byte layouts, or alignment data in MIR nodes. (Storage placement belongs to LIR.)
- A secondary hierarchy or identity system that shadows MIR ids (coordinate tuples, ordinals, symbol
  paths used as identity). (Identity is owned by the layer; programming languages do not have two
  parallel name systems for the same thing.)
- Global semantic lookup used inside MIR. Relationships live on the owning object. (Structural
  ownership replaces side-table lookup.)
- A parallel list, flag, or side field that classifies members -- signal versus owned child,
  instance versus generate scope, scalar versus array -- outside the type system. The type is the
  classification, and a consumer reads it from the type. (Type system carries every fact about a
  member.)
- Re-deriving a member's scope kind, cardinality, or storage shape from anything other than its type
  (its name, a naming convention, an enclosing-construct category, or a separate enum).
- Side tables that recover topology from keys rather than direct ownership.
- Deferred or concurrent assertions represented as flags or lowering-pass side effects instead of
  explicit callables. (Effects are first-class entities; they are not represented by flags.)
- A MIR node that preserves SystemVerilog syntactic sugar as an opaque payload: a `CaseStmt` /
  `CaseInsideStmt` in MIR, an `InsideExpr` / `CasezExpr` / `CasexExpr` in MIR's expression set, a
  `ForeachStmt` in MIR. Sugar of this kind is desugared to primitives upstream. (MIR's vocabulary is
  the generic-language vocabulary, not the source language's.)
- A MIR node kind invented to express a backend-side storage realization or runtime library wrapper
  (a `ReadCell` / `WriteCell` / `MutateCell` node for the C++ backend's `Var<T>`, an `AcquireLock` /
  `ReleaseLock` node for some scheduling discipline). Backend storage realizations are library types
  in the target; their operations appear in MIR as ordinary `CallExpr` nodes against the library
  type's API. The decision that a read or write through such a wrapper is expressed as a method
  invocation is made at HIR-to-MIR lowering, which emits a `CallExpr` against the wrapper type's
  API; the resulting MIR carries no trace of the wrapper as a node concept. (MIR's primitive set is
  closed; the existing `CallExpr` carries every backend-side library call the same way it carries
  every other call.)
- A runtime helper invocation that wraps a primitive operator family as an opaque call to recover
  source-level shape (e.g., `Inside(lhs, items)`, `CaseMatch(sel, labels)`). Sugar collapses to
  primitives in MIR; readability of generated backend source is not recovered by reintroducing
  intrinsic-style calls downstream.
- A backend that recovers a semantic fact MIR does not state by inferring it from a node's body
  contents or any side signal, instead of reading it from an explicit node or reference. Reading
  which node consumes a value is reading structure and is allowed; scanning a body to decide what it
  must be is re-deriving. If two backends could infer a fact differently, it is not yet in MIR and
  belongs there -- expressed through the existing primitive set.
- A node field that no backend's realization reads, or that restates what the node's structural
  context -- the nodes it references, or the node that consumes it -- already fixes. A node's fields
  are the inputs to its fixed per-backend realization: a field a realization can ignore is dead, and
  a field that duplicates what the surrounding structure already determines is redundant. Both are
  the same defect from two sides -- the encoded fact is either absent from the structure (and
  belongs there as structure) or already present (and the field is noise). The falsifiable check on
  a new field is: does every backend's realization read it, and does it state something the
  structure does not?
- An expression node whose meaning depends on which callable encloses it. A node that resolves to
  "the current receiver, whatever that is" carries an implicit context the consumer must walk
  surroundings to discover. Receiver semantics live on `body.vars[0]` as the `self` binding, reached
  through `LocalRef`; member access reads the receiver from the containing `MemberAccess`'s receiver
  field, never from a node that means "look around and figure it out". (Programming languages do not
  have expressions whose meaning depends on syntactic context.)

## Notes / Examples

Reviewing MIR via the dumper should feel like reading the compiled program's structure expressed in
a generic programming language. The C++ backend emits MIR to C++ source: it is a backend output, not
a debug view. The C++ surface syntax happens to map naturally onto MIR's vocabulary because the two
share many concepts, but C++ is not MIR's semantics and more than one backend may exist.

A module instance, a named generate scope, and an instance array differ only in one member's type:
`owning-ptr(external-unit object)`, `owning-ptr(intra-unit object)`, and `vector(owning-ptr(...))`
respectively. A two-dimensional array is `vector(vector(owning-ptr(...)))`. Because the
classification lives entirely in the type, a consumer that recurses on the type -- a vector layer
recurses on its element, an owning pointer to an object reaches the object -- handles every
combination, including arrays of any dimensionality, with no per-form branch. A consumer that
switches on whether a member "is an instance" or "is a generate scope" or "is an array" is
reconstructing what the type already states.

The primitive expression set above is shaped by the downstream optimizer. Each backend
(`backend::cpp` today, the LLVM backend via LIR) consumes MIR as primitives that its optimizer can
fold, propagate, and pattern-match across. A sugar node carried into MIR forces every backend to
either re-implement the desugaring or emit a runtime helper call; in the LLVM path the helper
appears as an opaque call through which constant folding, dead-arm elimination, and jump-table
synthesis cannot reach, and which LTO mitigates but does not eliminate. Backend-local readability of
generated artifacts is a debug concern and is recovered through value-type C++ operator overloads in
the runtime, not through sugar nodes in MIR.

Constructs in the expression set that look like sugar are not. The rvalue conditional form (`?:`) is
preserved because MIR's `if` is a statement; there is no primitive rvalue branching in MIR's
expression set that decomposes the ternary. Value-build primitives for aggregate construction
(concatenation, replication, structured literal, and similar) have no smaller decomposition. Select
expressions are access primitives. Each of these stays in MIR for the same reason: removing it would
require expanding into a statement-form rewrite that does not fit the expression context.

A callable is one concept: callable code (a signature plus an internal body or an external symbol)
and a callable value (code plus a bound environment). A closure is a callable value with a captured
environment, synthesized only by HIR-to-MIR. `self` is the code's receiver parameter, the result
type is the call protocol, and a bound field is a snapshot or an alias by its type. `callable.md` is
the canonical contract.

An access through a runtime library wrapper type illustrates the boundary between MIR's vocabulary
and a backend's storage realization. The C++ backend wraps an observable signal's storage in
`Var<T>`, exposing `Get` / `Set` / `Mutate` on the wrapper. MIR does not have a "cell access" node:
a read of an observable signal is a `CallExpr` whose callee is the wrapper type's `Get` method and
whose receiver is the `MemberAccess` reaching the signal; a write is a `CallExpr` to `Set`. The
decision that an observable read is a method call -- not an implicit value-read -- is made at
HIR-to-MIR, which sees that the signal's MIR type is the wrapper type and emits the corresponding
call. By the time MIR is read by any backend, the program already says `wrapper.Get(...)` or
`wrapper.Set(...)`; the backend reads the call and emits it the same way it emits any other call.
