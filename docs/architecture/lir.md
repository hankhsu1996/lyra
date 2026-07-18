# LIR

## Purpose

LIR is a self-contained, target-neutral executable model IR. Its conceptual peers are LLVM IR and
Rust's MIR -- compiler IRs that express a program as a control-flow graph over typed values, with
explicit effect ordering and an explicit storage model, shaped so a backend can lower it to a
target. LIR is not a high-level programming language; it is what a program looks like once
structured control flow and the object-oriented vocabulary have been lowered into a control-flow
graph a code generator can consume.

LIR's vocabulary is what those execution-model IRs share:

- Basic blocks with explicit successor lists, forming control-flow graphs per callable.
- Typed values, each either a transient computed value or a place -- storage named as a base local
  plus a projection chain. Which one a value is, is explicit.
- A self-contained type graph: every value, place, and call is typed by a LIR-owned type identity.
- Low-level operations: arithmetic, comparisons, loads, stores, calls.
- Effect ordering within a block.
- An explicit protocol to the scheduling runtime: entry, suspend, resume, completion appear as CFG
  edges and call instructions, not as conventions implied by node order.

MIR is the source language LIR is lowered from. The semantic vocabulary of MIR -- structured `if` /
loop, expression-level operators, member access through a receiver, callables with parameter lists,
classes -- is translated at MIR-to-LIR into LIR's machine-execution vocabulary: structured control
flow becomes a CFG, expression trees become instruction streams, member access becomes a place
projection, callable invocation becomes a call instruction. Once LIR sees a value it sees a typed
result; it does not know or care that the same value was a structured expression in MIR.

LIR carries no source-language concept. SystemVerilog specificity is paid in full at HIR-to-MIR: by
the time the program is MIR it is already a generic programming language, with every SV-specific
construct -- an out-of-bounds guard, an event control, a non-blocking assignment, a generate region
-- already expressed as generic control flow, generic expressions, or runtime-library calls.
MIR-to-LIR therefore introduces execution-model facts -- a control-flow graph, logical storage
topology, scheduling edges -- without interpreting any source-language semantics.

LIR is target-neutral. It carries the logical shape of storage -- which local, which member, which
element a place names -- but not its physical realization. Byte sizes, alignments, field offsets,
object-header placement, and target calling conventions are not LIR facts; they are derived below
LIR from the LIR program together with the target data layout and the runtime ABI profile. Its
place-oriented form, where addressable state is a place and transient computation is a value, is
closer to Rust's MIR than to LLVM's fully SSA form; the move to SSA and to physical layout happens
below LIR, at LIR-to-LLVM.

## Owns

- Control-flow graphs: basic blocks, successors, edges.
- A self-contained type graph: LIR-owned type identities that type every local, value, place
  projection, return, and call argument. Each is translated from a generic-PL type at MIR-to-LIR and
  carries no live reference back to MIR.
- The place vocabulary: a place is a base local plus a projection chain (member, index, dereference,
  slice, downcast). Every load, store, and address-of names a place by logical identity.
- The transient-value vocabulary: a computed value with a pure dataflow origin, not backed by a
  named memory location.
- Logical storage topology: which local, member, element, or referent a place names, and the logical
  identity of every class member and callable a node refers to.
- Effect ordering within a block.
- Low-level operations: arithmetic, comparisons, machine conversions, loads, stores, calls.
- Foreign symbols: the linkage name of a callable defined outside the program, which a call may
  target. LIR states the name and the machine types the call crosses on; how the name is resolved --
  a link line, an execution session -- is below LIR.
- The boundary protocol to the scheduling runtime (entry, suspend, resume, completion).

## Does Not Own

- Semantic ownership of objects, members, or callables (see `mir.md`). LIR consumes the ownership,
  wrapper, and object-model decisions MIR fixed; it does not re-decide them.
- Physical layout: byte size, alignment, field offset, object-header placement, padding. These are
  derived below LIR from the logical topology, the target data layout, and the runtime ABI profile.
- Target ABI classification: by-value versus indirect parameter passing, `sret` returns, register
  and stack placement, calling conventions, and the LLVM-facing encoding of a value.
- A live dependency on MIR. After lowering, LIR consults nothing upstream; every type and identity
  it uses is its own.
- Language-level constructs (loops as loops, assertions as assertions).
- The generate-region tree or hierarchy structure (see `hierarchy_and_generate.md`).
- Identity rules for upstream references (see `identity_and_ownership.md`).
- Target-machine details: register allocation, instruction selection, per-target calling
  conventions. These belong below LIR.

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose -- LIR is a
self-contained, target-neutral executable model IR. An invariant that cannot be re-derived from that
identity is the suspect, not the analysis.

1. LIR is a CFG-shaped IR. Every callable body is a graph of basic blocks with explicit successors.
   _Machine-execution consequence: control flow is the graph the codegen consumes; structured
   constructs do not survive at this layer._
2. Each LIR value is either a place or a transient value, and which one is explicit. Which one a
   local is follows a canonical lowering rule, not the source language's notion of a variable: a
   local is a place -- named storage -- exactly when the canonical lowering needs an address for it
   (its address is taken, it is assigned after its initialization, or it holds a control-flow join).
   A value computed once and consumed is a transient value with a pure dataflow origin. The rule
   fixes the storage topology; it does not minimize it -- recovering a register where the address is
   never truly needed is a separate derivation below LIR. _Machine-execution consequence: the
   codegen knows for every value whether it lives in memory or in a register-class temporary; no
   value's storage class must be inferred._
3. LIR fixes logical storage topology, not physical layout. A place names storage by logical
   identity -- a base local and a projection chain of member, index, and dereference steps -- never
   as a byte offset, an address, or pointer arithmetic. The physical realization of that topology is
   derived below LIR. _Machine-execution consequence: one LIR program is valid for every target; the
   target-specific layout is a separate derivation, not a property baked into a place._
4. LIR identity is self-contained. Every type, class, member, and callable a LIR node names is a
   LIR-owned identity. Every MIR type entering LIR is fully translated to a LIR type or rejected at
   the MIR-to-LIR boundary; no LIR node carries a MIR id, index, or borrowed pointer as a live
   reference. _Machine-execution consequence: a finished LIR unit is dumpable, verifiable, and
   lowerable with no MIR present._
5. LIR consumes MIR's semantics and never re-derives them. It reads the ownership, wrapper, and
   object-model decisions MIR fixed -- whether a handle is shared, a reference is managed, a member
   is observable -- and acts on them. _Machine-execution consequence: lowering is one-way; LIR reads
   upstream decisions and acts on them, never re-decides them._
6. The runtime protocol is explicit: suspend and resume points appear as CFG edges, not as lowering
   conventions implied by node order. _Machine-execution consequence: the scheduler's boundary is
   visible to the optimizer as edges, not as a side convention only the lowering knows._
7. LIR carries no source-language semantics. Every node is a generic machine-execution operation; no
   SV-specific shape -- an out-of-bounds guard, an index-validity predicate, an NBA region, an event
   control -- survives, because each was lowered to generic control flow or a runtime call at
   HIR-to-MIR. _Machine-execution consequence: a source-language concept reaching LIR is an upstream
   leak, never a LIR node to model._

## Boundary to Adjacent Layers

- Consumes MIR. MIR-to-LIR is the layer where the generic programming language becomes a
  machine-execution model: structured control flow (`if`, loop, break, continue, return) is
  flattened into basic blocks and terminators, member access becomes a place projection, expression
  trees become instruction streams, callable invocation becomes a call instruction, and suspension
  (an await, a sensitivity wait) becomes suspend/resume edges with scheduler calls. Every MIR type
  and identity is translated into a LIR-owned one; a type MIR-to-LIR cannot translate is rejected at
  the boundary with a diagnostic, never passed through as a MIR id. MIR-to-LIR introduces every fact
  LIR owns and that MIR does not (CFG structure, logical storage topology, effect ordering,
  scheduling-protocol edges), and it introduces no source-language interpretation -- a lowering rule
  that must decide what a MIR node means is reading a defect in MIR, not doing LIR's job.
- Produces a self-contained LIR program for the layers below. Physical layout and target ABI are a
  separate derivation: the LIR program, the target data layout, and the runtime ABI profile together
  yield byte sizes, alignments, field offsets, and parameter-passing classification. LIR-to-LLVM
  consumes the LIR program and that derivation and is mechanical -- it encodes decided facts as LLVM
  types, address computations, loads, stores, and calls, and it is also where the place /
  transient-value form becomes fully SSA. The LLVM path never queries MIR; it is not a second
  semantic lowering.

## Forbidden Shapes

These are the patterns that violate the identity. Each is the inverse of a property a
self-contained, target-neutral execution-model IR implies; the diagnostic for any new forbidden
shape is "what identity property does this break".

- A parallel identity or ownership system that duplicates MIR's. (Identity is owned by the layer
  that introduced it; LIR consumes, never re-mints.)
- A MIR identity reaching LIR as a live reference: a `mir::TypeId`, a class / member / method index,
  or a borrowed `mir::CompilationUnit`, held on a node or consulted in a later pass. An
  un-translated MIR type passed through as a fallback instead of being rejected at the boundary is
  this shape.
- Byte offsets, addresses, alignment, padding, or LLVM-facing types on a LIR node or place. A place
  that names `base + N` instead of a logical projection is this shape; physical layout is derived
  below LIR, not encoded in the place.
- Object or member semantics carried into LIR nodes. Object model belongs at MIR. (LIR's vocabulary
  is machine-execution, not high-level programming language.)
- Hierarchy navigation logic expressed at LIR. Hierarchy is resolved by MIR and consumed as logical
  storage topology at LIR.
- Language-level constructs reintroduced at LIR (a "foreach" node instead of a loop CFG). (LIR is
  CFG-shaped; structured constructs do not survive lowering.)
- A source-language concept reintroduced as a LIR node: a guard, an index-validity predicate, an
  out-of-bounds default, an NBA region, an event control. These are lowered to generic control flow
  or runtime calls at HIR-to-MIR; one reappearing at LIR is an upstream leak, never a LIR node to
  add.
- Affine-ownership machinery -- move semantics, drop elaboration, borrow checking -- at LIR. LIR's
  value model is value-copy plus explicit borrowed references; it does not reconstruct a
  source-language ownership discipline.
- Implicit control flow. Every control transfer is an explicit edge. (Machine-execution IRs do not
  carry implicit transitions; the codegen relies on edges being explicit.)
- Reintroducing semantic structure that MIR has already lowered away.
- Target ABI policy decided during LLVM lowering. Parameter passing, `sret`, and register / stack
  classification are facts the physical-layout derivation fixes; the LLVM step encodes them, it does
  not choose them.
- The LLVM path querying MIR for a semantic type, a class member, an object layout, or an ABI
  decision. The LLVM path consumes the LIR program and the physical-layout derivation only.
- Basic blocks that cross callable boundaries. (Each callable is one CFG; cross-callable transfer is
  a call edge, not a basic-block-level transition.)

## Notes / Examples

A MIR process becomes an LIR callable with a CFG. Its suspend points (for example, event controls)
appear as explicit CFG edges that hand control back to the scheduler. The optimizer sees the suspend
point as an edge it can reason about (does it post-dominate any node, what values does it demand)
rather than as a convention buried inside a node's semantics.

A structured `if` with then and else blocks becomes a conditional branch to two basic blocks that
rejoin at a merge block; a loop becomes a header, a body, and a back edge; `break` and `continue`
become branches to the loop's exit and header. The structured nesting MIR carried is gone; the CFG
is the only control structure LIR has. This flattening is the core of MIR-to-LIR -- a backend that
targets a structured language (the C++ backend) never performs it, so it is work LIR introduces from
scratch.

A callable's function-local set is small: the receiver `self` and the compiler temporaries the
lowering introduces. A source-level variable with static lifetime is not a function local; it is a
member of the enclosing class, reached through `self` as a place. The bulk of a program's state is
member storage reached through receivers, not locals on a frame.

LIR is not an SSA form. A local is either a transient -- computed once, consumed, with a pure
dataflow origin -- or a place, which is storage on the frame. Which one a local is follows a
canonical lowering rule, not the source language's notion of a variable: a local is a place exactly
when the canonical lowering needs an address for it -- its address is taken, it is assigned after
its initialization, or it holds a control-flow join. Everything else is a transient. So there is no
phi and none is needed: a value produced on more than one path is a place each path writes, a
conditional expression writes its result into a place from each arm, and a loop-carried variable is
a place the body updates. A parameter arrives as a transient (the incoming argument) and becomes a
place only when the body assigns it or takes its address, in which case the entry block copies the
argument into that place. Recovering a register where the address is never truly needed is a
target-side derivation, not a property LIR must establish: LIR states the storage topology, it does
not minimize it.

A closure lowers to a code reference plus an environment value; invocation is an indirect call
passing the environment and the call arguments. `self` is the first argument, supplied like any
other; LIR has no implicit receiver. A direct call to a statically known callable passes the
environment and arguments to a named function. LIR knows only "call target" and "arguments"; the
capture policy that built the environment was MIR's.

A place is the logical path -- `self.counter`, `array[i]`, `*p` -- the way Rust's MIR names a field
projection rather than `base + N`. The same place is valid on every target; only the derivation
below LIR turns it into an address. A member access lowers to a place whose projection names the
member by its logical identity, not by an offset. An observable signal arrives already as a
runtime-library call -- the cell's get / set / mutate, because HIR-to-MIR expressed the access that
way -- and lowers as a call, the same as any other. Whether a plain-value member's place becomes a
load or a store, and at what address, is the physical layer's question, not LIR's.

An object member access -- reaching a member of an object through its receiver -- always yields a
place. What the use site does with that place -- read the value it holds, write a value into it, or
name where it lives -- is the use site's decision, never a property of the access. Some storage has
no first-class value in LIR at all: a storage cell, a scope, an object-tree node. Such a type is
address-only, and every operation over it consumes its address, so loading or storing a place of
that type is a lowering defect. Address-only is a fact about the storage object, not about how
values are represented: a packed value reached through an opaque handle is an ordinary first-class
value, and a place holding one is loaded and stored like any other. The cell that holds it is what
may only be addressed.

A place denotes independently addressable storage -- storage with an identity of its own -- not
merely something a source-level assignment can target. Assignability and place-ness are different
questions: in a value language, `s.b = x` on a struct is assignable but names no independent
storage, because the struct is a value and `s.b` is a part of it, not a location. So the place
vocabulary is for mutable, independently addressable storage (a local that needs an address, an
object member reached through its receiver, the referent of a dereference); a value aggregate is not
that. An unpacked struct -- the heterogeneous product value (see `mir.md`) -- reached through an
opaque handle is a first-class value, but its interior is not a place the way an object's members
are: value semantics forbid mutating a component in place, because the change would be visible
through every copy of the struct. So a component is reached by value projection, not by an
addressable sub-place. Reading a component extracts it from the product value; writing a component
produces a new product value equal to the old one with that component replaced, which is then stored
whole into the struct's own storage -- a component write is a whole-value store of a functional
update, uniform whether the struct lives in a local or in an observable cell. This is the
value-aggregate counterpart of the object member's place: an object member is mutable addressable
storage, so it is a place loaded and stored in place; a struct component is an immutable value part,
so it is a pair of value operations, an extract and an insert (the aggregate peers of LLVM's
`extractvalue` / `insertvalue`). The two look different because the value / reference distinction is
real, not because one is a special case of the other. The same principle governs the rest of the
value-aggregate family: a packed slice, a container element, and a union member are value
sub-accesses, so a sub-write is a functional whole-value update, never a store into an independently
addressable sub-place -- a static, structural selector (a struct component) may ride a dedicated
value instruction, a dynamic, runtime-indexed one (a container element) a runtime-library call, but
the principle that a value sub-write produces a new whole value is the same. A whole-value mutating
method on a value receiver -- a container's `delete`, a queue's `push` -- follows the same rule: it
is realized as a functional operation whose result is stored back through the receiver's owner, not
an in-place mutation of the value. How a target keeps value semantics for such a store is below LIR:
a target with language-level value copies may fulfill it by mutating a private copy in place, while
one whose container value is reached through a shared handle must produce a new value, so the model
LIR states -- a new whole value written back to the owner -- holds for both.

LIR carries the fact that a packed value is two-state or four-state; it does not carry how a
four-state value is stored. The canonical encoding of a four-state value -- value bits plus a state
mask -- is the runtime ABI profile's; turning that encoding into a concrete size, alignment, and
aggregate placement is the physical-layout derivation's. LIR states the state-ness; the layers below
realize it.
