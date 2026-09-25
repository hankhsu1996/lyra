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
- Typed values, each either a transient computed value or storage named by a place -- a base local
  plus a projection chain. Which one a local is, is explicit.
- A self-contained type graph: every value, place, and call is typed by a LIR-owned type identity.
- Low-level operations: arithmetic, comparisons, loads, stores, calls.
- Effect ordering within a block.
- An explicit protocol to the scheduling runtime: entry, suspend, resume, abandonment, and
  completion appear as CFG edges and call instructions, not as conventions implied by node order.

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
- The place vocabulary: a place is a base local plus a projection chain of member, dereference,
  element, and component steps, spelled inside the instruction that consumes it and never held.
  Every load, store, and address-of names a place by logical identity, and address-of is the one
  operation that turns the path into a value the program can retain. An element or component step
  reaches a part that is storage of its own -- an array's element, a structure's member; a part that
  is a view of its whole -- a packed value's bits, a string's character, a union's member -- is
  reached by value projection. There is no slice step, because a window is several elements rather
  than one storage, and storage behind an indirection is reached by a dereference step.
- The transient-value vocabulary: a computed value with a pure dataflow origin, not backed by a
  named memory location.
- The value-aggregate selector vocabulary: which subvalue of a view a step names -- the one member
  an active-member value holds at a time, a runtime coordinate, a fixed-width window. One extract
  and one update carry every step; the selector never says how the step is realized.
- Logical storage topology: which local, member, element, or referent a place names, and the logical
  identity of every class member and callable a node refers to. A member step reaches a class this
  unit compiles or a class another unit published, both member-bearing objects of this unit's own
  graph, and only what the artifact emits distinguishes them. Nothing of another unit's object is
  reached this way: what that unit promised of it is behaviors, and a behavior is a call.
- Effect ordering within a block.
- Low-level operations: arithmetic, comparisons, machine conversions, loads, stores, calls.
- Symbols a call reaches by name: a body of this program that another artifact holds, and a foreign
  symbol -- the linkage name of a callable defined outside the program. LIR states the name and the
  machine types the call crosses on; how the name is resolved -- a link line, an execution session
  -- is below LIR. The two are kept apart because a departure can come out of the first and never
  out of the second, which is a frame that ends only by returning.
- The boundary protocol to the scheduling runtime (entry, suspend, resume, abandonment, completion).

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
   A block ends in exactly one terminator, which is what makes it a basic block rather than a run of
   instructions; there is no terminator meaning "not yet decided", so a pass that builds blocks
   holds the undecided state in its own shape and never in LIR. _Machine-execution consequence:
   control flow is the graph the codegen consumes; structured constructs do not survive at this
   layer._
2. Each LIR local either names storage or is a transient value, and which one is explicit. Which one
   a local is follows a canonical lowering rule, not the source language's notion of a variable: a
   local is a place -- named storage -- exactly when the canonical lowering names it as storage (a
   declared local, a value a part is reached in, or a result that control-flow paths join at). A
   value computed once and consumed is a transient value with a pure dataflow origin. The rule fixes
   the storage topology; it does not minimize it -- recovering a register where the address is never
   truly needed is a separate derivation below LIR. _Machine-execution consequence: the codegen
   knows for every value whether it lives in memory or in a register-class temporary; no value's
   storage class must be inferred._
3. LIR fixes logical storage topology, not physical layout. A place names storage by logical
   identity -- a base local and a projection chain of steps naming a member, a referent, an element,
   or a component -- never as a byte offset, an address, or pointer arithmetic. The physical
   realization of that topology is derived below LIR. _Machine-execution consequence: one LIR
   program is valid for every target; the target-specific layout is a separate derivation, not a
   property baked into a place._
4. LIR identity is self-contained. Every type, class, member, and callable a LIR node names is a
   LIR-owned identity. Every MIR type entering LIR is fully translated to a LIR type or rejected at
   the MIR-to-LIR boundary; no LIR node carries a MIR id, index, or borrowed pointer as a live
   reference. _Machine-execution consequence: a finished LIR unit is dumpable, verifiable, and
   lowerable with no MIR present._
5. LIR consumes MIR's semantics and never re-derives them. It reads the ownership, wrapper, and
   object-model decisions MIR fixed -- whether a handle is shared, a reference is managed, a member
   is observable -- and acts on them. _Machine-execution consequence: lowering is one-way; LIR reads
   upstream decisions and acts on them, never re-decides them._
6. The runtime protocol is explicit: suspend, resume, abandonment, and each way a body is left
   appear as CFG edges, not as lowering conventions implied by node order. A suspension names both
   ways control can leave it -- the driver resumes the body, or ends it where it stands -- so what
   the second owes is in the graph rather than assumed; and a body left by a departure says so with
   a terminator of its own, as a returning one does. _Machine-execution consequence: the scheduler's
   boundary is visible to the optimizer as edges, not as a side convention only the lowering knows._
7. LIR carries no source-language semantics. Every node is a generic machine-execution operation; no
   SV-specific shape -- an out-of-bounds guard, an index-validity predicate, an NBA region, an event
   control -- survives, because each was lowered to generic control flow or a runtime call at
   HIR-to-MIR. _Machine-execution consequence: a source-language concept reaching LIR is an upstream
   leak, never a LIR node to model._
8. A place is an access path, not an entity. It is spelled inside the instruction that consumes it,
   is not an operand and not a local, and does not cross a control-flow edge; address-of is the one
   operation that turns a path into something the program retains, and its result is an ordinary
   value. _Machine-execution consequence: what a callee receives, what a local holds, and what
   survives a suspension is always a value, so no consumer re-evaluates a path whose base or
   projection may have changed since._

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
- A source language's ownership discipline reconstructed at LIR -- move semantics, borrow checking.
  LIR's value model is value-copy plus explicit borrowed references, and an affine discipline is a
  rule about what a program may write, decided where the program is still that language. (Deriving
  where storage is released is not this: it is a lowering computing a fact about the code in front
  of it, which is what an execution-model IR is for, and both peers named in Purpose do it.)
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
cell of whatever its declaration belongs to, which for a body of the design hierarchy is a member of
the enclosing class reached through `self` as a place. The bulk of a program's state is member
storage reached through receivers, not locals on a frame.

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

A place is the logical path -- `self.counter`, `*p`, `(*p).flag` -- the way Rust's MIR names a field
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

A place is the access path to independently addressable storage -- storage with an identity of its
own -- not merely something a source-level assignment can target. Path and identity are two phases,
and the split is what makes the vocabulary safe: the path is written where an instruction consumes
it and locates whatever its base and projections reach at that moment, while the identity a program
retains is the value address-of yields (`storage.md`). Assignability and place-ness are different
questions, and which parts of a value are storage of their own is the language's answer rather than
this layer's. An element of an array, queue or associative array, and a member of an unpacked
structure, each have an identity a second name may denote (`storage.md`), so each is a place: a step
into it extends the place holding its value, a load reads it where it lies, and a store writes into
the object already there. Value semantics hold because a copy of the aggregate copies its parts, so
no two values share one and a write through one is seen through no other. A packed value's bits, a
string's character and a union's member have no identity of their own; each is a view of its whole,
reached by value projection -- an extract, and an update producing the whole again, which is stored
where the whole lives (the aggregate peers of LLVM's `extractvalue` / `insertvalue`). One extract
and one update carry every such view, and the selector says only which subvalue is named -- the one
member an active-member value holds at a time, a runtime coordinate, a fixed-width window. A step's
direction is the access's: reading a missing element reads the default, and writing one allocates or
discards it by the container's own rule, which the library applies when the step is realized. Which
library entry realizes a step, and whether it is an instruction or a call at all, is a realization
question answered below LIR; it never decides which node the step is expressed as. A mutating method
on a receiver -- a container's `delete`, a queue's `push` -- changes the storage the receiver names
in place; where the receiver is a view, it is read out, changed, and written back as any write to a
view is.

LIR carries the fact that a packed value is two-state or four-state; it does not carry how a
four-state value is stored. The canonical encoding of a four-state value -- value bits plus a state
mask -- is the runtime ABI profile's; turning that encoding into a concrete size, alignment, and
aggregate placement is the physical-layout derivation's. LIR states the state-ness; the layers below
realize it.
