# LIR

## Purpose

LIR is a machine-execution model IR. Its conceptual peers are LLVM IR and MLIR -- compiler IRs that
express a program as a control-flow graph over typed values, with explicit storage and explicit
effect ordering, shaped for code generation onto a concrete target. LIR is not a high-level
programming language; it is what a program looks like once structured control flow and the
object-oriented vocabulary have been lowered into something a register allocator and a code
generator can consume.

LIR's vocabulary is what those execution-model IRs share:

- Basic blocks with explicit successor lists, forming control-flow graphs per callable.
- Typed values, each either a transient computed value or a place -- a named memory location reached
  as a base local plus a projection chain. Which one a value is, is explicit.
- Low-level operations: arithmetic, comparisons, loads, stores, calls.
- Effect ordering within a block.
- An explicit protocol to the scheduling runtime: entry, suspend, resume, completion appear as CFG
  edges and call instructions, not as conventions implied by node order.

MIR is the source language LIR is lowered from. The semantic vocabulary of MIR -- structured `if` /
loop, expression-level operators, member access through a receiver, callables with parameter lists,
classes -- is translated at MIR-to-LIR into LIR's machine-execution vocabulary: structured control
flow becomes a CFG, expression trees become instruction streams, member access becomes a load or
store against a resolved storage location, callable invocation becomes a call instruction. Once LIR
sees a value it sees a typed result; it does not know or care that the same value was a structured
expression in MIR.

LIR carries no source-language concept. SystemVerilog specificity is paid in full at HIR-to-MIR: by
the time the program is MIR it is already a generic programming language, with every SV-specific
construct -- an out-of-bounds guard, an event control, a non-blocking assignment, a generate region
-- already expressed as generic control flow, generic expressions, or runtime-library calls.
MIR-to-LIR therefore introduces execution-model facts -- a control-flow graph, storage placement,
scheduling edges -- without interpreting any source-language semantics. The lowering is structural,
not semantic, and gets simpler the lower it goes precisely because the source-language specificity
is already spent upstream.

LIR is also not the target machine. Register allocation, instruction selection, and per-target
calling conventions belong below LIR (in the LLVM backend's own pipeline). Its place-oriented form,
where addressable state is a place and transient computation is a value, is closer to Rust's MIR
than to LLVM's fully SSA form; the final move to SSA happens at LIR-to-LLVM. LIR is the model the
compiler-side optimizer reasons over; the target-side lowering consumes that model.

## Owns

- Control-flow graphs: basic blocks, successors, edges.
- The place vocabulary: a place is a base local plus a projection chain (field, index, dereference,
  slice, downcast). Every load, store, and address-of names a place.
- The transient-value vocabulary: a computed value with a pure dataflow origin, not backed by a
  named memory location.
- Storage placement: offsets, layouts, and concrete addresses of state -- including the storage
  layout of an object's members, which MIR states only as a name and a type.
- Effect ordering within a block.
- Low-level operations: arithmetic, comparisons, loads, stores, calls.
- The boundary protocol to the scheduling runtime (entry, suspend, resume, completion).

## Does Not Own

- Semantic ownership of objects, members, or callables (see `mir.md`).
- Language-level constructs (loops as loops, assertions as assertions).
- The generate-region tree or hierarchy structure (see `hierarchy_and_generate.md`).
- Identity rules for upstream references (see `identity_and_ownership.md`).
- Target-machine details: register allocation, instruction selection, per-target calling
  conventions. These belong below LIR.

## Core Invariants

Each invariant below is a direct consequence of the identity stated in Purpose -- LIR is a
machine-execution model IR shaped for codegen. An invariant that cannot be re-derived from that
identity is the suspect, not the analysis.

1. LIR is a CFG-shaped IR. Every callable body is a graph of basic blocks with explicit successors.
   _Machine-execution consequence: control flow is the graph the codegen consumes; structured
   constructs do not survive at this layer._
2. Each LIR value is either a place or a transient value, and which one is explicit. A value is a
   place -- a named memory location -- exactly when it needs one: its address is taken, or it names
   storage written through more than once. A value computed once and consumed is a transient value
   with a pure dataflow origin. _Machine-execution consequence: the codegen knows for every value
   whether it lives in memory or in a register-class temporary; no value's storage class must be
   inferred._
3. Storage placement is decided at LIR. Upstream layers do not carry offsets or addresses; an
   object's member layout is assigned here from each member's type. _Machine-execution consequence:
   a higher-level IR's job ends at "this is a member of this object"; the address is LIR's._
4. LIR does not reintroduce high-level semantic ownership. It consumes ownership decided by MIR.
   _Machine-execution consequence: lowering is one-way; LIR reads upstream decisions and acts on
   them, never re-derives them._
5. The runtime protocol is explicit: suspend and resume points appear as CFG edges, not as lowering
   conventions implied by node order. _Machine-execution consequence: the scheduler's boundary is
   visible to the optimizer as edges, not as a side convention only the lowering knows._
6. LIR carries no source-language semantics. Every node is a generic machine-execution operation; no
   SV-specific shape -- an out-of-bounds guard, an index-validity predicate, an NBA region, an event
   control -- survives, because each was lowered to generic control flow or a runtime call at
   HIR-to-MIR. _Machine-execution consequence: a source-language concept reaching LIR is an upstream
   leak, never a LIR node to model._

## Boundary to Adjacent Layers

- Consumes MIR. MIR-to-LIR is the layer where the high-level programming language gets translated
  into a machine-execution model: structured control flow (`if`, loop, break, continue, return) is
  flattened into basic blocks and terminators, member access becomes load/store against a resolved
  storage location, expression trees become instruction streams, callable invocation becomes a call
  instruction, and suspension (an await, a sensitivity wait) becomes suspend/resume edges with
  scheduler calls. MIR-to-LIR introduces every fact LIR owns and that MIR does not (CFG structure,
  storage placement, effect ordering, scheduling-protocol edges), and it introduces no
  source-language interpretation -- a lowering rule that must decide what a MIR node means is
  reading a defect in MIR, not doing LIR's job.
- Produces input to LLVM IR. LIR-to-LLVM is mechanical translation rather than design; it is also
  where the place / transient-value form becomes fully SSA.

## Forbidden Shapes

These are the patterns that violate the identity. Each is the inverse of a property a machine-
execution model IR implies; the diagnostic for any new forbidden shape is "what identity property
does this break".

- A parallel identity or ownership system that duplicates MIR's. (Identity is owned by the layer
  that introduced it; LIR consumes, never re-mints.)
- Object or member semantics carried into LIR nodes. Object model belongs at MIR. (LIR's vocabulary
  is machine-execution, not high-level programming language.)
- Hierarchy navigation logic expressed at LIR. Hierarchy is resolved by MIR and consumed as storage
  addresses at LIR.
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
- Storage decisions decided upstream and encoded in LIR as passthrough. (Storage is LIR's; upstream
  carries no offsets to pass through.)
- A scheduling decision made outside LIR but implied by LIR shape. (The scheduling-protocol boundary
  is explicit; conventions hidden in node-order arrangements violate the protocol.)
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

A closure lowers to a code reference plus an environment value; invocation is an indirect call
passing the environment and the call arguments. `self` is the first argument, supplied like any
other; LIR has no implicit receiver. A direct call to a statically known callable passes the
environment and arguments to a named function. LIR knows only "call target" and "arguments"; the
capture policy that built the environment was MIR's.

A member access lowers by what its type already states. A plain-value member arrives as a member
projection and lowers to a load or store against the placed member. An observable signal arrives
already as a runtime-library call -- the cell's get / set / mutate, because HIR-to-MIR expressed the
access that way -- and lowers as a call, the same as any other. LIR carries no wrapper concept
either way; it carries the loads, stores, and runtime calls the MIR already named.
