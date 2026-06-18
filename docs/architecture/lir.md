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
- Typed values whose storage source -- a register, a stack slot, a memory location -- is explicit.
- Low-level operations: arithmetic, comparisons, loads, stores, calls.
- Effect ordering within a block.
- An explicit protocol to the scheduling runtime: entry, suspend, resume, completion appear as CFG
  edges and call instructions, not as conventions implied by node order.

MIR is the source language LIR is lowered from. The semantic vocabulary of MIR -- structured `if` /
loop, expression-level operators, member access through a receiver, callables with parameter lists,
classes -- is translated at MIR-to-LIR into LIR's machine-execution vocabulary: structured control
flow becomes a CFG, expression trees become instruction streams, member access becomes a load or
store against a resolved storage location, callable invocation becomes a call instruction. Once LIR
sees a value it sees a typed SSA-style result; it does not know or care that the same value was a
structured expression in MIR.

LIR is also not the target machine. Register allocation, instruction selection, and per-target
calling conventions belong below LIR (in the LLVM backend's own pipeline). LIR is the model the
compiler-side optimizer reasons over; the target-side lowering consumes that model.

## Owns

- Control-flow graphs: basic blocks, successors, edges.
- Storage placement: offsets, layouts, and concrete addresses of state.
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
2. Each LIR value has an explicit type and an explicit storage source or dataflow origin. _Machine-
   execution consequence: every value the codegen handles has a known representation; there are no
   "values reach this point implicitly" shapes._
3. Storage placement is decided at LIR. Upstream layers do not carry offsets or addresses. _Machine-
   execution consequence: a higher-level IR's job ends at "this is a member of this object"; the
   address is LIR's._
4. LIR does not reintroduce high-level semantic ownership. It consumes ownership decided by MIR.
   _Machine-execution consequence: lowering is one-way; LIR reads upstream decisions and acts on
   them, never re-derives them._
5. The runtime protocol is explicit: suspend and resume points appear as CFG edges, not as lowering
   conventions implied by node order. _Machine-execution consequence: the scheduler's boundary is
   visible to the optimizer as edges, not as a side convention only the lowering knows._

## Boundary to Adjacent Layers

- Consumes MIR. MIR-to-LIR is the layer where the high-level programming language gets translated
  into a machine-execution model: structured control flow becomes a CFG, member access becomes
  load/store against a resolved storage location, expression trees become instruction streams,
  callable invocation becomes a call instruction. MIR-to-LIR introduces every fact LIR owns and that
  MIR does not (CFG structure, storage placement, effect ordering, scheduling-protocol edges).
- Produces input to LLVM IR. LIR-to-LLVM is mechanical translation rather than design.

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

A MIR `MemberAccess` reaching a runtime library wrapper type (the C++ backend's `Var<T>`, an extern
upward-reference) is translated at MIR-to-LIR into a load or store against the resolved storage
location, with the wrapper type's invariants (subscribe-on-set, mutate-then-commit) realized as the
appropriate sequence of LIR instructions plus scheduling-runtime call edges. LIR does not carry the
wrapper concept; it carries the loads, stores, and runtime calls the wrapper expanded into.
