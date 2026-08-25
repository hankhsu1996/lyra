# Lowering Boundaries

## Purpose

Define what each lowering step may and may not do. Lowering is one-way, and each step has a narrow
set of allowed transformations.

A lowering exists so that a class of knowledge stops being needed downstream. AST-to-HIR retires
frontend identity; HIR-to-MIR retires SystemVerilog; MIR-to-LIR retires structured code; LIR-to-LLVM
retires the compiler's own vocabulary. Each step retires its class by **writing it down in the next
layer's vocabulary** -- everything the input layer knew that still matters is restated in the
target's own terms. Knowledge a step neither restates nor retires is knowledge every downstream
consumer must guess at, and each consumer guesses from whatever side data is nearest to hand. Those
guesses are tables, and a table has holes: the construct nobody thought of reaches a consumer that
cannot name it.

Restating is therefore local and total. Local: an input node's output is determined by that node and
its own subtree, never by a property of the tree around it. Total: every input node produces output,
including one that declares nothing. A step that must first walk its whole input to compute a
property, and only then knows what to emit, is not restating -- it is deciding, and the decision
belongs to whichever layer already held what the property was computed from.

## Owns

- The rule that lowering is one-way: a later stage does not write into an earlier stage.
- The rule that each lowering has a fixed input layer, a fixed output layer, and a fixed set of
  transformations it is permitted to perform.
- The dump and checkpoint contract: each lowering's output is inspectable in the output layer's
  canonical textual form (HIR dump, MIR dump, etc.). Dumps are debug surfaces, not compilation
  paths.

## Does Not Own

- The internal shape of any single IR layer.
- Implementation strategy within a single lowering.

## Core Invariants

1. AST-to-HIR is the cutover from frontend identity to compiler-owned identity. Frontend ids are
   discarded at this boundary and never reach HIR or beyond.
2. HIR-to-MIR converts HIR constructs into MIR objects, members, and callables. It does not
   introduce CFG, storage placement, or scheduling.
3. MIR-to-LIR introduces CFG, storage placement, and the scheduling protocol. It does not
   reintroduce language-level constructs or re-decide ownership.
4. LIR-to-LLVM is mechanical translation. Design decisions are not made at this boundary.
5. Information decided at an earlier layer is carried forward, not rebuilt. Downstream layers do not
   re-derive upstream decisions from side data.
6. Lowering does not introduce new semantic identity. The target layer's identity kinds are defined
   by the target layer's contract; a lowering may only use identity kinds that belong to the input
   or output layer.
7. The pipeline branches at MIR into two backends: the LIR-then-LLVM path (the architectural target)
   and the C++ emitter (a transitional realization that consumes the same MIR while LIR is being
   built). A backend emit makes no semantic decisions -- every semantic fact is fixed in MIR -- and
   chooses only how to represent each MIR node in its target, a representation fixed by the node's
   kind. The two backends represent the same node differently (a coroutine method versus an LLVM
   coroutine frame) yet realize the same behaviour, which holds only because neither adds, infers,
   or re-derives a semantic fact. **Both backends are mechanical at the same discipline.** The C++
   backend's transitional status does not loosen this -- if its render needs decision logic in value
   emission (an `if` whose arms produce different syntactic shapes), the MIR is wrong, and the
   LIR/LLVM path would face the same obstruction. The C++ backend's render is therefore the
   cross-check on MIR shape today (`backend_contract.md`).
8. A lowering translates node by node, with no preliminary pass over its input. Each input node's
   output follows from that node and its own subtree; every input node produces output, whether or
   not it declares anything. A step that needs to know a property of the whole input before it can
   emit any of it has taken on a decision that belongs upstream.

## Boundary to Adjacent Layers

- Each lowering produces output that is valid in the target layer's contract. The output passes the
  target layer's invariants immediately; there is no intermediate "almost-HIR" or "partial MIR"
  state exposed to other stages.

## Forbidden Shapes

- A lowering pass that writes data back into its input layer.
- A lowering pass that skips an IR layer (for example, HIR to LIR directly).
- A lowering pass that produces output violating the target layer's invariants and defers fixup to a
  later pass.
- A lowering pass that reconstructs upstream ownership from string names or coordinates.
- A lowering pass that depends on runtime identity or per-instance data at compile time.
- A lowering pass that invents a new identity kind not defined by either the input or output layer's
  contract.
- A lowering that computes a property over its entire input before it can translate any part of it.
  The property is a decision, and it belongs to the layer that already held what it is computed
  from.
- A predicate inside a lowering that gives one source construct two different output shapes. Every
  downstream consumer then carries both shapes, and the one a consumer forgets is a defect that the
  other shape hides.
- An entity whose output placement is decided by a property of what its descendants contain rather
  than by what it declares itself. Placement that flows outward obliges a later stage to record
  where each thing went, and every consumer to consult that record.
- An output node emitted only when its input node is non-empty. Zero is a count, not a different
  shape; a construct that declares nothing produces the same output as one that declares many, with
  an empty list.
- An output node that omits a fact the input node stated, leaving a consumer to recover it from the
  node's type, its name, or its position. The consumer's recovery is a table over the cases someone
  enumerated, and the case nobody enumerated is a hole.

## Notes / Examples

If HIR-to-MIR needs to emit a CFG node, the boundary has been violated. Either the node belongs in
MIR in a non-CFG form, or the lowering is doing MIR-to-LIR's work.

When extending HIR-to-MIR, the lowering's output is also validated against `backend_contract.md`'s
mechanical-translation invariant: the produced MIR must be translatable by a mechanical LLVM IR
backend without payload-driven branches in value emission. The C++ backend's transitional status as
the current observation surface for MIR makes this check operational today -- if the C++ render
would need decision logic to consume a proposed MIR shape, the MIR is wrong, and the lowering is
what produced it.
