# Lowering Boundaries

## Purpose

Define what each lowering step may and may not do. Lowering is one-way, and each step has a narrow
set of allowed transformations.

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

## Notes / Examples

If HIR-to-MIR needs to emit a CFG node, the boundary has been violated. Either the node belongs in
MIR in a non-CFG form, or the lowering is doing MIR-to-LIR's work.
