# LIR

## Purpose

Define the execution-oriented IR. LIR expresses the compiled program as control-flow graphs, basic
blocks, and storage accesses, shaped for backend codegen.

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

## Core Invariants

1. LIR is a CFG-shaped IR. Every callable body is a graph of basic blocks with explicit successors.
2. Each LIR value has an explicit type and an explicit storage source or dataflow origin.
3. Storage placement is decided at LIR. Upstream layers do not carry offsets or addresses.
4. LIR does not reintroduce high-level semantic ownership. It consumes ownership decided by MIR.
5. The runtime protocol is explicit: suspend and resume points appear as CFG edges, not as lowering
   conventions implied by node order.

## Boundary to Adjacent Layers

- Consumes MIR. MIR-to-LIR lowering shapes MIR callables into CFGs, assigns storage, and
  materializes effect ordering.
- Produces input to LLVM IR. LIR-to-LLVM is mechanical translation rather than design.

## Forbidden Shapes

- A parallel identity or ownership system that duplicates MIR's.
- Object or member semantics carried into LIR nodes. Object model belongs at MIR.
- Hierarchy navigation logic expressed at LIR. Hierarchy is resolved by MIR and consumed as storage
  addresses at LIR.
- Language-level constructs reintroduced at LIR (a "foreach" node instead of a loop CFG).
- Implicit control flow. Every control transfer is an explicit edge.
- Reintroducing semantic structure that MIR has already lowered away.
- Storage decisions decided upstream and encoded in LIR as passthrough.
- A scheduling decision made outside LIR but implied by LIR shape.
- Basic blocks that cross callable boundaries.

## Notes / Examples

A MIR process becomes an LIR callable with a CFG. Its suspend points (for example, event controls)
appear as explicit CFG edges that hand control back to the scheduler.
