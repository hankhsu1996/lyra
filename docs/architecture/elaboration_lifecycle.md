# Elaboration Lifecycle

## Purpose

Define the staged protocol that turns compiled, per-unit classes into a live, observable object
graph. A SystemVerilog instance does not become a semantically observable elaborated instance the
moment its C++ object is constructed; it becomes one only after a defined sequence of phases
completes. This doc owns that sequence and the rule that separates **allocation** (a C++
constructor's only job) from **SystemVerilog elaboration semantics** (structure building, topology
resolution, initialization, activation), which are distinct phases.

The driving principle:

> A generated C++ constructor is an allocation mechanism, not a SystemVerilog semantic executor.
> SystemVerilog elaboration is a staged graph-building and topology-resolution protocol, not
> recursive C++ constructor execution.

This is grounded in the language itself: elaboration -- expanding instantiations, computing
parameter values, resolving hierarchical names, and establishing connectivity -- occurs after
parsing and **before** simulation (LRM 3.12), whereas variable initialization and process execution
are simulation-time-zero activity (LRM 4). Collapsing all of it into one recursive constructor
conflates phases the language keeps separate, and every cross-instance ordering hazard we have hit
(a child read before its `ref` port is bound, pass-through `ref`, an initializer that reads an
upward reference, input/output initial-value visibility) is a symptom of that conflation.

## Owns

- The rule that elaboration is a staged protocol with five semantic phases: **Build -> Resolve ->
  Seal -> Initialize -> Activate**. These are a contract, not necessarily five separate entry
  points; an implementation may collapse phases for a unit that needs no separation, but it may
  never reorder them or let one phase observe a later phase's results.
- The rule that a generated constructor allocates the instance shell only -- storage cells, port
  endpoint placeholders, the child table -- and runs no SystemVerilog semantic logic.
- The rule that cross-instance connections are recorded as **declarative facts** during Build and
  realized into candidate endpoints during Resolve, then committed at Seal.
- The rule that Seal is a global barrier on the elaborated design: every required reference is
  canonicalized, validated, and committed as a final endpoint atomically with respect to Initialize.
- The rule that variable initialization runs in Initialize, **after** Seal, so an initializer
  observes only sealed canonical endpoints and bound values.
- The phase each port-connection realization belongs to, by port semantics.
- The "observable only after sealing" contract: no SystemVerilog-visible behavior occurs, and no
  instance's incoming bindings are observed, before the phases that establish them complete.

## Does Not Own

- The shape of the object tree and the faithfulness of generate expansion (see
  `hierarchy_and_generate.md`). Build constructs that tree; this doc owns only the phase ordering.
- What a cross-unit reference resolves into and the by-name resolution mechanism (see
  `reference_resolution.md`). Resolve is _when_; that doc is _into what_ and _how_.
- Parameter and specialization strategy (see `specialization_model.md`). Baking versus flowing in is
  orthogonal to the lifecycle; this doc only requires structural parameters to be available by
  Build.
- IR shapes (see `hir.md`, `mir.md`).
- Event scheduling within simulation after Activate (see `scheduling.md`).

## The five phases

### Build

Construct the instance shell and the structural object graph. This phase runs the unit's
**structural** elaboration: it applies the parameter environment, evaluates generate (`if` / `for` /
`case`) to decide which children exist, creates child shells (recursively), and **records** the
unit's connection facts -- port connections, hierarchical references, alias bindings -- as
declarative records. Build does **not** execute any value-level SystemVerilog body: no variable
initializer, no process body, no port read, no reference dereference.

Generate is Build-phase logic: its conditions are constant expressions (LRM 27.5), available from
the parameter environment, so generate never depends on Resolve.

### Resolve

With the full shell graph in existence and every connection fact recorded, execute each reference's
route. Each route uses typed navigation through layout-visible segments and the runtime SDK across
unit boundaries; each route produces a **candidate endpoint**. A route whose execution requires
another reference's sealed endpoint waits for that reference's resolution first; the order respects
those dependencies, however they are realized.

Resolve produces candidates, not committed final endpoints. A candidate may still be a forwarding
link, and a candidate may yet fail validation. Sealing those is the next phase.

### Seal

Commit every reference's resolved endpoint. Seal is a global barrier on the elaborated design: it
validates each candidate endpoint against the reference's declared access protocol; collapses every
forwarding link to the final canonical endpoint; rejects every reference whose route traverses a
non-constructed runtime object (a non-selected conditional-generate arm, an out-of-range
instance-array element); and commits each reference's endpoint atomically.

A reference visible to Initialize is sealed; an unsealed reference is invisible. Seal is a property
of the elaborated design as a whole, not of any single scope -- forwarding chains cross scope
boundaries and CU boundaries, and a per-scope walk cannot model that.

A forwarding chain ends at a **direct final cell** after Seal, flattening any number of intermediate
forwarding links: a pass-through `Top.z -> Mid.r -> Leaf.r` ends with both `Mid.r` and `Leaf.r`
denoting `Top.z`'s cell, never a reference that points at another reference.

### Initialize

Execute SystemVerilog variable initializers (LRM 6.8 / 10.5). This runs after Seal, so an
initializer reads only sealed canonical endpoints: a port read returns its bound source's value, an
upward reference reads its committed target cell, a `ref` read denotes the final aliased storage.
This is simulation time-zero activity, not construction.

### Activate

Register reactive behavior and begin simulation: install continuous-assignment / always processes,
seed `initial`, arm event controls, and enter the scheduler. All storage topology is sealed before
this phase.

## Core Invariants

1. **Elaboration is staged; the constructor only allocates.** The five phases run in order. A
   generated constructor produces an inert shell; it executes no variable initializer, no process
   body, and no port/reference read.
2. **Build before Resolve, system-wide.** Every shell and every connection fact exists before any
   cross-instance reference is resolved. Resolution is never interleaved with shell construction.
3. **Connections are declarative until Resolve.** A port connection or cross-unit reference is a
   recorded fact after Build; it becomes a candidate endpoint only in Resolve. Recording a
   connection never requires the other endpoint to be resolved yet.
4. **Resolve respects dependency order.** A reference's route may depend on another reference's
   sealed endpoint to execute. The order in which routes resolve respects those dependencies; a
   reference whose route requires another's sealed endpoint resolves after that reference seals. The
   mechanism that realizes this order is unspecified at this layer.
5. **Seal is a global barrier.** Seal commits each reference's validated, canonical final endpoint
   atomically with respect to Initialize. No reference observable at or after Initialize is a chain
   to another reference, an unresolved route, or a candidate that has not validated. Seal collapses
   every forwarding chain to its final direct cell.
6. **Initialize after Seal.** A variable initializer observes only sealed canonical endpoints, never
   an unbound reference, an unsealed forwarding link, or an unconnected port.
7. **Parent-edge ownership.** A connection whose behavior evaluates a parent-side actual expression
   or depends on parent-side sensitivity is realized as a **parent-owned reactive process**,
   registered in Activate. It is never relocated into the child, which cannot know the parent's
   expression, read-set, or scheduling.
8. **Observable only after sealing.** No SystemVerilog-visible behavior occurs before Activate, and
   no instance's incoming bindings are observed before Seal commits them. SystemVerilog-visible
   time-zero ordering is a property of these phases, never an accident of when a C++ constructor
   returns.
9. **The hot path consumes only sealed endpoints.** Simulation-time reads, writes, and observations
   reach sealed endpoints; no hot-path access performs route traversal, name matching, or resolution
   lookup. Whatever mechanism orders Resolve and Seal exists only at elaboration; it does not
   persist into simulation.

## Boundary to Adjacent Layers

- **The language (LRM 3.12, LRM 4).** Elaboration -- expanding instantiations, computing parameters,
  resolving hierarchical names, establishing connectivity -- happens after parsing and before
  simulation; this is **Build + Resolve + Seal**. Variable initialization and process execution are
  simulation-time-zero; this is **Initialize + Activate**. The phase split mirrors the elaboration /
  simulation boundary the language already draws.
- **`runtime_model.md`** defines the constructor / simulation execution-context split. This doc
  refines that split into the five phases.
- **`reference_resolution.md`** owns _what_ a route segment classifies as and what its endpoint
  becomes. This doc owns _when_: every reference resolves in Resolve, seals in Seal, and is read on
  the hot path after Activate.
- **`hierarchy_and_generate.md`** owns the object tree this lifecycle builds; generate is
  Build-phase constructor-time logic.
- **`specialization_model.md`** owns parameter strategy, orthogonal to the lifecycle.

## Forbidden Shapes

- A generated constructor that executes SystemVerilog semantic logic -- a variable initializer, a
  process body, a port read, a reference dereference -- rather than only allocating the shell.
- Resolving or sealing a cross-instance reference inside the constructor that builds the subtree. It
  forces a child to be fully constructed before its parent can bind it, which is the pass-through
  `ref` hazard and the construction-time-read hazard.
- Running a variable initializer before Seal completes. It observes an unbound reference, an
  unsealed candidate, or an unconnected port.
- A reference that remains a chain (a reference pointing at another reference) or unresolved after
  Seal.
- A per-scope walk that pretends to seal. Seal is a global property of the elaborated design;
  per-scope sealing cannot model cross-scope forwarding.
- Resolution dispatched on the frontend's lexical-form classification or on source order. The
  mechanism follows the route's segment layout visibility, not the form that named the target.
- Realizing an input or output port as anything other than a parent-owned reactive process, or
  relocating that process into the child.
- Realizing a `ref` port as a persistent cross-unit slot or a value-cell access path. A `ref` port
  is a forwarding link resolved away during Seal; it owns no child-side cell and needs no
  simulation-time reach from the parent.
- Letting "when the C++ constructor returns" determine any SystemVerilog-observable ordering.
- Reading a route's endpoint on the simulation hot path via hierarchy traversal or by-name lookup.
  Every nonlocal hot-path read consumes a sealed endpoint; whatever resolution mechanism produced it
  exists only across elaboration.

## Notes / Examples

**The three port semantics land in different phases.** They share only the connection _fact_
recorded in Build; their realization differs.

| Port               | Realization                                                                                                                                                   | Phase                     |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------- |
| `input` / `output` | parent-owned directional reactive edge (an implied continuous assignment); persistent endpoint resolved in Resolve, sealed in Seal, process armed in Activate | Resolve + Seal + Activate |
| `ref`              | forwarding link; the child's reference member sealed to the actual's final cell                                                                               | Resolve + Seal            |
| `inout`            | attachment of both terminals to one net connectivity / resolution domain (a separate net model)                                                               | deferred (net domain)     |

**Pass-through `ref`.**

```
module Mid(ref int r);
  Leaf c(.r(r));
endmodule
```

Build records two reference facts: `Mid.r` (alias of the parent's connected cell) and `Mid.c.r`
(alias of `Mid.r`). `Mid.c.r` resolves after `Mid.r` because it requires `Mid.r`'s sealed endpoint;
Seal collapses both to the final cell `Mid.r` ultimately aliases. The hazard that existed when
`Mid`'s constructor built `Leaf` before `Mid.r` was bound does not arise: binding is not a
constructor step.

**An initializer that reads a port.**

```
module Child(ref int r);
  int x = r;        // construction-looking, but a simulation-time-zero read
endmodule
```

`x = r` runs in Initialize, after `r` is sealed in Seal, so it reads the aliased cell's value. In a
recursive-constructor model this read would execute during `Child`'s construction, before the parent
bound `r` -- the bug this lifecycle removes.

**A sibling-of-sibling hierarchical reference.**

```
module Top;
  if (1) begin : a int ax; always_comb from_b = b.bx; end
  if (1) begin : b int bx; always_comb from_a = a.ax; end
endmodule
```

Both `a.bx` and `b.ax` are references recorded in Top's elaborated design. Build constructs `a` and
`b` in either order; both produce reference records. Resolve walks each route through the typed
parent edge into Top and the typed member access into the sibling's class; neither route depends on
the other's resolution. Seal commits each reference's endpoint. `always_comb` bodies read sealed
endpoints; the source order of `a` and `b` does not affect mechanism.

**Baked parameters are orthogonal.** When a parameter is baked into a specialization, "apply the
parameter environment" in Build is trivial (the value is a compile-time constant in that
specialization). When parameters instead flow in as constructor inputs, they are an incoming binding
that must be available by Build. Either way the phase ordering is identical; the lifecycle does not
depend on the parameter strategy.
