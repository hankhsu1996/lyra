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

- The rule that elaboration is a staged protocol with four semantic phases: **Build -> Resolve ->
  Initialize -> Activate**. These are a contract, not necessarily four separate entry points; an
  implementation may collapse phases for a unit that needs no separation, but it may never reorder
  them or let one phase observe a later phase's results.
- The rule that a generated constructor allocates the instance shell only -- storage cells, port
  endpoint placeholders, the child table -- and runs no SystemVerilog semantic logic.
- The rule that cross-instance connections are recorded as **declarative facts** during Build and
  realized during Resolve, with one resolution phase for all reference directions.
- The rule that variable initialization runs in Initialize, **after** Resolve, so an initializer
  observes connected and bound values.
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

## The four phases

### Build

Construct the instance shell and the structural object graph. This phase runs the unit's
**structural** elaboration: it applies the parameter environment, evaluates generate (`if` / `for` /
`case`) to decide which children exist, creates child shells (recursively), and **records** the
unit's connection facts -- port connections, cross-unit reference declarations, hierarchical paths
-- as declarative records. It does **not** execute any value-level SystemVerilog body: no variable
initializer, no process body, no port read, no reference dereference.

Generate is Build-phase logic: its conditions are constant expressions (LRM 27.5), available from
the parameter environment, so generate never depends on Resolve.

### Resolve

With the full shell graph in existence, realize every cross-instance connection. One phase resolves
all reference directions -- downward (into an owned child), upward (climb to a named ancestor), and
`ref` alias bindings -- into stored direct references, and materializes the persistent endpoints
that reactive edges will read. Because every shell already exists, resolution sees the whole graph
and is free of the ordering hazards that arise when resolution is interleaved with construction.

A `ref` alias is resolved here to a **direct final cell**, flattening any chain: a pass-through
`Top.z -> Mid.r -> Leaf.r` ends with both `Mid.r` and `Leaf.r` denoting `Top.z`'s cell, never a
reference that points at another reference. Resolution may require dependency ordering among aliases
(an alias whose actual is itself a not-yet-resolved reference resolves after it).

### Initialize

Execute SystemVerilog variable initializers (LRM 6.8 / 10.5). This runs after Resolve, so an
initializer may read a port, an upward reference, or a `ref` -- all are bound. This is simulation
time-zero activity, not construction.

### Activate

Register reactive behavior and begin simulation: install continuous-assignment / always processes,
seed `initial`, arm event controls, and enter the scheduler. All storage topology is sealed before
this phase.

## Core Invariants

1. **Elaboration is staged; the constructor only allocates.** The four phases run in order. A
   generated constructor produces an inert shell; it executes no variable initializer, no process
   body, and no port/reference read.
2. **Build before Resolve, system-wide.** Every shell and every connection fact exists before any
   cross-instance reference is resolved. Resolution is never interleaved with shell construction.
3. **Connections are declarative until Resolve.** A port connection or cross-unit reference is a
   recorded fact after Build; it becomes a stored direct reference only in Resolve. Recording a
   connection never requires the other endpoint to be resolved yet.
4. **Initialize after Resolve.** A variable initializer observes connected and bound values, never
   an unbound reference or an unconnected port. (This is the invariant our recursive-constructor
   model violated: an initializer ran during construction, before the relevant binding existed.)
5. **Reference sealing.** At the end of Resolve, every required reference points directly at a final
   observable cell. No reference remains a chain to another reference, and none is unresolved.
6. **Parent-edge ownership.** A connection whose behavior evaluates a parent-side actual expression
   or depends on parent-side sensitivity is realized as a **parent-owned reactive process**,
   registered in Activate. It is never relocated into the child, which cannot know the parent's
   expression, read-set, or scheduling.
7. **Observable only after sealing.** No SystemVerilog-visible behavior occurs before Activate, and
   no instance's incoming bindings are observed before Resolve completes for it.
   SystemVerilog-visible time-zero ordering is a property of these phases, never an accident of when
   a C++ constructor returns.

## Boundary to Adjacent Layers

- **The language (LRM 3.12, LRM 4).** Elaboration -- expanding instantiations, computing parameters,
  resolving hierarchical names, establishing connectivity -- happens after parsing and before
  simulation; this is **Build + Resolve**. Variable initialization and process execution are
  simulation-time-zero; this is **Initialize + Activate**. The phase split mirrors the elaboration /
  simulation boundary the language already draws.
- **`runtime_model.md`** defines the constructor / simulation execution-context split. This doc
  refines that split into the four phases. Where `runtime_model.md` previously placed variable
  initialization and downward-reference resolution "in the constructor," this doc relocates them to
  Initialize and Resolve respectively; `runtime_model.md` is updated to point here.
- **`reference_resolution.md`** owns what a cross-unit reference resolves into and the by-name
  mechanism. This doc owns _when_: a single Resolve phase, after Build, for all directions. Where
  `reference_resolution.md` previously split "downward in the constructor, upward at bind," this doc
  unifies both into Resolve; that doc is updated to point here.
- **`hierarchy_and_generate.md`** owns the object tree this lifecycle builds; generate is
  Build-phase constructor-time logic.
- **`specialization_model.md`** owns parameter strategy, orthogonal to the lifecycle.

## Forbidden Shapes

- A generated constructor that executes SystemVerilog semantic logic -- a variable initializer, a
  process body, a port read, a reference dereference -- rather than only allocating the shell.
- Resolving a cross-instance reference inside the constructor that builds the subtree. It forces a
  child to be fully constructed before its parent can bind it, which is the pass-through `ref`
  hazard and the construction-time-read hazard.
- Running a variable initializer before Resolve completes. It observes an unbound reference or an
  unconnected port.
- A reference that remains a chain (a reference pointing at another reference) or unresolved after
  Resolve.
- Realizing an input or output port as anything other than a parent-owned reactive process, or
  relocating that process into the child.
- Realizing a `ref` port as a persistent cross-unit slot or a value-cell access path. A `ref` port
  is a one-time alias binding performed in Resolve; it owns no child-side cell and needs no
  simulation-time reach from the parent.
- Letting "when the C++ constructor returns" determine any SystemVerilog-observable ordering.

## Notes / Examples

**The three port semantics land in different phases.** They share only the connection _fact_
recorded in Build; their realization differs.

| Port               | Realization                                                                                                                                               | Phase                 |
| ------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------- |
| `input` / `output` | parent-owned directional reactive edge (an implied continuous assignment); persistent endpoint resolved in Resolve, process registered/seeded in Activate | Resolve + Activate    |
| `ref`              | one-time alias binding of the child's reference member to the actual's final cell; no child cell, no persistent slot                                      | Resolve               |
| `inout`            | attachment of both terminals to one net connectivity / resolution domain (a separate net model)                                                           | deferred (net domain) |

**Pass-through `ref`.**

```
module Mid(ref int r);
  Leaf c(.r(r));
endmodule
```

Build records the alias fact `Mid.c.r <- Mid.r` declaratively, without resolving it -- `Mid.r` may
be unbound at that point, which is fine because no value-level code runs in Build. Resolve, with the
whole graph present, resolves `Mid.r` first and then `Leaf.r`, flattening both to the final cell.
The hazard that exists when `Mid`'s constructor builds `Leaf` before `Mid.r` is bound simply does
not arise, because binding is not a constructor step.

**An initializer that reads a port.**

```
module Child(ref int r);
  int x = r;        // construction-looking, but a simulation-time-zero read
endmodule
```

`x = r` runs in Initialize, after `r` is bound in Resolve, so it reads the aliased cell's value. In
a recursive-constructor model this read would execute during `Child`'s construction, before the
parent bound `r` -- the bug this lifecycle removes.

**Baked parameters are orthogonal.** When a parameter is baked into a specialization, "apply the
parameter environment" in Build is trivial (the value is a compile-time constant in that
specialization). When parameters instead flow in as constructor inputs, they are an incoming binding
that must be available by Build. Either way the phase ordering is identical; the lifecycle does not
depend on the parameter strategy.
