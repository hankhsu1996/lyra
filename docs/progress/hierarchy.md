# Multi-Module Hierarchy

Tracks multi-level module structure and hierarchical paths: a module instantiating other modules,
the per-instance object tree this builds, the connections across that tree (ports), and references
that name signals in another instance (hierarchical references). Covers the archive items under
`archived/tests/sv_features/hierarchy/` (`instantiation`, `ports`, `refs`) and the instantiation
side of `archived/tests/sv_features/generate/`. When the last item lands, this file is deleted.

The stage IDs (A1, B1, ...) are stable references. Stage letters **do** imply dependency order: a
later stage may not begin until the stages it depends on are settled. Within a stage the items are
not ordered.

## Contracts

This workstream reasons from these architecture docs; it does not restate them:

- `reference_resolution.md` -- intra-unit references resolve at compile time; cross-unit references
  (ports, hierarchical references, cross-instance triggers) resolve once at construction into a
  stored direct reference. This is why Stages D and E are two surfaces on one resolution path (Stage
  C), not separate features.
- `specialization_model.md` -- one compiled artifact per distinct code shape; value-only parameters
  flow in at construction.
- `compilation_unit_model.md` -- each module is an independently compiled unit.
- `hierarchy_and_generate.md` -- the object tree is built at construction; path identity derives
  from ownership on it.
- `runtime_model.md` -- constructor context builds the tree; simulation context runs processes.

## Dependency Order

```
A  Specialization as compilation unit
   |
B  Object-graph construction (instantiation)
   |
C  Non-local access substrate
   |
   +---- D  Hierarchical references
   |
   +---- E  Ports
```

- A gates everything: until more than one module can be compiled as its own unit, there is no second
  object to instantiate, connect, or reference.
- B gates C: a cross-unit reference can only be bound once the target instance exists in the object
  tree.
- C gates D and E: both are cross-unit references resolved over the same construction-time path.
- D and E are independent of each other; their relative order is an open question (see below).

## Sub-Steps

### Stage A -- Specialization as compilation unit

- [x] A1 -- More than one module definition compiles in a single run; the design is no longer
      assumed to be a single top module. Each module is its own independently compiled unit. The LRM
      permits multiple top-level blocks (LRM 3.11): every elaborated but uninstantiated module is
      implicitly a top-level block, and all of them sit under one implicit root scope ($root). The
      program constructs every top-level block as a child of $root and runs them all under a single
      scheduler on one shared time axis -- there is no single "main" block. This is end-to-end
      testable with no instantiation edge: two independent top modules each run their own processes
      under the shared schedule.
- [ ] A2 -- Inputs are classified into code-shape-affecting (enter the specialization key) versus
      constructor/config inputs (flow in at construction). One compiled artifact exists per distinct
      code shape, not per instance.
- [ ] A3 -- Two instances whose code-shape inputs match share one compiled artifact; value-only
      parameters differ per instance without forking the artifact.

Unlocks the compile-time side of `instantiation/specialization_grouping` and
`instantiation/param_slots`.

### Stage B -- Object-graph construction (instantiation)

- [x] B1 -- A module instantiates a child; the constructor builds the child as a distinct object
      owned by the parent in the object tree. Parent and child remain separate units.
- [x] B2 -- The same module instantiated several times yields independent objects, each owning its
      own state.
- [x] B3 -- Instantiation nests: a child that itself instantiates a grandchild builds a multi-level
      object tree.
- [x] B4 -- A child's own (non-port) local state and processes run correctly inside its object.
- [x] B5 -- Generate (`for` / `if` / `case`) wraps child instances: which generate blocks exist, and
      how many loop iterations, is a construction-time decision, and each block owns its instances
      as part of the object tree.
- [x] B6 -- An instance array (`Child c[3]()`) is one named member that expands to a vector of
      independent child objects.

Unlocks `instantiation/multiple_instances`, `instantiation/nested_hierarchy`,
`instantiation/local_variables`, the runtime side of `instantiation/param_slots`, and
`instantiation/generate_repertoire`.

### Stage C -- Non-local access substrate

- [x] C1 -- Cross-unit references resolve once at construction into a stored direct reference, read
      directly thereafter (per `reference_resolution.md`). This is the substrate Stages D and E
      consume. Landed for downward references at any depth: the slot's recipe is a navigation path
      from a local child member down to the referenced leaf, materialized once after the subtree is
      built. The resolve-once slot is the shared path ports will populate; upward resolution feeds
      the same slot in a later cut.
- [x] C2 -- A process on one instance can observe a member of another instance and re-evaluate when
      that member changes, without the observed instance knowing who watches it (cross-instance
      sensitivity). The combinational process subscribes through the resolved slot, independent of
      how deep the slot's path reaches.

This stage produces no user-visible feature on its own; it is the substrate the next two stages
consume. Coverage is demonstrated through Stage D and Stage E.

### Stage D -- Hierarchical references

- [x] D1 -- A downward reference reads and writes a signal in a child instance.
- [ ] D2 -- An upward reference reads and writes a signal in an ancestor instance.
- [x] D3 -- Multi-level dotted paths resolve through the object tree across more than one level.
      Landed for downward paths through scalar instances.
- [ ] D4 -- A combinational process reading a hierarchical reference re-triggers when the referenced
      signal changes, including paths spanning multiple levels and reads from several instances.
      Downward paths re-trigger today at any depth; reads from several instances within one process
      remain.
- [ ] D5 -- The hierarchical path of an instance (for `%m`, display, and scope queries) derives from
      object-tree ownership.
- [ ] D6 -- A hierarchical path that indexes an instance array (`c[i].x`) resolves to the selected
      element.
- [ ] D7 -- A hierarchical reference crosses a generate-block scope boundary: a path step through a
      generate block (`g.sig`), or a reference originating inside a generate block that names a
      signal in an enclosing scope.

Unlocks `refs/hierarchical_refs`, `refs/upward_refs`, and `instantiation/hierarchical_sensitivity`.

### Stage E -- Ports

- [ ] E1 -- Port directions (input / output) and named port connections at the instantiation site.
- [ ] E2 -- An input port reflects its parent-side source continuously: when the source changes, the
      child sees the new value and its dependent processes re-evaluate.
- [ ] E3 -- An output port propagates a child write so the parent-side target observes it.
- [ ] E4 -- Expression-driven and constant-valued port connections.
- [ ] E5 -- Single-driver net-typed ports alongside variable-typed ports; both behave as continuous
      assignments between the two objects' own storage.
- [ ] E6 -- Pass-through ports (a port forwarded into a deeper child while the module keeps its own
      local state) and sibling-to-sibling connections through a shared parent signal.

Unlocks the `ports/*` archive group.

## Open Questions

- Relative order of Stage D and Stage E. Both sit on the Stage C substrate. Hierarchical references
  exercise the read / write / sensitivity surface most directly and may be the cleaner forcing
  function for C; ports add connection direction and continuous propagation on top. Ports are more
  fundamental to real designs. Order to be decided when Stage C lands.
- How much of the specialization-key classification (which inputs are code-shape-affecting) is
  pinned in A2 versus refined as later stages reveal more code-shape-affecting inputs.
- Whether positional port connections and connection shorthands (`.*`, `.name` implicit) are in
  scope for Stage E or deferred.

## Out of Scope

- Interfaces, modports, and programs as compilation-unit kinds. They are unit kinds in
  `compilation_unit_model.md` but are a separate workstream from module hierarchy. A hierarchical
  reference resolved via an interface port belongs to that workstream.
- Primitive and gate-level instances (UDPs, built-in gates).
- Bind directives and configuration (`config` / `bind`).
- Net resolution and net merging: multi-driver resolved nets and net collapsing across ports. A
  single-driver net port behaves as a continuous assignment and is in scope; multi-driver net
  resolution is a separate design-global concern. `inout` ports are bidirectional net connections in
  this same deferred net domain. A hierarchical reference whose target is a net-typed (non-variable)
  signal follows net value support, which is not yet established in any scope.
