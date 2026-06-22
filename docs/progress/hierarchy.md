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
      consume. Landed for downward references at any depth: the slot is filled once, after the
      subtree is built, by navigating from the referrer's own child down to the referenced leaf. The
      resolve-once slot is the shared path ports will populate; upward resolution feeds the same
      slot in a later cut.
- [x] C2 -- A process on one instance can observe a member of another instance and re-evaluate when
      that member changes, without the observed instance knowing who watches it (cross-instance
      sensitivity). The combinational process subscribes through the resolved slot, independent of
      how deep the slot's path reaches.

This stage produces no user-visible feature on its own; it is the substrate the next two stages
consume. Coverage is demonstrated through Stage D and Stage E.

### Stage D -- Hierarchical references

- [x] D1 -- A downward reference reads and writes a signal in a child instance.
- [x] D2 -- An upward reference reads and writes a signal directly on the matched ancestor, at any
      depth. The child cannot know its depth when compiled, so it resolves the reference at
      construction inside its own artifact: it climbs its parent chain to the ancestor named by the
      reference (matching the module name, LRM 23.8; a nearer ancestor whose instance name happens
      to equal that module name does not shadow the target) and fetches the signal by name, naming
      no ancestor type (see `docs/decisions/hierarchical-reference-resolution.md`,
      `docs/architecture/emission_model.md`). A reference wrapped in a value-level operation
      (`Top.g[3]`, `Top.g + 1`) works -- the value part is ordinary expression handling. The
      remaining forms below are each rejected with a clean "unsupported" diagnostic, except D2d.
- [x] D2a -- An upward reference that descends through a child after the climb (`Top.sib.y`,
      `Top.mid.deep.z`, `Top.bank[2].y`): the climb reaches the ancestor, then the reference steps
      down by name into the ancestor's owned children to the leaf, at any depth and through array
      indices. The tail is by-name for the same reason the climb is -- the referrer owns neither the
      ancestor nor its children -- so each owner answers for its own children from the names it
      registered at construction. A leaf directly on the ancestor is the empty-tail zero-case of the
      same walk.
- [x] D2b -- An upward reference written inside a generate block (conditional or loop) rather than
      the module body. It resolves the same as one in the module body -- its member rides the
      generate-block scope and climbs that object's own parent chain -- including an upward write,
      per-iteration members in a loop block, and several blocks naming the same ancestor signal
      (each block gets its own resolution within its own scope).
- [x] D2c -- An upward reference whose head is a `$root`-anchored absolute path (LRM 23.6) or a
      named generate block (LRM 23.8), rather than a module instance. Both heads name a scope that
      already exists in the object tree -- `$root` is the implicit root that owns every top-level
      block, and a named generate block is a first-class constructed object -- so once the head
      locates that scope the reference reads the leaf by name like any other hierarchical reference.
      The head is the only new part: the climb matches an ancestor either by its module definition
      name (a module head) or by the ancestor scope's own name (a generate-block label, or `$root`),
      selected by the head; `$root` reaches the tree root and the rest of the path is a downward
      by-name walk. A top-level block now adopts `$root` as its parent so an upward climb from
      inside a top reaches the root. Covered for a scalar named generate block; an indexed
      loop-generate head (`blk[i].x`) is rejected with a clean diagnostic and remains.
- [ ] D2d -- LRM 23.9 instance-name precedence: when the first component is written as an instance
      name and several instances of the same module nest on the chain, the reference must bind the
      named instance. Today the climb keys on the module name, so it can silently bind the nearer
      same-module ancestor instead -- the one form that resolves wrong without an error. The common
      module-name form is correct at any depth; this gap needs the source-written first identifier,
      which the elaborated AST does not retain.
- [ ] D2e -- An upward reference whose head is a named procedural block (a named `begin`/`end` or
      `fork`/`join`, LRM 23.9). Unlike a module or generate scope, a named procedural block is not a
      constructed object today: its static-lifetime locals live as members on the enclosing unit
      (`docs/decisions/variable-lifetime-storage.md`), so there is no scope to climb to and no
      by-name signal to fetch. This form needs either named procedural blocks to materialize as
      navigable scopes or a member-name-aware resolution -- a model question deferred from D2c.
- [x] D3 -- Multi-level dotted paths resolve through the object tree across more than one level.
      Landed for downward paths through scalar instances.
- [ ] D4 -- A combinational process reading a hierarchical reference re-triggers when the referenced
      signal changes, including paths spanning multiple levels and reads from several instances.
      Downward paths re-trigger today at any depth; reads from several instances within one process
      remain.
- [ ] D5 -- The hierarchical path of an instance (for `%m`, display, and scope queries) derives from
      object-tree ownership.
- [x] D6 -- A hierarchical path that indexes an instance array (`c[i].x`) resolves to the selected
      element, including multi-dimensional arrays (`c[i][j].x`).
- [x] D7 -- A hierarchical reference crosses a generate-block scope boundary. A by-name reference
      reaches a generate block by its LRM name (the source label, or `genblk<n>` when unnamed, LRM
      27.6), indexes a loop-generate block, and continues to a signal or a further child inside it;
      when an if/case construct's alternatives share a name (LRM 27.5) the reference binds whichever
      alternative was instantiated. Covered for every head: an upward reference whose downward tail
      enters the generate; a reference from the scope that owns the generate descending into its own
      block (and regardless of whether the reference precedes the generate in source); a reference
      originating inside a generate block (see D2b); and a reference from an enclosing scope into a
      child instance's generate block (`leaf.g.x`, `leaf.bank[i].y`, and deeper through an instance
      inside the block).

Unlocks `refs/hierarchical_refs`, `refs/upward_refs`, and `instantiation/hierarchical_sensitivity`.

### Stage E -- Ports

- [x] E1 -- Port directions (input / output) and named or positional port connections at the
      instantiation site. Landed for variable-typed ports as the implied continuous assignment
      between the two objects' own storage (LRM 23.3.3).
- [x] E2 -- An input port reflects its parent-side source continuously: when the source changes, the
      child sees the new value and its dependent processes re-evaluate.
- [x] E3 -- An output port propagates a child write so the parent-side target observes it.
- [x] E4 -- Expression-driven and constant-valued port connections. A constant connection drives the
      port once at construction and then holds.
- [ ] E5 -- Single-driver net-typed ports alongside variable-typed ports; both behave as continuous
      assignments between the two objects' own storage.
- [x] E6 -- Pass-through ports (a port forwarded into a deeper child while the module keeps its own
      local state) and sibling-to-sibling connections through a shared parent signal. Landed for
      variable-typed ports; both endpoints keep their own storage.
- [ ] E7 -- A `ref` port aliases the connected variable as a hierarchical reference, sharing its
      storage with no separate continuous assignment (LRM 23.3.3.2).
- [x] E8 -- An input port left unconnected takes its declared default value (LRM 23.2.2.4). A
      declared default is a constant expression whose names resolve in the module that declares the
      port, not the instantiating scope; like a default argument at a call site, its value is
      materialized into the connection wherever the port is omitted. Omitting a port inserts its
      default; an explicit empty connection (`.port()`) suppresses the default and leaves the port
      at the data type's default initial value (LRM 23.3.2.2 / 23.3.3.2), as does an unconnected
      port with no default; an explicit expression overrides. Defaults are permitted only on input
      ports (LRM 23.2.2.4), which the frontend enforces.
- [ ] E9 -- A port connection on an instance array drives each element.
- [ ] E10 -- A port connection whose type is non-integral (an unpacked struct or array).

Unlocks the `ports/*` archive group.

## Open Questions

- How much of the specialization-key classification (which inputs are code-shape-affecting) is
  pinned in A2 versus refined as later stages reveal more code-shape-affecting inputs.
- Connection shorthands (`.*`, `.name` implicit) resolve to the same connection set as explicit
  named connections in the frontend; whether any need distinct handling is open. Positional and
  explicit named connections are both supported.

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
