# Multi-Module Hierarchy

Tracks multi-level module structure and hierarchical paths: a module instantiating other modules,
the per-instance object tree this builds, the connections across that tree (ports), and references
that name signals in another instance (hierarchical references), including the instantiation side of
generate.

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
- [x] A2 -- A module instantiated with different parameter values behaves according to each
      instance's own values. A binding that decides what is compiled produces a distinct compiled
      artifact, so several specializations of one module no longer collapse onto one; a value only
      read as a value is handed to each instance when it is built, and those instances share one
      artifact. Covers scalar value parameters (LRM 6.20.2) and type parameters (LRM 6.20.3); an
      aggregate value parameter gets a distinct identity too, but its end-to-end emit waits on
      aggregate type support. A type parameter bound to a type the instantiating module itself
      declares -- an enumeration, a structure or a union, packed or not, named or written in place
      -- is one type for every instance of that module, so they share the child's artifact. A child
      that compares two types handed to it (LRM 6.23) is refused, since the `type` operator is not
      supported; which types match is decided per instance (LRM 6.22), so that answer has to be told
      apart per instance when it is.

Unlocks the runtime side of `instantiation/param_slots`.

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
- [x] B7 -- An instance array whose elements are elaborated differently -- a `defparam` reaching one
      element (LRM 23.10.1), as in `defparam c[1].K = 9`, or a `bind` or configuration rule naming
      one -- builds each element as its own elaboration describes, in one or several dimensions.
- [x] B8 -- An override written elsewhere reaches only the instance it names: a `defparam` through
      any number of levels, a loop's block or an array of parents (LRM 23.10.1), a `bind` naming an
      instance or listing several (LRM 23.11), and a configuration's instance rule choosing a cell
      or setting parameters (LRM 33.4.1.6, 33.4.3), also where the instance sits under a parent
      instantiated more than once. A value a `defparam` or a configuration gives is held the way the
      instantiation's own assignment is.
- [x] B9 -- An interface array an override made differ in its elements reaches each element as its
      own elaboration made it (LRM 25.3, 23.3.3.5): through a port carrying a range, that port
      handed on to another, a part of the array connected to a port, a port and an array declared in
      opposite directions, and an array an interface holds reached through a port.
- [ ] B10 -- A `defparam` on a `parameter` a named block, task or function declares (LRM 23.10.2) is
      refused by the front end, which makes such a parameter a `localparam`; LRM 6.20.1 makes only a
      generate block's, a package's, a compilation-unit scope's and a class's parameter local.
- [x] B11 -- A design element is told apart by which one it is and not by its name alone. Two
      libraries may each hold a cell of one name (LRM 33.2.1, 33.3) and one design may hold both,
      whichever way a configuration chose between them: an instance clause, a cell clause, a library
      list inherited from an instance above, or a configuration an instance above was handed to (LRM
      33.4.1.4 to 33.4.1.6, 33.4.2). Two modules may each declare a module of one name inside them,
      beside one declared outside both (LRM 23.4), and a nested module reads the parameters of the
      module declaring it. A `bind` written in each of two cells of one name is two directives.
- [ ] B12 -- A module declared inside another that reads a variable or a net of the module declaring
      it (LRM 23.9) is refused: a body reaches storage outside itself only through a hierarchical
      name or a port.
- [ ] B13 -- `%l` and `%L` (LRM 33.7), which print the library and cell an instance is bound to, are
      refused as unknown format specifiers.
- [ ] B14 -- Two cells of one name mapped to one library stop the build as a duplicate definition,
      where LRM 33.3.1 has the later one replace the earlier with a warning. The front end binds the
      later one and reports the error under a warning option a caller may lower.
- [x] B15 -- A module carrying an interface that itself carries an interface standing inside that
      module (LRM 25.3, 25.3.3) builds and runs, whether the inner interface is one the module
      declares or one writing a name that lands in the module. It used to end the compiler with no
      message.

Unlocks `instantiation/multiple_instances`, `instantiation/nested_hierarchy`,
`instantiation/local_variables`, the runtime side of `instantiation/param_slots`, and
`instantiation/generate_repertoire`.

### Stage C -- Non-local access substrate

- [x] C1 -- Cross-unit references resolve once at construction into a stored direct reference, read
      directly thereafter (per `reference_resolution.md`). This is the substrate Stages D and E
      consume. Landed for downward references at any depth: the slot is filled once, after the
      subtree is built, by navigating from the referrer's own child down to the referenced leaf.
      Ports and upward references fill the same resolve-once slot.
- [x] C2 -- A process on one instance can observe a member of another instance and re-evaluate when
      that member changes, without the observed instance knowing who watches it (cross-instance
      sensitivity). The combinational process subscribes through the resolved slot, independent of
      how deep the slot's path reaches.

This stage produces no user-visible feature on its own; it is the substrate the next two stages
consume. Coverage is demonstrated through Stage D and Stage E.

### Stage D -- Hierarchical references

- [x] D1 -- A downward reference reads and writes a signal in a child instance.
- [x] D2 -- An upward reference reads and writes a signal directly on the matched ancestor, at any
      depth (LRM 23.8; a nearer ancestor whose instance name happens to equal the module name does
      not shadow the target). The child cannot know its depth when compiled, so the reference starts
      at the nearest enclosing scope of the class the name landed on, found once at construction,
      and reaches the signal as a member that scope published -- so a misspelt or mistyped name is
      refused where the child compiles. Two instances of one module whose upward names land on
      different classes compile separately (see
      `docs/decisions/a-design-element-publishes-its-declarations.md`). A reference wrapped in a
      value-level operation (`Top.g[3]`, `Top.g + 1`) works -- the value part is ordinary expression
      handling.
- [x] D2a -- An upward reference that descends through a child after the climb (`Top.sib.y`,
      `Top.mid.deep.z`, `Top.bank[2].y`): the climb reaches the ancestor, then the reference steps
      down through the children each scope published to the leaf, at any depth and through array
      indices. A leaf directly on the ancestor is the empty-tail zero-case of the same route.
- [x] D2b -- An upward reference written inside a generate block (conditional or loop) rather than
      the module body. It resolves the same as one in the module body -- its member rides the
      generate-block scope and climbs that object's own parent chain -- including an upward write,
      per-iteration members in a loop block, and several blocks naming the same ancestor signal
      (each block gets its own resolution within its own scope).
- [x] D2c -- An upward reference whose head is a `$root`-anchored absolute path (LRM 23.6) or a
      named generate block (LRM 23.8), rather than a module instance. Both heads name a scope that
      already exists in the object tree, so once the head locates that scope the reference reaches
      the leaf like any other hierarchical reference, as a member the scope published. A `$root`
      path starts at the top-level instance it names, and the rest of the path is a downward route
      through what each scope published. Covered for a named generate block and for an indexed
      loop-generate head (`blk[i].x`).
- [x] D2d -- LRM 23.8 step b / 23.9: the climb visits each enclosing scope's children rather than
      the ancestor itself, so the head can be a sibling of any enclosing scope rather than the
      closest enclosing scope. Two generate blocks at the same level reading each other's state
      through `<sibling_label>.<member>` resolves through this path; sibling-of-grandparent at any
      depth resolves through the same path. The frontend canonicalizes the head's identity (LRM 23.9
      instance-name precedence applies at the front end's resolution step), so the reference starts
      at the enclosing scope of the class the front end landed on.
- [x] D2e -- A hierarchical reference whose head is a named procedural block (a named `begin`/`end`,
      LRM 9.3.5 / 23.9). A named block is a first-class structural declaration of the compilation
      unit: it becomes a child runtime scope of its nearest enclosing addressable scope when
      something under it is reachable by name, its static-lifetime locals live on that scope's
      class, and `%m` inside it reports the block whether or not it does (LRM 21.2.1.5). A task or
      function is a scope on the same footing (LRM 23.9), so a named block inside one is named below
      it and `%m` inside one reports the subroutine. Intra-unit access (`outer.x` from a peer
      process) resolves as a typed route: the reference names the static itself, and which named
      blocks stand between it and its scope follows from the declaration, so a block label reused in
      two scopes cannot cross-bind. Cross-unit access (`Top.c.outer.x` from another module) is the
      same: the module publishes each static of its named blocks with the blocks it sits in, and the
      referrer reaches it as a published member, checked where it compiles. A named `fork`/`join`
      block as head is not yet supported; the front-end still rejects it at construction.
- [x] D3 -- Multi-level dotted paths resolve through the object tree across more than one level.
      Landed for downward paths through scalar instances.
- [x] D4 -- A combinational process reading a hierarchical reference re-triggers when the referenced
      signal changes, across paths spanning multiple levels, several instances read within one
      process, and upward references alongside downward ones. The process subscribes to every
      referenced signal regardless of direction or depth, and each source re-triggers independently.
- [x] D5 -- The hierarchical path of an instance (for `%m`, display, and scope queries) derives from
      object-tree ownership. Each runtime scope receives the whole of what it adds to a name (its
      label plus one elaborated index per dimension) from its parent at construction; `%m` walks the
      parent chain and joins each scope's own segment, never reverse-searching a parent registry for
      a child's bracketed name. A generate-loop iteration's identity therefore lives entirely on
      itself: `loop[0]` is what the iteration scope holds, not metadata the parent decorates onto an
      un-indexed `"loop"`. The walk stops at the implicit `$root` so multi-top output reads `Top.x`
      rather than `$root.Top.x`. Closure-deferred prints (`$strobe`) capture `self` via the closure
      builder, so the path printed is the issuing scope's, not whatever scope is active when the
      postponed region drains. A top stands under the identifier its module was declared with: one
      module compiles to one artifact per parameterization, and an artifact's name is not a name the
      design shows, so a parameterized top prints and is climbed to under its plain identifier like
      any other. VPI-style scope queries (`$scope`, `$function`) stay out of scope -- they belong to
      the assertion/debug workstream.
- [x] D5a -- The name a scope reports is text that refers to that scope and to no other (LRM 23.6).
      An identifier a hierarchical name has to escape is reported escaped, with its backslash and
      the white space that ends it, for an instance, a top, a generate block, a named block, a task
      and a function alike: one holding a character a simple identifier cannot, and a keyword. An
      escaped identifier that spells a simple one is reported as the simple one (LRM 5.6.1). An
      element of an instance array reports the index its range declares for it, whichever way the
      range runs and in every dimension. So two scopes whose identifiers spell alike once joined by
      periods have different names, and a foreign caller finds each by its own (LRM Annex H.9.3).
      Which identifiers are keywords is read off the language version the build compiles under, not
      off a `begin_keywords` region around the declaration.
- [x] D6 -- A hierarchical path that indexes an instance array (`c[i].x`) resolves to the selected
      element, including multi-dimensional arrays (`c[i][j].x`).
- [x] D7 -- A hierarchical reference crosses a generate-block scope boundary. A reference reaches a
      generate block by its LRM name (the source label, or `genblk<n>` when unnamed, LRM 27.6),
      indexes a loop-generate block, and continues to a signal or a further child inside it; when an
      if/case construct's alternatives share a name (LRM 27.5) the reference binds whichever
      alternative was instantiated. Every generate block publishes what it declares, and the scope
      holding a generate construct publishes which block was built at each index or label, so each
      step is checked where the referrer compiles. The blocks of a loop that differ in nothing a
      class declares are objects of one published class, named from the source alone
      ([../decisions/a-generate-block-is-a-nested-definition.md](../decisions/a-generate-block-is-a-nested-definition.md)).
      Covered for every head: an upward reference whose downward tail enters the generate; a
      reference from the scope that owns the generate descending into its own block (and regardless
      of whether the reference precedes the generate in source); a reference originating inside a
      generate block (see D2b); and a reference from an enclosing scope into a child instance's
      generate block (`leaf.g.x`, `leaf.bank[i].y`, and deeper through an instance inside the
      block).
- [x] D7a -- An instance inside a generate block names the block that holds it, or one enclosing
      that, by an upward name (LRM 23.8), and stays one unit however many blocks the loop counts
      out: the name starts at the nearest enclosing scope of the block's class, and every block of
      one application is of that class. A subroutine a block publishes is called from another module
      on whichever block the name selects, also where the blocks' code came out different.
- [x] D8 -- A loop-generate iteration reads another iteration of the same loop by hierarchical name
      from inside its own body (`g[i-1].v` -- the systolic / pipeline / carry-chain shape),
      including a forward read of an iteration constructed after the referrer. The reference binds
      only after the whole object tree exists, so a sibling not yet constructed when the referrer's
      own construction runs still resolves; no instance is dereferenced across the boundary during
      construction. The intra-unit, indexed extension of the sibling reads in D2d.

#### What a hierarchical name reaches, by declaration kind and by route

The two axes of Stage D, and the only statement of this workstream's coverage that can be checked
rather than believed. Rows are the declaration kinds LRM 23.8's Syntax 23-8 enumerates, plus the
three LRM 23.9 adds by making a block, a task and a class define scopes. Columns are where a route
leads -- not the spellings, of which there are many, since `c.x`, `g[i].x` and `$root.Top.g.x`
differ in spelling and not in what each step is.

Legend: **ok** runs end to end -- **ref** refused with a located diagnostic -- **def** answers
wrongly or fails where nothing reports it, recorded in `tests/paths/*.defects.yaml`. A cell with no
letter is one nobody has run, and the corpus is what earns it one.

A cell is what the name reaches, which is a question the front end answers, so it reads the better
of the two backends: a target one backend carries and the other has no realization for is that
backend's gap and is recorded against it, not against the route.

| Declaration kind (LRM 23.8, 23.9) | in this unit | into a child instance | out of this unit | through an interface |
| --------------------------------- | ------------ | --------------------- | ---------------- | -------------------- |
| variable                          | ok           | ok                    | ok               | ok                   |
| net                               | ok           | ok                    | ok               | ok                   |
| parameter                         | ok           | ok                    | ok               | ok                   |
| port                              | ok           | ok                    | ok               | ok                   |
| enum value                        | ok           | ok                    | ok               | ok                   |
| named event                       | ok           | ok                    | ok               | ok                   |
| static of a named block           | ok           | ok                    | ok               | ok                   |
| static of a subroutine body       | ok           | ok                    | ok               | ok                   |
| class property, through a handle  | ok           | ok                    | ok               | ok                   |
| function or task                  | ok           | ok                    | ok               | ok                   |
| block or task, as a `disable`     | ok           | ok                    | ok               | ok                   |

**Read the rows, not the cells.** A gap is normally a whole row, because what a route reaches is
stated by its leaf alone and the head and steps that got there are the same ones every other kind
uses. So a kind that fails fails on every route, and a kind that works works on all of them -- which
is why closing the callable row closed four cells at once, and why closing the `disable` row after
it needed only the leaves the route ends at.

**A cell that is a cell rather than a row has always been a refusal standing on the route's head
rather than on its leaf**, and both of the ones this table carried came off the same way. A net out
of this unit was refused where a route headed at this unit's own scope reached the same net without
complaint, and a name through an interface port was refused past what the interface published where
the same name reaching the same declaration through a module instance resolved. Neither was a
missing realization: each was a second walk, or a guard, deciding for one head what the route
already decides for every head. That is the shape to look for before believing any single cell -- a
lone failure is either one of these or a misreading of the row.

**One row is not a route fact.** An enumerator is a constant, and a name ending at one is folded to
its value before any route is built, so that row reads `ok` everywhere by never reaching the object
tree at all. It stays in the table because LRM 23.8 lists it among what a hierarchical name may end
at.

- [x] D9 -- The declaration kinds a hierarchical name may end at. LRM 23.8 enumerates them -- a
      variable, a net, a parameter, a port, a named block, a function, a task -- and LRM 23.6
      enumerates what a name may do with one: be read, be written (by assignment or as an actual a
      subroutine writes through), be triggered off, and name a subroutine. Every kind but the last
      two resolves, over every route stages D1 through D8 carry and in every direction, with reading
      and writing and waiting sharing the one route: a variable, a net reached downward, a value or
      type parameter (including in a context that elaborates, such as a width), a port, an enum
      value, a named event triggered and waited on, a static a named block declares, a static a
      static task declares, a static a named block inside a static task declares, a property reached
      through a class handle, a member of a generate block, and an element of an instance array. The
      two the language names beside those -- naming a subroutine, and naming what a `disable` ends
      -- have their own items below.

      **"Be written" was read as "be written by a procedure" for two months, and the table said
      `ok` throughout.** A continuous assignment (LRM 10.3) whose left-hand side named a target in
      another instance was refused, while a procedural write and a `force` to the identical target
      both ran -- so every row of the table was right about what the name reached, and the
      enumeration above was wrong about what could be done with it. The cause was a walk over the
      target expression that ran ahead of the assignment lowering and asked whether this unit's own
      declaration table held the name, which is the shape `front-end-semantic-boundary.md` D1
      forbids; removing it is what closed the gap, and nothing was added.

      **The reading to carry: a coverage claim that enumerates what may be done with a target is
      checkable only for the ways someone ran.** The table's axes were routes and declaration
      kinds, so nothing in it had a column for "by which kind of assignment", and the one that had
      never been run read the same as the three that had.

- [x] D10 -- A subroutine a hierarchical name enables (LRM 23.6, 23.8.1). A call is the same route a
      read of a declaration takes, ending at the callable instead of at storage, so every spelling a
      read reaches by an enable reaches too: downward (`c.fn()`, `c[i].fn()`), into a child's
      generate block (`c[i].blk.fn()`), upward (`Top.fn()`), through a sibling of an ancestor
      (`Top.s.fn()`), through an absolute path, and through an interface port. A task suspends the
      enabling process until it completes exactly as an intra-unit enable does.

      A module publishes its subroutines as an interface does, so on every route the call compiles
      against the subroutine's published signature and is a direct call; a misspelt name or a wrong
      argument is refused where the caller compiles. An upward enable and a downward one are the
      same: each unit's declarations are derived from that unit alone, so two units calling into
      each other read each other's declarations without a cycle
      (`docs/decisions/a-design-element-publishes-its-declarations.md`). Both backends run every
      route.

- [x] D13 -- A block or task a `disable` names elsewhere on the hierarchy (LRM 9.6.2, 23.6). What
      the statement ends is selected by static declaration identity, so it may sit in another
      process and in another instance, and the name that reaches it is the same route a read of a
      declaration there takes: into a child instance and through an instance-array element, down
      into a generate block, out to an enclosing module, across to a sibling of one, through an
      absolute path, and through an interface port. A block inside a task, and the task itself, are
      each targets on that route, and ending one leaves everything the name did not reach running --
      a sibling instance of the same module, another element of the same array, another iteration of
      the same generate loop.

      A module publishes each disable target of its named blocks and tasks, stated with the named
      blocks and subroutines it sits in, so a name reaching one in another unit ends at a published
      member like any other leaf and is checked where the referrer compiles. Both backends carry
      this, as they do the enable above.

      A target the writing body's own declaration scope declares stays what it was -- an identity
      into that scope's own registry, with no route at all -- which is also the only form available
      inside a class method or a package subroutine, neither of them standing on the hierarchy a
      route walks.

- [x] D15 -- A static-lifetime local of a **subroutine body** named by a hierarchical path. LRM 23.9
      puts a task and a function on the path exactly as it puts a named block, and LRM 23.6 excludes
      only what an _automatic_ subroutine declares, so the two owe one answer -- and the block form
      resolved while the subroutine form was answered by nothing, on every route that left the
      declaration's own scope.

      What the name reaches is decided by which scope declares it, and that is the source's own
      answer, never the shape of the statements below it: a task's `int counted;` belongs to the
      task whether or not the statements that follow it were grouped, while a `begin ... end` the
      source wrote inside that task is a scope of its own and, unnamed, is one LRM 6.21 says no
      hierarchical name reaches into. Reading a sequence of statements as a scope of its own is what
      put every declaration written without a `begin ... end` behind a block nobody wrote. Nothing
      else was missing: the cell already sat on the object that replicates the declaration, and the
      subroutine already had its own node on the tree under its own name.

      What the name traverses on the way is decided the same way. A task or a named block between a
      declaration and the structural scope that replicates it is where the storage sits rather than
      an object holding it, so a name reaching such a static from a sibling scope of its own unit
      ends at that structural scope and not at the subroutine -- which the two kinds of scope now
      answer alike.

- [x] D12 -- A **class property reached through a handle across a unit boundary**, out of this unit
      and through an interface alike (LRM 23.6, 8.14). The class a module or interface declares is
      published with it, so the access compiles against the class the handle was declared with and
      reaches the property that class names, never one a derived class declares under the same name.

      What such a class keeps for itself -- a static property, a static method, and an object built
      by assigning `new` to the handle (LRM 6.22, 8.9, 8.10) -- belongs to the instance declaring
      the class, and a name climbing out of the reader's instance reaches that instance from where
      the climb lands (LRM 23.8). That landing is what tells one compiled module apart from another,
      so one module instantiated under several parents reaches the same instance from each.

- [x] D11 -- A hierarchical reference whose target is a net, in every direction. A net reached
      downward reads and is waited on like a variable, and the name that reaches it there ends at
      the same published member an upward name reaches, the net's resolution node itself -- so the
      two directions were never separated by what the leaf can answer with. What separated them was
      a guard reading the route's head, stated twice and reasoning about a cell a net does not have;
      removing both left reading an upward net and waiting on one working on either backend, with no
      realization added anywhere.

- [x] D14 -- A name reached through an interface port that the interface did not publish. An
      interface publishes its members (LRM 25.10), so nearly every name through a port is one the
      module compiles against; a name ending deeper than a member -- a static of a subroutine body
      or of a named block, the LRM 23.9 cases -- is published too, stated with the blocks and
      subroutines it sits in, and used to be refused there.

      It was refused by a second walk rather than by anything missing. The port decides where a
      descent starts and nothing else about it, so what each step below is follows from the one
      question every descent asks; the same port already carried a route that asked it, since a
      `disable` naming a block inside the interface reaches it through the general walk. Routing the
      port's own descent through that one classification is what closed this, and it removed a
      second place that decided what "published" means rather than adding a mechanism.

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
- [x] E5 -- Net-typed ports alongside variable-typed ports: a net driven across a module port in
      either direction, and a bidirectional port, which drives in neither and instead joins the nets
      on both sides into one resolution. The net side resolves its drivers per the net model; that
      model and both port facets are tracked in `nets.md`.
- [x] E6 -- Pass-through ports (a port forwarded into a deeper child while the module keeps its own
      local state) and sibling-to-sibling connections through a shared parent signal. Landed for
      variable-typed ports; both endpoints keep their own storage.
- [x] E7 -- A `ref` port and the variable it connects to are one shared piece of storage (LRM
      23.3.3.2): a read or write through the port is immediately the connected variable's value,
      with no copy and no delay, and a write through it wakes any process sensitive to the connected
      variable (see `docs/decisions/reference-as-data-type.md`). Covers plain `ref`, including a
      read in the child's own variable initializer and a `ref` forwarded through intermediate
      modules to a deeper child (every reference on the chain denotes the same variable). A
      `const ref` port (read-only through the reference) is rejected with a clean diagnostic and
      waits for its own cut. A process waits on the port's own name as it waits on anything else: an
      edge or value-change event control, the implicit sensitivity of an `always_comb` or `@*` that
      reads it, and a `wait` condition over it all watch the connected variable. Reading and waiting
      reach that variable differently, and the difference belongs to the reference: every operation
      on a cell -- a read, a write, a sampled read -- answers through the reference, while a wait
      needs the cell as storage in its own right, because a registration names storage. The
      execution backend refuses the second: a reference there names storage of one of two kinds and
      its type says which values that storage holds rather than which kind it is, so an address
      taken through one would name whichever kind the type does not admit.
- [x] E8 -- An input port left unconnected takes its declared default value (LRM 23.2.2.4). A
      declared default is a constant expression whose names resolve in the module that declares the
      port, not the instantiating scope; like a default argument at a call site, its value is
      materialized into the connection wherever the port is omitted. Omitting a port inserts its
      default; an explicit empty connection (`.port()`) suppresses the default and leaves the port
      at the data type's default initial value (LRM 23.3.2.2 / 23.3.3.2), as does an unconnected
      port with no default; an explicit expression overrides. Defaults are permitted only on input
      ports (LRM 23.2.2.4), which the frontend enforces.
- [x] E9 -- A port connection on an instance array drives each element's own cell (LRM 23.3.3.5).
      The connection is distributed per element -- replicated to every element when its size matches
      a single port, or mapped element to element when it matches the array dimensions -- and each
      element's port is then the same implied continuous assignment a scalar instance gets, in
      either direction. Covered for one- and multi-dimensional arrays, replicated and array-matched
      connections, on inputs and outputs.
- [x] E10 -- A port connection whose type is non-integral resolves as the same implied continuous
      assignment an integral port does (LRM 23.3.3), with no dependence on the data type: the child
      cell is driven from, or drives, the parent-side storage, and the output side re-triggers the
      parent when the child re-drives the whole signal. Covered for a string and an unpacked array
      connected in both directions. An unpacked struct port rides on unpacked-struct type support
      and lands with it.
- [x] E11 -- A port connected to part of an internal name (LRM 23.2.2.2): what crosses the port is
      that part, so two ports selecting disjoint parts of one name each carry their own connection
      into their own bits and neither disturbs the other's. The unit publishes which of its
      declarations the port reaches and the descent that reaches the part, so the connection lands
      on that part and the storage the two ports share stays one declaration. A `ref` port naming a
      part is rejected with a clean diagnostic, since such a port seals to the whole of the
      connected variable's cell (LRM 23.3.3.2).

Unlocks the port-connection surface.

## Open Questions

- Connection shorthands (`.*`, `.name` implicit) resolve to the same connection set as explicit
  named connections in the frontend; whether any need distinct handling is open. Positional and
  explicit named connections are both supported.

## Out of Scope

- Interfaces and modports, tracked in `interfaces.md`. An interface is a compilation-unit kind that
  instantiates, parameterizes, and is referenced hierarchically exactly as a module does, so this
  workstream already carries it; what that one adds is how a module reaches an interface it does not
  own -- the interface port, the modport view over it, and the virtual interface. A hierarchical
  reference resolved through an interface port belongs there.
- A port that may not be left unconnected, on the module a simulation is run from. A `ref` port (LRM
  23.3.3.2) and an interface port (LRM 23.3.3.4) both require a connection, and nothing instantiates
  a top to connect its ports, so the simulation has nowhere to begin. The module itself is supported
  and is reached by instantiating it from one that connects the port. Checking such a module reports
  what it always did; running or emitting one is refused with a diagnostic.
- Programs as a compilation-unit kind. A program is a unit kind in `compilation_unit_model.md`, but
  its scheduling region and termination semantics are its own subject rather than a facet of module
  hierarchy.
- Primitive and gate-level instances (UDPs, built-in gates).
- Specify parameters (`specparam`, LRM 6.20.5). They carry timing and delay values for specify
  blocks and belong to the timing domain, not the parameter-specialization path; they wait for a
  specify-block workstream rather than blocking this one.
- Unbounded parameter values (`$`, LRM 6.20.7) and the `$isunbounded` query over them. The value `$`
  denotes "no upper limit" and is used almost exclusively in assertion and property checkers;
  supporting it needs an unbounded-value representation and the `$isunbounded` system function,
  which belong to the assertion domain, not the parameter-specialization path. The specialization
  identity already distinguishes such a binding from a numeric one; only the value's representation
  is missing. It waits for the assertion workstream rather than blocking this one.
- Net resolution, including which nets a connection makes one resolution. A port connection's own
  structure is in scope here whichever direction it carries -- a directional one is a continuous
  assignment and a bidirectional one joins the two nets -- while what a net does with the drivers
  that reach it belongs to `nets.md`. A hierarchical reference whose target is a net is **not** in
  that domain and used to be listed here as though it were: a single-driver net reads and is waited
  on through a name in every direction today, which was never a question about nets.
