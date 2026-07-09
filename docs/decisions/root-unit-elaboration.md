# Design elaboration is the synthetic `$root` unit's construct

Date: 2026-07-08 Status: accepted

## Context

Generated behavior reaches the runtime through a per-specialization unit definition
([generated-behavior-boundary](generated-behavior-boundary.md)): each unit supplies a `construct`
entry that builds its own owned children top-down, plus lifecycle entries. Construction is
root-anchored and cascading -- a unit root's `construct` runs once and builds its subtree, assigning
each created scope its program.

The design's top-level entry sits outside that model. Constructing each top-level module and
attaching it so the simulation can run is fabricated by the host, twice: the C++ backend emits it as
bespoke `main` text that names the runtime driver surface directly, and the in-process JIT
re-implements the same construct-and-bind sequence in its own driver. It is the one place a backend
invents design structure instead of translating generated behavior, and the invention is duplicated
per backend.

The runtime already carries half of the answer. `BindDesign` synthesizes an implicit root scope
`$root` with an all-no-op program and attaches each externally-constructed top to it as an owned
child; the architecture already states that the object tree has a single implicit `$root` owning
every top-level block as a child, and that program entry constructs every top-level block under
`$root` (`runtime_model.md`, `hierarchy_and_generate.md`). So the tops are already `$root`'s owned
children -- but `$root` has no construct of its own, and the host builds the tops externally and
hands them in.

The dividing line this decision rests on:

> MIR describes the semantic behavior of a design. The host / engine describes how a design artifact
> is executed.

The question for any code is not whether the engine could be a MIR builtin -- it could -- but
whether it is part of what the design _is_. Top-level elaboration is: it is the same "construct my
owned children" every unit performs, one level up. Engine creation, binding, and running the
scheduler are runner policy: they change with the execution mode (an AOT executable, an in-process
JIT runner, a differential test, an embedding API) while the design does not.

## Decision

`$root` is a first-class synthetic generated unit. Its `construct` entry _is_ the design
elaboration: it constructs the top-level units and attaches them as its owned children, exactly as
any unit's `construct` builds its children. Design construction becomes uniform end to end:

```
$root.construct
  -> Construct Top      -> Top.construct
                              -> Construct generate scopes / instances / children -> ...
```

There is no separate "design-level elaboration free function" and no new unit-level free-function
container in MIR / LIR. The top-level elaboration is the existing `construct` concept anchored at
`$root`, realized through the existing `UnitDefinition` / `ScopeProgram` machinery. Both backends
lower `$root.construct` mechanically like any other `construct`; neither fabricates top
construction.

The host shell keeps only runner policy. It constructs the one `$root` unit -- whose generated
constructor elaborates the design -- and hands it to the engine:

```
Engine engine;
auto root = construct the $root unit (engine.Services());  // its constructor builds the tops
engine.BindDesign(std::move(root));
engine.RunSimulation();
```

Constructing the `$root` unit is backend-specific only in its allocation -- the C++ backend
allocates its concrete class, another backend allocates a generic instance -- the same Phase 1
boundary [generated-behavior-boundary](generated-behavior-boundary.md) already draws around
allocation. The constructor body that runs is generated and backend-neutral. Engine creation,
`BindDesign`, `RunSimulation`, exit code, stdout capture, exception-to-exit mapping, JIT symbol
lookup, and AOT `main` policy stay host-side and never enter MIR. Generated code owns design
elaboration; the runtime / host owns engine lifecycle.

### Invariants

1. **`$root` is a synthetic elaboration root, not a source unit.** It does not pollute source-level
   name lookup, hierarchical names, or diagnostic identity. It exists so top construction has a
   receiver whose owned children are the tops; it is not an SV scope a user can name or reach.

2. **`$root` is produced at the design-link / compilation-aggregate level, not by lowering any
   single source module.** Its construct needs the top list, which no individual module has. It is a
   link product of the aggregate, referencing the top units' definitions.

3. **The elaboration lifecycle is fixed.** The host constructs the `$root` unit (allocation is a
   backend concern; its constructor is generated and elaborates the design by building every top and
   recursively the whole tree); `BindDesign` takes the constructed root and walks the already-built
   tree (resolve then initialize then activate, each a full top-down pass); `RunSimulation` starts
   the scheduler. `BindDesign` no longer receives a top list and no longer constructs -- it binds an
   existing tree.

4. **The host shell is thin and target-specific; the elaboration body is generated and
   backend-neutral.** The only generated behavior the shell invokes for construction is the `$root`
   unit's constructor. The C++ and JIT shells differ only in runner policy, never in how the design
   is built.

5. **Engine, bind, and run are not MIR.** They are runner policy. Modeling them as MIR builtins
   would make a design artifact a specific runner program, couple it to one execution mode, and
   invert the responsibility that the runtime drives the design.

## Rejected alternatives

- **`ElaborateDesign` as a loose design-level free function.** The earlier framing. It introduces a
  new unit-level free-function container in MIR / LIR that duplicates the `construct` concept.
  `$root.construct` reuses the existing model with no new container and makes top construction a
  special case of ordinary child construction.

- **The host constructs the tops externally and passes them to `BindDesign` (status quo).** The one
  place a backend invents design structure; duplicated across the emitted AOT `main` and the JIT
  driver; top construction is never observed as generated behavior, so the backend cross-check
  (`backend_contract.md`) does not cover it.

- **`Engine` / `BindDesign` / `RunSimulation` as MIR builtins.** Makes the artifact a specific
  runner program rather than a design program. It couples the compiled design to one execution mode,
  forces every runner (AOT `main`, JIT `Execute`, embedding, differential test) to share one
  generated main policy, and inverts responsibility -- the runtime should drive the design, not the
  design the runtime.

## Consequences

- `generated-behavior-boundary`'s `construct` is now anchored at `$root`, not only at each source
  unit root; the root-anchored, cascading construction that doc describes extends one level up to
  the synthetic root.
- `BindDesign` takes the constructed `$root` instead of a top list: the host builds `$root`, the
  engine takes ownership, and binding operates on the already-built tree.
- The C++ `main` emission and the JIT driver both collapse to the thin runner shell; the
  construct-and-bind duplication is removed, closing the render-contract gap tracked against
  `backend_contract.md`.
- In per-unit emission, `$root` is a synthesized link-level unit referencing the top units'
  definitions, not a lowered source module.

## Cross-references

- [generated-behavior-boundary](generated-behavior-boundary.md) (the unit definition / scope program
  / construct model this anchors at `$root`)
- [elaboration-lifecycle-phases](elaboration-lifecycle-phases.md) (the build / resolve / initialize
  / activate protocol `BindDesign` walks)
- [jit-value-realization](jit-value-realization.md) (the generated-call scope wrapping each
  runtime-to-generated call, including the root construct)
- `../architecture/runtime_model.md`, `../architecture/hierarchy_and_generate.md` (`$root` owns
  every top-level block as a child; program entry constructs the tops under `$root`)
