# Architecture

Binding contracts for each layer and cross-cutting concept of the compiler.

Every doc here is a **contract**. It describes the intended permanent shape of the subject. If the
current code violates a contract, the code is wrong, not the contract. A doc whose subject the code
does not yet fully realize says so once, in a paragraph directly under its title, and nowhere else,
so a doc with no such paragraph states that the code conforms to all of it.

`north_star.md` sits above every other document in this directory. If a lower-level document
conflicts with a North Star principle, the lower-level document is wrong and must be fixed.

## Reading Order

Read top to bottom on first pass:

1. `north_star.md` -- top-level objective and first-class constraints; constrains every other doc
2. `compiler_overview.md` -- pipeline, worldview, compile-time vs runtime; what a run produces and
   reports when something fails, and of its own cost
3. `compiling_a_design.md` -- the whole scheme for compiling a design to few artifacts, each named
   from inputs before it is built, so an edit rebuilds only what it changed; how it lines up with
   rustc, C++ and Bazel, the models it is not, how wide a build runs, and why compiling and running
   are separate
4. `compilation_unit_model.md` -- what a compilation unit is and owns; the signature it publishes
   and what crosses the boundary
5. `runtime_model.md` -- the constructor / simulation execution-context split, and the run around
   them: the design root, ambient run access, random stability, what a run records
6. `elaboration_lifecycle.md` -- the build / resolve / initialize / activate phase protocol; the
   time-zero start order
7. `specialization_model.md` -- what counts as an argument, and which lowerings are one unit and one
   body
8. `hir.md` -- source-near semantic IR; its type pool and the procedural scope tree
9. `mir.md` -- object-oriented semantic IR (objects, members, callables); how a call names what it
   runs; the rewrites between HIR-to-MIR and every consumer; constants
10. `callable.md` -- the one callable concept; callable code vs callable value; capture model;
    references as a field type; receivers; the completion payload; formal defaults; foreign
    callables; subroutines an interface leaves to a module
11. `object_model.md` -- the one nominal object model; module / scope / class as one object type;
    inheritance, dispatch, construction, references and handles; the class's definition; dynamic
    casts; member naming; classes declared inside an instance; what a published class promises
12. `storage.md` -- what has storage identity and what may alias it; an aggregate's components and
    what a whole-aggregate assignment does to them; access path versus retained identity; what a
    reference carries; container element identity; what a write reports and which storage a wait may
    name
13. `lifetime.md` -- the lifetime regimes and why the language needs more than one; a value's end
    emitted versus an object's discovered; managed reachability, precise tracing, activation frames,
    safepoints and roots; where a static lives and how type-associated storage is brought up; the
    lease a call's binding takes
14. `lir.md` -- execution-oriented IR (CFG, basic blocks, storage); unwinding; how a value lies
    below LIR and what code compiled once is told of a type
15. `activation.md` -- the runtime execution instance; completion slot; ownership / continuation /
    cancellation relations
16. `nonlocal_control_flow.md` -- leaving a scope from anywhere within it; region, raise, cleanup;
    the language layer under a cancellation library; disable targets and their generations; the
    foreign-frame boundary
17. `scheduling.md` -- stratified event scheduler, regions, suspension protocol; what a wait watches
    and what a write tells it; how a run ends
18. `assertions.md` -- sampled values and their clock, concurrent assertions and `expect`, deferred
    reports, assertion control
19. `verification_constructs.md` -- clocking blocks, covergroups and constraints: what each is made
    of in the object model and the library, and what each adds to scheduling
20. `hierarchy_and_generate.md` -- hierarchy and generate ownership; what a scope states of itself
    to the run; packages beside the tree
21. `reference_resolution.md` -- references as routes from origin to typed endpoint; every step
    typed against what a scope published; bound endpoint on the hot path; one reference per use; how
    a port's sink names its source
22. `net_resolution.md` -- a net's value as the resolution of its driver contributions; net vs
    variable; drivers attached along the reference route; procedural continuous assignment
23. `binding_and_capture.md` -- the lexical reference axis: logical binding identity, per-body
    materialization, closure capture forwarding, carrier vs view
24. `compiler_generated_storage.md` -- compiler-generated storage: the closure (`ClosureType`, a
    concrete callable value) and the lifted local (its own cell, held through a shared pointer),
    capture forms
25. `emission_model.md` -- how a backend emits independent per-unit artifacts and realizes each
    route step against the class a scope published
26. `backend_contract.md` -- per-node within-artifact realization rules; type mapping vs value
    emission; what a backend may and may not name in render; rendered text written once; how a value
    reads under a format
27. `identity_and_ownership.md` -- identity rules and forbidden shapes; an identity is never a
    rendering; a declaration's path of steps
28. `lowering_boundaries.md` -- what each lowering may and may not do; what a write names; array
    methods as loops; shape from types
29. `lowering_organization.md` -- how lowering passes organize their internal objects (facts,
    registries, builders, walk frame); staging identities, declarations and bodies; what a walk says
    when it fails
30. `incremental_build.md` -- query-based incremental compilation and caching; the store every kept
    result lives in
31. `runtime_distribution.md` -- the runtime a program links: what a unit's object holds of it, the
    execution backend's ABI with it, where it ships and is found, what a run writes
32. `foreign_interface.md` -- DPI-C and VPI: the standard's headers, linking foreign code, the
    boundary a design publishes, open arrays, the context extent, handles and callbacks
33. `building_a_program.md` -- what a build command answers for: claimed outputs, cleanup, status,
    the prepared header, the checked build
34. `project_model.md` -- what `lyra.toml` declares: a library and its design, dependencies and
    ports, and the command line selecting within it
35. `testing_strategy.md` -- what a test may claim; Lyra's records of its answers; benchmarks
36. `language_conformance.md` -- what source text is accepted, and how a disputed text is ruled on

## Concept Index

If you are looking for a concept, this table points to the canonical doc.

| Concept                                                                                                                                  | Canonical doc                   |
| ---------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------- |
| Primary optimization target; non-negotiable design constraints                                                                           | `north_star.md`                 |
| Pipeline (HIR -> MIR -> LIR -> LLVM IR); compile-time vs runtime split                                                                   | `compiler_overview.md`          |
| Collect-and-continue; a request names its products; time trace and stats file                                                            | `compiler_overview.md`          |
| Artifacts and their names; signature, body, construction data; blocks built; what an edit rebuilds                                       | `compiling_a_design.md`         |
| How wide a build runs; compiling vs running                                                                                              | `compiling_a_design.md`         |
| Compilation unit; class-level artifacts; instance records                                                                                | `compilation_unit_model.md`     |
| What a signature may carry; port projections; a view's names; `$unit` naming                                                             | `compilation_unit_model.md`     |
| Constructor vs simulation execution contexts; structural vs process                                                                      | `runtime_model.md`              |
| Storage and binding vs construction; type-driven member walk                                                                             | `runtime_model.md`              |
| The design root as a unit of the link; timescales and the global precision; ambient run access                                           | `runtime_model.md`              |
| Timescale queries: a named element's time unit and precision, and `$root`'s                                                              | `runtime_model.md`              |
| A system task or function that asks the run (time, printing, dumping, random values)                                                     | `runtime_model.md`              |
| Random stability; generators and seeds                                                                                                   | `runtime_model.md`              |
| Dumps; a scope's data-object rows                                                                                                        | `runtime_model.md`              |
| Elaboration phases (build / resolve / initialize / activate); ctor scope; time-zero start order                                          | `elaboration_lifecycle.md`      |
| Generate as constructor-time logic; object graph shape; child ownership and borrowed handles                                             | `hierarchy_and_generate.md`     |
| Instance array as a data type; multiplicity vs generate axes                                                                             | `hierarchy_and_generate.md`     |
| Scope records; the tree's root `$root`; packages as run objects; names from outside the design                                           | `hierarchy_and_generate.md`     |
| A system task or function that asks the hierarchy (`%m`, a scope's name)                                                                 | `hierarchy_and_generate.md`     |
| Reference routes; typed steps against what a scope published; bound endpoints                                                            | `reference_resolution.md`       |
| Port connection pairing; a sink naming its source                                                                                        | `reference_resolution.md`       |
| Net value as the resolution of driver contributions; net vs variable; drivers                                                            | `net_resolution.md`             |
| Force / release / assign / deassign                                                                                                      | `net_resolution.md`             |
| Logical binding identity; per-body materialization; capture forwarding; carrier/view                                                     | `binding_and_capture.md`        |
| Per-unit artifact emission; a scope's published part and realization; no wiring                                                          | `emission_model.md`             |
| Per-node within-artifact render; type mapping vs value emission                                                                          | `backend_contract.md`           |
| Rendered text written once; value formatting                                                                                             | `backend_contract.md`           |
| A constant as its bits at its type; the type a callee is generic over; an operand standing for a type                                    | `backend_contract.md`           |
| Parameter values, unit, body and lowering names, per-body artifacts                                                                      | `specialization_model.md`       |
| Identity rules; ownership; forbidden identity shapes                                                                                     | `identity_and_ownership.md`     |
| Lowering permissions (what each lowering may and may not do)                                                                             | `lowering_boundaries.md`        |
| Joins of lvalues; array methods; operands vs types                                                                                       | `lowering_boundaries.md`        |
| Lowering pass organization (facts / registry / builder / walk frame)                                                                     | `lowering_organization.md`      |
| Staging (identities, declarations, bodies); failure context; pool selection                                                              | `lowering_organization.md`      |
| HIR shape (statements, expressions, primaries)                                                                                           | `hir.md`                        |
| Type pools and interning; procedural scope tree; HIR equality                                                                            | `hir.md`                        |
| MIR shape (objects, members, callables)                                                                                                  | `mir.md`                        |
| Member and type model; object types; borrowed handle; vector wrapper                                                                     | `mir.md`                        |
| Integral type as width, signedness and state domain; a select or an index as a position                                                  | `mir.md`                        |
| Call forms; runtime entries; construction forms                                                                                          | `mir.md`                        |
| MIR phases and rewrites; verification                                                                                                    | `mir.md`                        |
| Consuming a closed set of alternatives: every consumer names each one                                                                    | `mir.md`                        |
| Records (struct, union, tagged union); enumerations                                                                                      | `mir.md`                        |
| Callable model; code vs value; captures; references as a field type                                                                      | `callable.md`                   |
| Receivers; completion payload; formal defaults; DPI-C callables                                                                          | `callable.md`                   |
| Closure (`ClosureType`) vs lifted local (a cell held by `Shared<>`); captures                                                            | `compiler_generated_storage.md` |
| Object model; nominal object types; inheritance; dispatch; handles                                                                       | `object_model.md`               |
| Construction order; dispatch tables; class definition; `$cast`                                                                           | `object_model.md`               |
| Object identity vs a reference's static view                                                                                             | `object_model.md`               |
| Access continuing past an object reference; upward-name anchors                                                                          | `reference_resolution.md`       |
| Storage identity; what a reference may bind; access path vs retained identity                                                            | `storage.md`                    |
| Aggregate components; whole-aggregate assignment; container element identity                                                             | `storage.md`                    |
| Ordering methods (`sort`, `rsort`, `reverse`, `shuffle`) and what a reference across one reads                                           | `storage.md`                    |
| What a write reports; observable storage form; update vs event identity                                                                  | `storage.md`                    |
| Lifetime regimes; when a value's storage ends and who ends it                                                                            | `lifetime.md`                   |
| Managed reclamation; tracing GC; activation frames                                                                                       | `lifetime.md`                   |
| Static storage placement; type-associated storage bring-up; lease                                                                        | `lifetime.md`                   |
| LIR shape (CFG, basic blocks, storage); unwinding                                                                                        | `lir.md`                        |
| How a value lies below LIR; what code compiled once is told of a type                                                                    | `lir.md`                        |
| Activation; execution instance; completion slot; cancellation domain                                                                     | `activation.md`                 |
| Leaving a scope from anywhere within it; region, raise, cleanup; landing                                                                 | `nonlocal_control_flow.md`      |
| `disable` targets, generations, the gate; DPI disable protocol                                                                           | `nonlocal_control_flow.md`      |
| Stratified scheduler; regions; suspension protocol; NBA / closure submit                                                                 | `scheduling.md`                 |
| Event controls; implicit sensitivity; what a write tells a wait; how a run ends                                                          | `scheduling.md`                 |
| Deferred and concurrent assertions; sampled values; assertion control                                                                    | `assertions.md`                 |
| Clocking block declaration, skew and drive; covergroups; constraints and the solver                                                      | `verification_constructs.md`    |
| Incremental compilation; query-based caching; kept results and the store                                                                 | `incremental_build.md`          |
| Locating and bundling the runtime; what a unit holds of it; runtime entries and type records; run output                                 | `runtime_distribution.md`       |
| DPI-C and VPI; the standard's headers; linking foreign code; open arrays; context scope; handles, callbacks                              | `foreign_interface.md`          |
| Claimed outputs; cleanup on a signal; build status; prepared header; `--sanitize address`                                                | `building_a_program.md`         |
| `lyra.toml`; libraries and dependencies; ports and `lyra.lock`; manifest versus command line                                             | `project_model.md`              |
| Test categories; suite layout; expectation forms; Lyra's records of its answers; benchmarks                                              | `testing_strategy.md`           |
| What text is legal; tolerated readings; ruling on a disputed text; what Lyra prints where the standard leaves the form open (`%s`, `%p`) | `language_conformance.md`       |

These docs are the contracts, each the one current statement of its subject. The few choices of
direction behind them are recorded in [`../decisions/README.md`](../decisions/README.md).

## Required Reading by Decision

Before designing a change, **read `north_star.md` first -- always**. Then read the docs that govern
the decision's subject, top to bottom, and **check the design against every relevant doc's
invariants and Forbidden Shapes before proposing it**. A design that fails a requirement an entry
names is wrong even if it compiles and passes tests; an entry that names no requirement is a
question for the design, never a verdict on it (`../design-process.md`).

| Decision touches...                                                                       | Binding docs (read all, top-down)                                                                                            |
| ----------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| Cross-unit access / hierarchical refs / ports / connectivity                              | `north_star`, `compilation_unit_model`, `reference_resolution`, `emission_model`, `elaboration_lifecycle`                    |
| Nets / drivers / net resolution / net-typed ports                                         | `north_star`, `net_resolution`, `reference_resolution`, `elaboration_lifecycle`, `mir`                                       |
| Backend emit / artifact structure / SDK boundary                                          | `north_star`, `compilation_unit_model`, `reference_resolution`, `emission_model`, `backend_contract`, `runtime_distribution` |
| Runtime library ABI / what a unit holds of the runtime                                    | `north_star`, `runtime_distribution`, `backend_contract`, `emission_model`                                                   |
| Compilation boundaries / unit dependencies / incrementality                               | `north_star`, `compiling_a_design`, `compilation_unit_model`, `specialization_model`, `incremental_build`                    |
| Build driver / project file / ports                                                       | `north_star`, `project_model`, `building_a_program`, `incremental_build`, `compiling_a_design`                               |
| Hierarchy / generate / object graph / construction                                        | `north_star`, `hierarchy_and_generate`, `runtime_model`, `elaboration_lifecycle`                                             |
| Identity / ownership / id kinds                                                           | `north_star`, `identity_and_ownership`                                                                                       |
| A new HIR or MIR node, or a new alternative of a closed set (start at `mir` invariant 19) | `north_star`, `compiler_overview`, `hir`, `mir`, `backend_contract`, `lowering_boundaries`                                   |
| Object model / classes / inheritance / dispatch / handles                                 | `north_star`, `object_model`, `lifetime`, `mir`, `callable`, `runtime_model`                                                 |
| Object lifetime / managed reclamation / GC / activation frames                            | `north_star`, `lifetime`, `object_model`, `mir`, `runtime_model`, `scheduling`                                               |
| References / aliasing / aggregate components / element identity                           | `north_star`, `storage`, `lifetime`, `mir`, `lir`, `backend_contract`                                                        |
| Locals / closures / capture / receiver / lexical references                               | `north_star`, `binding_and_capture`, `compiler_generated_storage`, `callable`, `mir`, `reference_resolution`                 |
| Waits / event controls / sensitivity / writes observed                                    | `north_star`, `scheduling`, `storage`, `activation`, `mir`                                                                   |
| Assertions / sampled values / assertion control                                           | `north_star`, `assertions`, `scheduling`, `hir`, `mir`                                                                       |
| Clocking blocks / covergroups / randomization                                             | `north_star`, `verification_constructs`, `scheduling`, `assertions`, `object_model`, `runtime_model`                         |
| DPI-C / VPI / foreign code                                                                | `north_star`, `foreign_interface`, `callable`, `runtime_model`, `runtime_distribution`, `nonlocal_control_flow`              |

The failure mode this guards against: anchoring on current code -- which may fall short of the
contract, as each doc's opening paragraph says where it does -- instead of the contract. Current
code that contradicts a contract is wrong; read the contract, not the code, for design direction.

## Document Shape

Type-contract docs (the IR layers, hierarchy, identity, ownership, lowering boundaries) follow the
template defined in `../style.md`. Behavioral and decision-cluster docs (`scheduling.md`) find their
own structure; the contract discipline still applies. See `../style.md` for which subjects use which
shape.
