# Front-End Semantic Facts and Lyra Route/Endpoint Translation Boundary

## Date

2026-07-08

## Status

Accepted

## Why this decision matters

Two sensitivity defects share one root cause: Lyra reconstructs semantic facts that the front-end
(slang) has already resolved, from degraded information, instead of translating the resolved facts.

- A combinational reader of a same-unit cross-scope signal (a sibling or child generate block, a
  named block) silently loses its change subscription when the target was lowered before the reader:
  `TranslateSensitivityReads` finds the target in the compilation-unit-global structural-data-object
  table, computes `HopsTo` to it, and -- because a sibling/child scope is not on the reader's
  enclosing chain -- drops the dependency instead of routing it. The result is order-dependent: the
  same design compiles to a working or a broken sensitivity depending on declaration order.
- A reader whose dependency is read only inside a called function (`always_comb y = f();`, `f` reads
  a signal) has no subscription at all, so it never re-evaluates. LRM 9.2.2.2.1 requires an
  `always_comb` to be sensitive to signals read within any function it calls.

Both are consequences of the same boundary being crossed the wrong way. This entry fixes the
boundary so subsequent reference and sensitivity work inherits it.

## The model that shaped the decision

The front-end has already performed SystemVerilog semantic resolution. For every reference and every
sensitivity dependency, slang objectively provides:

- The resolved target: a `ValueSymbol*` that is an elaborated-instance-member identity (distinct per
  elaborated instance; the same symbol for one target across spellings; one symbol for a reference
  authored once inside a reused unit).
- For a hierarchical reference, the complete route: `HierarchicalReference` carries `target`, the
  originating expression, a per-hop `path` (each hop's traversed symbol plus its index/range/name
  selector), `upwardCount`, `isUpward()`, and `isViaIfacePort()`.
- The sensitivity dependency shape: `(ValueSymbol* target, bit range)` per read, plus a per-term
  edge for explicit event controls. The correct read set per consumer is a specific slang surface
  (below), not one lowered surface for all.
- The target's semantic kind: net vs variable (`SymbolKind::Net` with its net kind), and driver kind
  flags for ports and `ref` (`InputPort`, `OutputPort`, `ViaIndirectPort`).

Both the reader scope and the target symbol self-locate in slang's elaborated hierarchy
(`getHierarchicalParent()` / `getHierarchicalPath()`; the instance-body transition is the
compilation-unit boundary, which is exactly where `getHierarchicalParent` differs from
`getParentScope`). Therefore the route from a reader to a target is a structural relationship
between two already-resolved tree positions -- a translation, never a re-resolution of names. This
holds identically for a direct read (where a caller-side reference expression also exists) and a
function-body indirect read (where it does not); the general input is
`(reader context, target symbol, footprint/edge)`, of which the direct read is the special case.

## The decision

### D1. slang owns semantic resolution and sensitivity extraction; Lyra owns translation

Lyra does not re-resolve SystemVerilog names, does not re-derive up/down/root classification, and
does not classify a reference from degraded information. slang is the authority for which symbol a
reference denotes, what its hierarchy route is, which dependencies form a procedure's sensitivity,
and whether a target is a net, a variable, a port, or a `ref`.

Forbidden shape: deciding how to reach or whether to keep a dependency from
`ValueSymbol* + compilation-unit-global table + HopsTo`. The CU-global structural-data-object table
is a legitimate declaration-identity registry (see `declarations-before-bodies.md`); using its
membership, or a failed `HopsTo`, as a routing classifier is the defect. A failed `HopsTo` means
only "the target is not on the reader's enclosing chain" (a sibling or child, or another unit); it
never means "drop," and it is never the classifier for local vs cross-scope vs cross-unit.

### D2. Sensitivity source is consumer-specific

Inferred sensitivity is not one raw `DefaultDFA.getRValues()` for every consumer. `getRValues()` is
the `@*` surface (call arguments only); it does not satisfy the `always_comb` requirement that
function-body reads contribute. Each consumer reads the surface slang defines for it (this realigns
the code with `read-set-inference.md`, whose intent the current code diverged from):

- `always_comb` / `always_latch` (LRM 9.2.2.2.1, 9.2.2.3): `AnalyzedProcedure::getSensitivityList()`
  -- the implicit list with function-body reads inlined and locally/function-driven bits subtracted.
- `always @*` (LRM 9.4.2.2): the implicit-event region read set (function arguments only, no
  function-body reads).
- explicit event control (`always @(...)`, `always_ff`): the explicit timing-control terms, carrying
  per-term edges.
- continuous assign / implied port assign (LRM 10.3, 23.3.3): the continuous-assign surface;
  function-read inlining follows the chosen policy flag (the LRM leaves it unspecified).
- `wait(cond)` (LRM 9.4.3): a fresh sub-expression analysis of the condition.

Every inferred surface normalizes to the same dependency shape:
`(elaborated target ValueSymbol*, bit range)`, with an edge only for explicit event controls. The
target is a semantic target, not a route and not a runtime endpoint.

### D3. Translation is two orthogonal axes, keyed on objective slang facts

Given a semantic dependency `(reader context, target ValueSymbol*, footprint, optional edge)`, Lyra
translates it with no name resolution:

- Navigation-segment classification (per intermediate hop): a segment is layout-visible typed
  navigation when its source and target classes are both owned by the emitting unit, and opaque
  by-name navigation when it crosses into another compilation unit's body. The classifier is the
  slang scope kind at each hop: a module/interface/package body, a `GenerateBlock` /
  `GenerateBlockArray`, or a `StatementBlock` stays in the unit (typed); descending into an
  `Instance` / `InstanceArray` body, or climbing out through the reader's own instance, crosses the
  boundary (opaque). This is the per-segment rule of `reference_resolution.md`.
- Endpoint-capability binding (at the leaf and any forwarding point): the target's access protocol
  is bound from the target symbol kind and driver flags -- a variable to a plain observable cell, a
  net to a resolved-net node, a `ref` to a collapsed alias of the connected cell, a port to its own
  cell plus the implied continuous-assign edge -- narrowed by the footprint. This is
  `reference_resolution.md`'s "the endpoint inherits the target's access protocol."

Value read, value write, and change observation of one dependency reach the same target through this
one translation; they must not each re-derive it.

## Immediate changes

1. `TranslateSensitivityReads` stops dropping on a failed `HopsTo`: a dependency whose target is not
   an enclosing structural object is routed through the reference the body resolved, never
   discarded. This removes the order-dependent loss for every consumer that infers a read set
   (`always_comb`, `always @*`, continuous assign, `wait`, port connection). Interim correctness
   repair; it does not yet remove the parallel classifier.
2. `always_comb` / `always_latch` read `AnalyzedProcedure::getSensitivityList()` rather than raw
   `getRValues()`, so function-body reads contribute per LRM 9.2.2.2.1. The other consumers already
   read the surface their semantics require -- `always @*` and `wait` the reads of their controlled
   region (function arguments only, LRM 9.2.2.2.2 / 9.4.3), continuous assign its RHS read set --
   subject to one ordering correction: an `always @*` now infers its sensitivity after its body
   lowers, so a read's cross-unit reference is resolved before the subscription is built, the same
   order the other consumers already used.
3. Regression tests cover: a same-unit cross-scope read in both declaration orders with a mutation
   at t > 0 (order independence and real re-trigger, not t = 0 settling); an `always_comb` whose
   dependency is read only inside a called function; and an `always @*` reading a cross-scope signal
   declared before it.

## Consequences

- The order-dependent stale-value defect and the function-body-read defect are both closed by moving
  to the correct front-end surfaces and refusing to reclassify from degraded information.
- Sensitivity no longer decides whether to keep a dependency, or how to reach it, from the CU-global
  table's membership plus a `HopsTo`, and neither the value read nor the observation classifies its
  route by slang's lexical up/down. A compilation-unit declaration pass (D3,
  `declarations-before-bodies.md`) registers a generate's owned-child identity before any body
  lowers, so both operations route a cross-scope target by layout visibility from its elaborated
  position -- a typed climb-and-descent while the segments stay in this unit, a by-name segment only
  across a unit boundary -- independent of the order the target and referrer appear in source. The
  observation supplies its own route for a dependency the reader's body never resolved (a signal
  read only inside a called function) and otherwise reuses the endpoint the body produced.
- Two gaps remain before this boundary is fully realized. The value read and the observation are
  built by two routines that duplicate the layout-visibility routing rather than one shared
  translation. And the declaration pass covers generate identities; instance and named-block
  identities are still registered during the body pass, so a forward cross-scope reference to one of
  those is not yet order-independent. Consolidating the routing onto one translation over every
  addressable identity is the remaining step.
- Whether value access and change observation ultimately share one bound runtime endpoint (and
  whether an enclosing reference binds a per-instance handle rather than re-navigating the parent
  chain on the hot path) is a later endpoint-capability decision, constrained by this boundary but
  not settled here.

## Relation to existing decisions

- `read-set-inference.md` chose these per-consumer surfaces; this entry makes the consumer-specific
  selection binding and records that the implementation must not collapse them onto `getRValues()`.
- `reference_resolution.md` / `hierarchical-reference-routing.md` own the per-segment route
  classification this entry translates onto; sensitivity observation rides the same route as value
  access.
- `declarations-before-bodies.md` owns the CU-global declaration-identity registry; this entry
  forbids using that registry's membership as a routing classifier.
- `net-driver-resolution.md`, `reference-as-data-type.md` own the endpoint capabilities (net, `ref`)
  the translation binds to.
