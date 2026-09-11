# Front-End Semantic Facts and Lyra Route/Endpoint Translation Boundary

## Date

2026-07-08

## Status

Accepted. D3's navigation-segment classifier is widened by `unit-signature.md`: descending into
another unit's instance body crosses the boundary, but the step is still typed when its target is a
declaration that unit published, and by-name only when it is not.

D1's forbidden shape held on the read path from the day this was written and on the write path only
from 2026-09-10, when the last classifier reading the declaration table's membership was removed --
see "What the write path owed" below.

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
   (`always_comb`, `always @*`, continuous assign, `wait`, port connection).
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

## What the write path owed

D1 and D3 were realized for reading and observing and not for writing, and the gap outlived this
entry by two months because nothing in it named the write path as a separate site. A walk over the
target expression ran ahead of the assignment lowering and asked, of every name it met, whether the
declaration table held it -- which is D1's forbidden shape exactly, in the direction this entry did
not enumerate. It decided three unrelated things with it: whether the program was legal
SystemVerilog, whether the target was a net or a variable, and whether the target was reachable.

**The front end owns the first, and had already answered it.** slang verifies that an assignment's
target is an lvalue whose every element can be assigned to, and refuses the program otherwise, so
every refusal that walk reached for legality was unreachable behind a front-end error. **The
reference owns the other two**: what a name reaches is stated by the route it lowers to, and what
may be done with it is stated by the endpoint's capability type. Neither was ever the walk's to
answer, and because it answered them from a table that only records this unit's own declarations,
its answer for a target in another instance was to refuse a continuous assignment LRM 10.3 makes
canonical.

So a continuous assignment to a target reached by a hierarchical name (LRM 10.3, 23.6) needs no
mechanism of its own: it is the assignment already lowered, against the reference already
translated, driving through the driver a net already attaches at Resolve. What Lyra legitimately
refuses about a target is whether it lowers the construct yet, and that answer belongs where the
lowering runs out of vocabulary rather than in a pass ahead of it.

**The general rule, which is D1 stated for every direction: a fact the front end has resolved is
translated once and consulted, never recomputed, by any consumer.** A second walk that recomputes it
does not merely duplicate work -- it computes a different answer, because it has less to compute
from, and nothing compares the two.

**D1 is now a build failure rather than a sentence, and that is the part worth carrying.** A gap of
this shape sits between two axes the architecture holds independent -- which construct states a
target's value, and how the target is named -- and no test run can see it, because each axis is
covered on its own and only the combination is wrong. Every suite stays green. So the instrument is
not a case but a check over the boundary itself: the declaration registry has two owners, and any
other site naming it is a construct deciding for its own case what the route decides for every case.
Written as a sentence here, that rule was true, correct, and unable to reach the site that broke it
for two months; the check would have failed the build the day it was written.

## What a refusal at this boundary is allowed to say

The same boundary decides what a refusal downstream of the front end may claim, and a sweep of every
one of them found the claim wrong more often than the code. Three verdicts are available and the
error policy already assigns them; what had gone missing is that one situation was spelled all three
ways.

- **The front end refuses the program.** The site is then unreachable -- lowering does not run when
  the front end reported an error -- and it is usually one arm of a dispatch over the front end's
  own enumeration, so the arm has to exist. It states a compiler invariant break, because reaching
  it means the front end accepted what the language forbids. A user-facing refusal there claims a
  gap that is not one.
- **The front end accepts the program and this compiler does not carry it.** That is a gap, it is
  stated as one, and the message names the construct rather than telling the reader their correct
  code is wrong.
- **The front end accepts it and the standard forbids it.** The refusal is this compiler's alone and
  cites the clause -- and the clause is read before it is cited, which is the step that fails: a
  refusal was found citing a clause that, read, says the opposite of what the refusal claimed.

**Two signals find these mechanically, and neither is a function name.** A diagnostic from the "this
program is illegal" family raised after the front end is a candidate by construction, since that
family is the front end's job. So is a refusal phrased as a rule about the source -- "must be a
...", "is not legal". Each candidate is then settled by feeding the front end alone the program the
site refuses, which is one command and decides which of the three verdicts applies.

## Consequences

- The order-dependent stale-value defect and the function-body-read defect are both closed by moving
  to the correct front-end surfaces and refusing to reclassify from degraded information.
- Value read, value write, and change observation reach a target through one route translator (D3),
  keyed on the reader's elaborated position and the target symbol. Neither re-derives the route, and
  no route depends on which consumer lowers first or on cross-unit-slot dedup. The translator
  classifies each segment by layout visibility from the target's elaborated position -- a typed
  enclosing climb or typed downward head while the segments stay in this unit, a by-name head where
  the route crosses the compilation-unit boundary -- so enclosing, downward-child, sibling,
  cross-module, and upward-out-of-unit references all route the same way, independent of the order
  the target and referrer appear in source. The reader is located in the elaborated hierarchy
  through its enclosing structural scope, so a read nested in a procedural block or a fork branch
  routes from that scope. slang's resolved reference path is provenance only, never a second routing
  authority.
- Every addressable structural identity, including a named procedural block's, is registered by the
  compilation-unit declaration pass before any body lowers. A named block's head identity is its SV
  label, registered directly from the elaborated scope members, so a forward cross-scope reference
  to it routes independently of declaration order like every other head kind. Its intra-unit access
  is a typed layout-visible segment: the enclosing climb reaches the borrowed handle on the
  materialized scope, which HIR-to-MIR recovers from the label.
- The endpoint-capability half of the boundary is realized. Value read, value write, and change
  observation of one target reach it through one bound endpoint: a reference that crosses a scope
  boundary seals to a per-instance endpoint in the resolve phase, and the hot path dereferences it
  rather than re-navigating the parent chain on each access; only a target on the reader's own scope
  is a direct member. The endpoint's access protocol is bound from the target's semantic kind and
  carried by the endpoint's type -- a variable's observable cell, a resolved net, a reference member
  -- so one lowering-time binding serves every consumer. A port connection reaches the child's port
  member through the same route as any hierarchical reference: an input or output port over its
  cell, a `ref` port bound once to the peer's cell through the one reference-store, with no
  port-only route species and no port-only alias path.

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
