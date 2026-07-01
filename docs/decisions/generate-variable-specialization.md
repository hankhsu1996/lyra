# Generate Variables Are Specialization Inputs

## Date

2026-06-30

## Status

Accepted. Supersedes an earlier witness-lowering framing (re-symbolize a concrete iteration's genvar
into one shared body) and a "two lowering modes" framing. There are not two modes of generate
lowering; there is one correctness baseline and one deferred compile-performance optimization,
uniform with how parameters are handled.

## Why this decision matters

A generate variable is an elaboration-time input, exactly like a module parameter: its value is
fixed at elaboration, and it may determine the compiled static representation, or constructor-time
structure, or neither. Which of these it does cannot be assumed. Lowering a `generate for` once with
the genvar as a runtime induction value assumes it determines neither -- unsound when it shapes
static representation, as with `in[i-1:0]`, whose width and type are the genvar. This record fixes
that a generate variable is a specialization input, on the same axis as a parameter
(`specialization_model.md`, `parameter-code-shape-over-approximation.md`, `north_star.md` invariants
2-3).

## Findings that shaped the decision

### F1. A generate variable and a parameter are the same kind of input

Both are fixed at elaboration. When a value flows into a body, we do not yet know whether it shapes
the compiled representation. A parameter passed to a child and a genvar used in a body are the same
situation: an elaboration-time value of unproven effect.

### F2. Today the two are handled oppositely

A module parameter is conservatively resolved to a concrete value -- every distinct binding is its
own specialization, which is always correct (`parameter-code-shape-over-approximation.md`). The
`generate for` genvar is the opposite: it is unconditionally demoted to a runtime induction value
and the body is lowered once, never per iteration. That demotion is sound only for a genvar that
shapes neither static representation nor structure a shared body cannot carry; applied
unconditionally it is unsound.

### F3. What an elaboration-time value can determine splits into two kinds

The distinction is load-bearing, because a single shared (generic) Build program has exactly one
static shape but can carry runtime construction control flow.

- **Static representation (a shared body cannot carry differences here).** Packed / unpacked type
  shape, width, or range; struct / enum representation; member layout; port ABI; a child's
  specialization key. `logic [i:0] tmp` and `Child #(.W(i))` produce genuinely different static
  representations per value.
- **Constructor-time structure (a shared body can carry differences here).** Which children or
  scopes are constructed and how many; which `generate if` / `case` arm is taken; a generate scope's
  index and hierarchy identity. A runtime Build program expresses these with runtime `for` / `if` /
  `case`, so differences of this kind do not by themselves require distinct bodies.

## Decision

1. **Correctness baseline -- explode.** A generate variable is a specialization input. Each genvar
   value produces its own concrete **iteration-specialized generated scope** (for example `g[3]`),
   lowered directly from the frontend's already-elaborated entry, exactly as a module instantiated
   with distinct parameter bindings produces distinct specializations. This may compile many copies;
   that is correct. An iteration-specialized generated scope is intra-unit -- a generated scope in
   the same compilation unit -- not a module specialization, and its concrete body does not enter
   any cross-unit specialization key.

2. **Compile-performance optimization -- demote.** An elaboration-time input -- a parameter or a
   generate variable -- may be demoted to a runtime constructor input, so one shared body serves
   many values, **only when a single Lyra-supported generic Build program reproduces every concrete
   entry's behavior**: the entries share one static representation (F3, first kind is uniform across
   the domain) and their differences are constructor-time structure (F3, second kind) that the Build
   program expresses with runtime `for` / `if` / `case`. The runtime value then controls
   construction, never static representation. This is the same optimization
   `parameter-code-shape-over-approximation.md` defers for parameters; the generate variable rides
   the same mechanism (a specialization key when exploded, a constructor input when demoted).

3. **The admission criterion is expressibility, not identity.** The question is not "are all
   concrete bodies identical" but "does one generic Build program reproduce them all." A
   `generate if` / `case` whose arms differ, or a loop whose count varies, is demotable when the
   Build program can express that control flow. Only a static-representation difference (F3, first
   kind) forces `ConcreteRequired`; a constructor-time-structure difference (F3, second kind) is
   `LiftableSharedTemplate` when the Build program supports the needed control flow.

4. **Conservative default.** Absent that proof -- including any static-representation difference and
   any control flow the Build program does not yet support -- the input stays a specialization key
   (explode).

Concrete elaboration is the floor beneath the demote optimization; demote is a proof-gated
compression of concrete entries into one generic Build program, never a substitute for having a
correct concrete form.

## Consequences

- The inactive-front-arm / witness problem dissolves. Each exploded entry is lowered at its own
  concrete genvar value with its own valid, concretely-typed body; there is no shared body to
  reconcile and no witness to select. (Re-symbolizing a concrete arm into one shared runtime loop is
  itself a demote, not the correctness baseline.)
- The exploded, per-block concrete scopes are the **sole** generate lowering representation today:
  every `if` / `case` / `for` construct lowers to one unconditionally-constructed scope per
  instantiated block. The demoted shared representation is not materialized; implementing demote
  reintroduces a shared body plus runtime construction control flow, produced only for a construct
  the classifier proves representation-invariant.
- The demotion proof is one classifier over elaboration-time inputs (parameters and generate
  variables together), with demote as the shared deferred optimization -- not a genvar special case.
- Cross-unit identity stays clean: an iteration-specialized generated scope does not enter a
  specialization key; only a real child module's parameter bindings enter that child's
  specialization identity (`specialization-identity.md`).
- Aligns with `north_star.md` (correctness is independent of the optimization) and
  `specialization_model.md` (concrete elaboration is the baseline; demotion is the optimization;
  classification is an eligibility filter, never a correctness precondition).

## Alternatives considered

- **Witness-lift into one shared body** (re-symbolize a concrete iteration's genvar). Rejected as
  the baseline: it is already a demote (one runtime loop plus one runtime `if`, with arms borrowed
  from different iterations), and it cannot preserve per-iteration static representation, so it
  produces wrong values when the genvar shapes representation.
- **"All concrete bodies identical" as the demote condition.** Rejected as too strict: it would
  force explode for a `generate if` / `case` or a variable-count loop that a generic Build program
  expresses with runtime control flow, discarding the runtime structural `for` / `if` / `case`
  capability that is the point of demoting.
- **Always demote the genvar (the current behavior).** Rejected as the baseline: it is the
  optimization applied without proof, unsound when the genvar shapes static representation.
- **Treating every representation fact as a demote blocker.** Rejected: only static-representation
  differences (F3, first kind) force explode; constructor-time-structure differences (F3, second
  kind) are expressible by a generic Build program and do not.
