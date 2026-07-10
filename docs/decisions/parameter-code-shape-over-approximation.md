# Treat Every Parameter as Code-Shape-Affecting for Now

## Date

2026-06-23

## Status

Accepted

## Why this decision matters

`specialization_model.md` classifies every parameter into one of two axes: a **code-shape-affecting
input** that enters the specialization key (a packed width, a `parameter type`), or a
**constructor/config input** that flows in at runtime construction and does not fork the artifact
(an initial value, a count that only steers how many children are built). The canonical example is
`parameter int N` controlling a `generate for`: "the unit compiles once for any N."

Realizing that classification -- deciding per parameter which axis it sits on, and threading the
constructor-input axis as a runtime parameter so one artifact serves every value -- is a significant
piece of work. This record captures the deliberate choice to defer it and instead **conservatively
over-approximate the specialization key: treat every parameter as code-shape-affecting**, resolving
all parameter values at compile time. The current code already behaves this way; this record makes
it a considered decision, not an unfinished state, so a reader who sees a folded parameter count
knows it is intended.

## Findings that shaped the design

### F1. No MIR-level constructor-input vehicle exists today

The architecture's target shape -- a value that flows into the constructor rather than being baked
-- is not present in MIR. `generate for` folds the loop variable per arm and constructs each arm's
own concrete scalar child directly, so the arm body reads the genvar as a compile-time constant. The
generate loop bound is still an expression evaluated in the constructor (`hir::LoopGenerate` holds
it as an `ExprId`), but no structural parameter is threaded onto the arm scope for later runtime
read. The infrastructure to thread a constructor-input value (a scope-level runtime parameter the
constructor body reads as a runtime value) has not been built yet, so no existing vehicle stands
ready for module parameters to reuse when the classification lands.

### F2. Over-approximating the specialization key is sound, not a hack

Treating every parameter as code-shape-affecting is a conservative over-approximation of the key.
`specialization_model.md` invariant 3 defines code-shape inputs as "those that change generated
code"; assuming every parameter changes generated code is a safe over-approximation:

- It is always **correct**. Baking a known parameter value never changes simulation results.
- It stays **per specialization, never per instance**. The forbidden shape (`north_star.md`) is
  compile-time work that scales with instance count. Folding a declared parameter value makes
  compile-time scale with the count of distinct parameter-value combinations -- the specialization
  axis, which is allowed -- not with instance count.
- It does not unroll. `generate for` and an instance array both lower to a constructor-side loop
  with a constant bound; they loop, they do not expand. Compile-time work is O(1) in the iteration
  count and the element count even under full folding.

### F3. The accepted cost is unshared specializations, not incorrectness

Under this decision, two instantiations whose parameters differ only on the constructor-input axis
do not share an artifact. `Foo #(4)` and `Foo #(8)` compile as two specializations even where `N`
only steers construction. This is a deferred optimization -- compile-time sharing across parameter
values -- not a correctness or soundness gap.

### F4. The storage shape must stay forward-compatible

Instance multiplicity is the vector wrapper (`mir.md`): a runtime-sized member whose per-dimension
count flows as a value on the construct statement, with construction a constructor-side loop. This
shape is correct whether the count is a folded constant (today) or a parameter flowed in at
construction (after classification lands) -- it does not change. Baking the count into the type (a
fixed-extent array carrying its extent on the type layer, rendered `std::array<T, N>`) is rejected:
it is valid only while the count is a compile-time constant, and would have to be torn out when the
count becomes a constructor input. The over-approximation makes the count a constant today, but it
does **not** license moving the count into the type.

## Decision

Until the parameter classification and the constructor-input threading it enables are built, every
parameter is treated as code-shape-affecting. Parameter values are resolved at compile time. No
classification axis exists; the specialization key conservatively includes every parameter. This
folded shape is the intended state, not a transitional one.

This over-approximation is the concrete correctness baseline of `specialization_model.md`: every
distinct binding is its own concrete artifact, which is always correct. Parameter classification and
constructor-input threading are the deferred **sharing optimization** over that baseline -- the same
posture `generate-concrete-witness-lowering.md` takes for generate bodies (concrete witness first,
proven sharing second).

Two constraints bind going forward, so that deferring the optimization does not dig a hole:

1. **Storage stays forward-compatible.** Instance multiplicity is the vector wrapper with the count
   as a constructor-side value. It is never a fixed-extent array type carrying the count in the
   type. (This is why the proposed "store an instance array as a fixed-size array, extent in the
   type" refactor was removed rather than implemented: it is forward-incompatible with the
   constructor-input end state and contradicts the vector-wrapper invariant.)

2. **Do not widen elaborated-graph consumption.** The instance-array count is currently sourced from
   slang's elaborated element count rather than the declared dimension expression. This is tolerated
   only because every parameter is folded anyway; it is the least forward-compatible point and the
   first thing to revisit when constructor-input threading is built. It must not be generalized into
   reading more of the elaborated instance graph (expanded per-instance values, materialized
   sub-instances) as compile-time facts -- that is the `north_star.md` / `compilation_unit_model.md`
   forbidden shape "the frontend-elaborated instance graph drives compile-time work."

## Consequences

- No classification tool and no module-parameter-as-constructor-input threading are built now. The
  future work builds the constructor-input vehicle from scratch: a scope-level runtime parameter
  that the constructor body reads as a runtime value, with `generate for` and module parameters
  wired onto it together.
- When compile-time sharing across parameter values becomes a turnaround-budget problem, the
  deferred work is: classify parameters per `specialization_model.md` invariants 2-4; thread the
  constructor-input axis as runtime parameters by extending the genvar vehicle to module parameters;
  and source counts from declared dimension expressions instead of the elaborated element count.
- This decision lives inside the space `specialization_model.md` leaves open -- it over-approximates
  the key, which the model permits as conservative -- and overrides no `north_star.md` invariant.
