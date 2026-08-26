# Performance

Tracks performance work that is intentionally deferred behind functionality. An item here is a known
optimization with a defined target shape, not a correctness gap: the feature already behaves
correctly without it, and the item only makes the same behavior cheaper. Split into two domains that
have different cost models and different measurement methods.

## Runtime performance

Simulation execution speed: the cost of running processes, scheduling events, and reading and
writing state once the object graph is built.

How hard a program is compiled is settled and is not a tracked gap. The runtime library an emitted
program links is optimized as shipped, independent of how the compiler that ships it was built,
because a user recompiles none of it. The design's own translation unit is compiled unoptimized by
default and optimized under `--release`, which is the one place the build-time / run-time trade is a
choice: iterating pays the compile on every edit, while a long run earns it back.

Re-establishing runtime performance _tracking_ -- a benchmark that runs in CI and a number that
regressions move -- is pending the execution backend and the benchmark CI jobs that drive it
(`execution-backend.md`). No concrete item is open here until that lands.

## Construction / compile-time performance

The cost of producing compiled artifacts and of building the object graph at time zero. The primary
constraint is `north_star.md`: compile-time work scales with the number of distinct unit
specializations, not with instance count.

- [ ] Specialization dedup. Today every distinct parameter binding produces its own compiled
      artifact, because unit identity is keyed on the frontend's per-parameter-set elaboration. This
      over-forks: two instances whose generated code is identical except for a folded constant
      compile twice. The target is `specialization_model.md`: classify each parameter as a
      code-shape-affecting input (enters the specialization key) or a constructor/config input
      (flows in at construction), emit one artifact per distinct code shape, and let value-only
      parameters differ per instance without forking the artifact (LRM 23.10). Functionality does
      not depend on this -- a correctly-identified per-binding artifact already behaves correctly
      (see `hierarchy.md` Stage A); dedup only reduces how many artifacts exist. The identity
      mechanism is unchanged from the functional case: the same canonical binding serializer
      (`docs/decisions/specialization-identity.md`) is fed only the code-shape-affecting subset, and
      value-only parameters are demoted to constructor inputs.

- [ ] The generate axis of that same sharing. A `generate for` lowers concretely: N iterations
      become N scope classes and N construction statements, so the artifact grows one-for-one with
      the iteration count even when no parameter varies and every body is identical. Measured over
      16, 64, and 256 iterations of one trivial child, emitted lines and generated classes both
      scale linearly with N. This is the concrete baseline `specialization_model.md` invariant 1
      calls correct, in its simplest form; the optimization is recognizing that iterations whose
      generated code is identical can share one class carrying a loop, which is the shape
      `runtime_model.md` describes. It needs a construction-time iteration vehicle, the same thing
      the parameter classification above needs, so the two are naturally taken together.

      `runtime_model.md` states this one as an absolute ("if you find compile-time work that scales
      with instance count, the design has been violated") where `specialization_model.md` invariant
      1 admits the concrete form as correct and scopes the requirement to the optimized steady
      state. The two should be reconciled when this is picked up, with the specialization model
      governing.

### Open questions

- How thin the specialization key goes. The fat-value runtime representation carries packed width
  and unpacked size as runtime fields rather than as distinct types
  (`decisions/integral-representation.md`, `decisions/unpacked-array-representation.md`), so for the
  C++ backend a width or size parameter does not change generated code and could be a constructor
  input. A width-templated backend (the future LLVM `iN` lowering) does specialize on width. Whether
  per-width specialization is a key axis or a backend-internal monomorphization downstream of one
  width-generic artifact is unresolved, and it conflicts with `specialization_model.md` invariant 3
  as written (which lists packed width as code-shape-affecting). Resolve before keying on width.
