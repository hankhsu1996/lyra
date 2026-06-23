# Performance

Tracks performance work that is intentionally deferred behind functionality. An item here is a known
optimization with a defined target shape, not a correctness gap: the feature already behaves
correctly without it, and the item only makes the same behavior cheaper. Split into two domains that
have different cost models and different measurement methods.

## Runtime performance

Simulation execution speed: the cost of running processes, scheduling events, and reading and
writing state once the object graph is built.

Re-establishing runtime performance tracking on the current architecture is pending the execution
backends and the benchmark CI jobs that drive it (`architecture-reset.md` R6). No concrete item is
open here until that lands.

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

### Open questions

- How thin the specialization key goes. The fat-value runtime representation carries packed width
  and unpacked size as runtime fields rather than as distinct types
  (`decisions/integral-representation.md`, `decisions/unpacked-array-representation.md`), so for the
  C++ backend a width or size parameter does not change generated code and could be a constructor
  input. A width-templated backend (the future LLVM `iN` lowering) does specialize on width. Whether
  per-width specialization is a key axis or a backend-internal monomorphization downstream of one
  width-generic artifact is unresolved, and it conflicts with `specialization_model.md` invariant 3
  as written (which lists packed width as code-shape-affecting). Resolve before keying on width.
