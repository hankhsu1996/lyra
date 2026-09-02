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

Runtime performance is tracked by the benchmark over the corpus under `tests/benchmark/`, which
builds every case under both Lyra and Verilator and puts the two rates side by side. It runs nightly
and gates its own run on a case building, running, and finishing inside a time limit -- never on a
rate itself, which would be a claim about the machine that ran it.

What a case reports is a **rate**, and no amount of work is written down anywhere: the harness
raises the amount until a measurement reaches a target duration, separately for each tool, and
divides it back out. So a case is never sized by hand, an engine that gets a thousand times faster
is followed rather than re-tuned around, and a reference simulator hundreds of times quicker is
measured over hundreds of times the work instead of over a millisecond of process startup.
`../decisions/benchmark-case-shape.md` carries why.

Two things still decide how a number is read. Every case that times a simulation is built optimized,
because the default unoptimized translation unit measures the build-time trade rather than the
simulation -- 4.5x on the NBA-heavy case, so an unoptimized reading is not a slower version of the
same answer. And the Verilator column is **not** a like-for-like comparison: there is no two-state
mode, so every factor carries whatever X/Z tracking costs.

Read that way, most cost families put Lyra between 270x and 4,700x Verilator's rate, with the
narrowest a subscription scan that fires nothing at 15x. Two sit nowhere near that band. Wide
bitwise work on a 256-bit value is **16,000x**, and writing an unpacked array element by element is
**three million times**, three orders of magnitude past anything else. A factor of a few hundred is
an engine that is slow everywhere; a factor of three million in one place is a defect, and the item
below names it.

The unpacked-array _read_ case cannot report a read rate while that defect stands, and the way it
fails is worth knowing. It fills the array before reading it, one element at a time, so its setup
runs the write path once over the whole array -- which costs more than a minute against a second for
each read pass that follows. Separating a fixed cost from a marginal one needs the marginal part to
clear the noise between two readings, and here it does not, so what the case reports is its own
setup. The number to expect once writes stop materializing the whole value is a few hundred, in line
with every other family: a read copies the element it selects and nothing else. The case becomes
truthful again then, with no change to the case.

A profile of the integration design says where the time goes there, and it is not where the
pre-reset engine spent it -- propagation does not appear at all. Two thirds of a run is a single
nonblocking write into the design's memory, because a partial write to a cell materializes the whole
value: the write copies the array, changes one element of the copy, compares the copy against the
stored value, and stores the copy back. A further quarter is spent constructing and range-checking
views over bit vectors, which every bit access pays whatever its storage.

- [x] An integral value's dimension stack no longer allocates for the single-dimension case. Every
      declared integral carried its stack in a growable container, so constructing or copying any
      value -- which generated code does at every operation, since the descriptor is built at the
      use site -- reached the allocator. Measured at 17% on the clocked-pipeline case and 26% on the
      NBA-heavy one, and 14% on a case that is scalar arithmetic with no arrays and no scheduling,
      which is what places the cost per value rather than per aggregate. The value grew from 144 to
      168 bytes in exchange, so a whole-array copy moves more; the trade was measured rather than
      assumed, and on a design whose arrays dominate it could go the other way.

- [ ] A partial write to a cell should not materialize the whole value. The snapshot exists so a
      partial write reuses the whole-variable commit path, with its LRM 4.3 change detection and
      subscriber wake, and for a packed value that is cheap and the edge classifier needs the old
      bits anyway. For an aggregate the premise fails: whether the value changed is answerable at
      the element being written. This is the largest single item here and it is a change to the
      observability contract, not an optimization behind it.

      Measured by writing each element of an unpacked array of 32-bit elements in turn: one element
      costs 1.55 ms into a 32768-element array and 72 us into a 2048-element one, against 1.16 us to
      read one element back. So the cost of a write tracks the size of the array it lands in rather
      than the size of the element, which is the signature of the copy. The larger figure is 5.5 MB
      of value moved in 1.55 ms, or 3.5 GB/s, which is that machine's memory bandwidth -- the write
      is bounded by copying the array and by nothing else.

      The array's length is therefore what the case measures, not how long it runs, and shrinking it
      to make the suite quick shrinks the defect along with it: at 2048 elements the same case
      reports a factor of a thousand against Verilator, in line with every other cost family, and
      the thing worth seeing disappears.

- [ ] The sequence type behind a value's words and dimensions holds its inline storage and its spill
      container at the same time, so a value that never spills still carries the spill container and
      copies it on every copy. Both of its uses want capacity one, which makes the general form a
      cost with no consumer.

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

- [ ] What remains of the instance array on that same axis. The member and the construction
      statement no longer scale: `Child c[0:N-1]` is one member whose type carries the multiplicity
      and one statement composing the whole sequence, so widening an array changes neither. Two
      things still do. The composition names every element, so the constructor holds N element
      expressions; reaching one loop needs a way to build a sequence from a count rather than from
      its elements, which the value model does not have -- a sequence is composed where it is built.
      And a port connection on an array is still distributed per element by the frontend, so its
      endpoint members and its implied continuous assignments stay one per element; collapsing those
      needs the peer to be a function of the index, which is exactly what the distribution spent.

### Open questions

- How thin the specialization key goes. The fat-value runtime representation carries packed width
  and unpacked size as runtime fields rather than as distinct types
  (`decisions/integral-representation.md`, `decisions/unpacked-array-representation.md`), so for the
  C++ backend a width or size parameter does not change generated code and could be a constructor
  input. A width-templated backend (the future LLVM `iN` lowering) does specialize on width. Whether
  per-width specialization is a key axis or a backend-internal monomorphization downstream of one
  width-generic artifact is unresolved, and it conflicts with `specialization_model.md` invariant 3
  as written (which lists packed width as code-shape-affecting). Resolve before keying on width.
