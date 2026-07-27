# The value-projection designator: MIR's formal write target and projection reference

## Date

2026-07-24

## Status

Accepted

## Why this decision matters

[value-projection-write](value-projection-write.md) settled the model: a write into a value
aggregate's interior is an owner-relative functional whole-value update, not a place store. It left
the representation to the last of its staged cuts -- "the canonical decomposition is promoted into
the formal MIR write-designator node, and the old nested-lvalue write encoding is deleted".

That cut is the one that pays the debt. Until it lands, MIR still encodes an interior write as a
nested lvalue expression, and both backends recover the owner and the selectors by walking that
chain and consulting each receiver's type kind. Two backends deriving one semantic fact is the
`mir.md` Forbidden Shape the model exists to remove; the walking is also what makes each new
container family arrive as another per-type branch.

This entry fixes the representation: what the node is, what a selector is, how a deferred write and
a `ref` share it, and what is deleted. It answers the questions the model deliberately left open, so
the migration that follows is mechanical.

## What the model already fixed

These are not reopened. The designator is designed to satisfy them.

- The owner boundary is structural, never a classification flag over a retained nested lvalue, and
  never recovered by walking (D1, and its first rejected alternative).
- Owner and selectors are structural children; a field that restates what the structure already
  fixes is forbidden (D1's third rejected alternative, and `mir.md`'s node-field rule).
- The owner and every selector evaluate exactly once, as a stated MIR semantic. Temporaries and the
  read / update / store sequence belong to MIR-to-LIR (D2, `lowering_boundaries.md`).
- The C++ in-place write is a behavior-preserving optimization admitted only against an equivalent
  proxy; MIR never asserts a value interior is addressable (D3).
- A `ref` bound to an interior is an owner-relative projection reference, never an interior pointer
  and never an addressable sub-place (D4).
- The read side keeps its value semantics: a slice read materializes an owned value (D5).

## The decision

### D1. The designator is one expression node whose children are the owner and the selector path

A write target is either a place expression, whose write is a store, or a `ValueProjection` node,
whose write is a functional whole-value update. The node has exactly two children: an owner
expression, which must be a place, and a non-empty selector path in owner-to-leaf order. The node's
type is the type of the part it designates.

The node kind is the classification. There is no place-versus-projection flag, no marker on the
assignment, and no payload restating what the children already say. A backend reads the boundary by
reading which node is at the target position; it never inspects a receiver's type kind to classify a
step, and it never walks a chain to find where the place prefix ends.

`AssignExpr.target` and `IncDecExpr.target` accept either form. Nothing else about those nodes
changes: the single compound-assignment shape `{target, compound_op, value}` stands
([compound-assignment-write-location](compound-assignment-write-location.md)).

### D2. The selector set is closed, coordinate-facing, and realized per value domain

A selector names one descent step into a value. The set is:

- **Component** -- a positional part of a product value (an unpacked struct's field).
- **UnionMember** -- a positional member of a union value. Its update makes the member active; the
  active-member value model is unchanged
  ([unpacked-union-representation](unpacked-union-representation.md)).
- **Element** -- one coordinate into a homogeneous or keyed value: an unpacked-array, dynamic-array,
  or queue element, an associative-array key, a string character, a packed bit-select.
- **Slice** -- a fixed-width window: a packed part-select, an unpacked slice, and a packed
  aggregate's member, which projects to a constant-bounds slice over the base
  ([packed-array-representation](packed-array-representation.md)).

One selector kind spans several value domains where the descent is the same shape; which runtime
operation realizes it follows from the type of the value it descends into, through the backend's
ordinary type-mapping dispatch. That is the same dispatch every value operation already uses; it is
not a decision in value emission.

Every coordinate is the source-level one. No selector carries a rebased position, a storage offset,
or a resolved index ([selector-coordinate-resolution](selector-coordinate-resolution.md)). A
selector whose value domain takes its declared range from its static type carries that range as its
own operand, materialized at HIR-to-MIR from the statically known type at that position in the path
([unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md)). Every LRM corner case --
out-of-range write discard, an x or z coordinate collapsing the write to a no-op, slice window
normalization, string `putc` rules, queue clamping and append, associative auto-allocation -- stays
inside the runtime operation that realizes the selector.

### D3. The path never crosses a dereference; reaching the owner is an ordinary read

A designator's owner is a place, and its path descends only through value parts. Where a chain
re-enters storage -- a class handle held inside a value aggregate, as in `s.h.f = x` -- the
dereference terminates the path: the owner is the dereferenced referent, and whatever projection was
needed to read the handle out of `s` appears inside the owner expression as an ordinary read. One
write is one designator; a chain is never split into several.

This is what makes the owner boundary well-defined without a rule about re-entering place-land: the
path is the value-aggregate descent, and a dereference is not one.

### D4. A projection reference is the designator's evaluated form, and it serves every deferred write

Evaluating a designator without writing it yields a projection reference: a reference to the owner's
storage plus the already-evaluated coordinates of its path. Reads go read-owner then select; writes
go read-owner, update the selected part, write-owner. It is never an interior pointer, and never a
LIR indexed place.

Three constructs need exactly this, and all three take it:

- A `ref` / `const ref` actual bound to an interior. It stays a live alias: a write lands in the
  owner immediately, not at completion ([unified-callable-model](unified-callable-model.md)).
- An `output` / `inout` actual that is an interior. The actual is bound once at call entry and
  written back after completion; binding once is what the projection reference provides.
- A nonblocking assignment into an interior. Owner and coordinates are evaluated where the statement
  executes; the update and the single writeback run in the update region.

The formal's type is unchanged -- a `ref` formal is a reference-typed parameter
([reference-as-data-type](reference-as-data-type.md)). What is new is that the actual may be a
designator, and that the reference a designator produces carries selector state. A `ref` port still
binds to a whole variable and seals to a direct cell; ports are not projections.

A projection reference holds a reference to storage, never a transient value handle, so it never
installs a per-stretch transient into longer-lived storage
([activation-frame-and-transient-scope](activation-frame-and-transient-scope.md)). Its coordinates
are values; one that outlives the stretch that built it is promoted with the reference.

### D5. The write side is a designator and the read side stays a select chain, because they are different operations

A read composes bottom-up: each step is a total function from a value to a part of it, so a nested
select chain says everything, and both backends already realize it identically. A write cannot
compose that way. The innermost step must know where the result goes, and a bottom-up value
composition never names the root place. That is why the write side needs the owner stated and the
read side does not.

This is the same asymmetry `lir.md` draws between reading a component and writing one, and it is not
duplicated structure: the read chain and the designator never describe the same access. A statement
that both reads and writes an interior -- a compound assignment -- has one designator, not a read
chain beside it.

### D6. Compound assignment and increment read through the same owner read they write back

`target op= value` reads the owner's whole value once, extracts along the path, applies the
operator, inserts along the path, and stores the whole value back. There is one owner read and one
writeback, whatever the path's depth. Increment and decrement follow the same shape. The evaluate-
once obligation of the model is discharged here rather than restated per target family, which is
what retires the per-target evaluate-once reimplementation the earlier decision was written to stop.

### D7. A projection write into an observable owner is one semantic store through the cell

Whatever the path's depth, the writeback is a single store through the owner's own write protocol --
for an observable owner, the cell's write, firing subscribers once
([value-type-concepts](value-type-concepts.md)). It is a semantic store, so it conforms the value to
the destination's declared type ([value-store-discipline](value-store-discipline.md)). Reactivity
stays whole-cell; field-granular reactivity is out of scope
([unpacked-struct-representation](unpacked-struct-representation.md)).

### D8. The C++ in-place recovery is uniform, or it is not taken

A backend may realize a projection write in place only against a proxy equivalent to the functional
update. The equivalence is not judged per call site: a value domain either has an equivalent proxy
for a selector kind or it does not, and the answer is a property of the type mapping, not a branch
in value emission. A backend with a partial proxy set realizes the whole family functionally rather
than branching between the two shapes.

Equivalence is established by the same backend-agreement tests the migration already uses: a source
that exercises the family's corner cases, run on both backends, output compared. A new proxy without
such a case is not admitted.

### D9. What is deleted

- The nested-lvalue write encoding: the write-side `Ref`-suffixed access forms (`element_ref`,
  `slice_ref`, and the union write form) disappear from MIR, along with the read-versus-write choice
  a lowering flag makes at a select.
- The MIR-to-LIR target decomposition and its per-type-kind predicate, which exist only to recover
  what the designator now states.
- The functional-write primitives that exist solely for the execution backend, once the designator
  makes the functional update a stated MIR fact both backends realize.
- `AddressOfExpr` over a value interior. Its operand must be an addressable place, which a
  projection is not; the verifier rejects it ([address-of-primitive](address-of-primitive.md)).

### D10. Well-formedness is established at construction, and violated forms fail loudly

A designator is only ever built by the lowering that peels a write target, and that lowering cannot
build an ill-formed one: it appends exactly one step per descent, so the path is never empty; the
owner is whatever the recursion reached that was not itself a descent, so it is a place; and each
step states the type it projects, so the chain types by construction.

The forms that would be ill-formed are rejected where they would be consumed rather than silently
accepted: a designator under an address-of names no place, and an address-of requires an addressable
place ([address-of-primitive](address-of-primitive.md)). There is no MIR verifier today -- LIR has
one and MIR does not -- so these are the checks that exist; when a MIR verifier exists, they are the
rules it states.

## Decisions this reverses

Each is reversed only for the write side of a value interior, and only in mechanism.

- [unpacked-union-representation](unpacked-union-representation.md) D4 -- "There is no
  union-specific reference concept: the write form is an ordinary reference into the active member's
  storage" -- and D5's composition of a nested member write on that reference. A union member
  becomes a selector; a nested write is a two-step path, not composition on a reference. The
  active-member value model, member activation as part of the write, and the inactive-read default
  are unchanged.
- [unpacked-array-representation](unpacked-array-representation.md) invariant 2 -- the write-side
  slice proxy "whose destructor scatters back into the receiver's storage", and the out-of-range
  reference to a shield slot. Both become realizations admitted under D8, not the model; the
  out-of-range rule is the functional update's no-op.
- [queue-operators](queue-operators.md) D2 -- "the assignment lowering marks its left-hand side as a
  write target, and the queue element-select picks the write-side method under that flag". The flag
  and the write-side method selection go; a queue element is a selector like any other. That an
  access is a call rather than a select node, and that the store boundary conforms the element
  shape, are unchanged.
- [compound-assignment-write-location](compound-assignment-write-location.md) and
  [slice-value-semantics](slice-value-semantics.md) were already reframed for value interiors by
  [value-projection-write](value-projection-write.md); this entry is where their write-side
  mechanism is actually removed.

## Rejected alternatives

- **Self-classifying access nodes in a retained nested chain.** Split each access primitive into a
  place form and a value-projection form so a backend can classify each step by node kind alone, and
  keep the chain. Rejected: it is the model's own first rejected alternative. Reading structure is
  allowed, but the owner boundary would still be recovered by each backend walking a chain, and two
  backends can walk it to different results. The boundary is stated, not found.
- **One designator for reads and writes.** Rejected: it would rewrite every read path to gain
  nothing. A read chain composes bottom-up and is already realized identically by both backends;
  there is no fact either backend re-derives. D5 gives the standing justification for the asymmetry.
- **A generic multi-level update entry in the runtime.** Rejected: the per-domain functional update
  entries already exist and compose one call per level, which is the accepted erased-aggregate cost
  ([jit-aggregate-realization](jit-aggregate-realization.md)). A generic entry would need to
  interpret a selector path at runtime -- a second selector vocabulary below LIR.
- **A distinct MIR reference type for a projection reference.** Rejected: the formal's type already
  states reference-ness, and a second reference type would split one concept by how its referent is
  reached. What a projection reference carries beyond a cell reference is its realization, below
  MIR.

## Consequences

- Every interior write -- struct component, union member, packed slice, packed or unpacked element,
  queue and associative element, string character -- is one node kind with one realization rule per
  backend. A new container family adds a value domain and its runtime entries, never a branch at the
  write path.
- `ref`, `output`, `inout`, and nonblocking assignment into an interior become expressible on the
  execution backend through one mechanism rather than four.
- Both backends lose their target-classification code; the execution backend's per-type-kind
  predicate and the C++ backend's write-side access forms disappear together.
- A deep path on a large aggregate costs one owner read, one update call per level, and one
  writeback. No measurement exists for this in the repository today; the optimization paths are the
  C++ in-place recovery of D8 and, on the execution backend, the deferred in-frame value layout
  ([jit-value-realization](jit-value-realization.md)), neither of which changes the model.

## Correction to an architecture document

`lir.md` states the place vocabulary as "a base local plus a projection chain (member, index,
dereference, slice, downcast)" and, in its invariant on logical storage topology, as "member, index,
and dereference steps". Neither matches the implementation, which has member and dereference only,
nor the model, under which a value interior is not an addressable sub-place. An array of storage --
owned children, for instance -- is reached as a member yielding a vector, an element yielding an
owning pointer, and a dereference of that pointer, so the place-ness comes from the indirection and
no index arm is required. The document is corrected to member and dereference as part of this work.
