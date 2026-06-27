# Packed Array Representation

## Date

2026-06-26

## Status

Accepted

## Why this decision matters

HIR represented a packed array as a flat node: a scalar `atom` (bit / logic / reg) plus a vector of
dimensions (`PackedArrayType { atom, signedness, dims, form }`). A bare scalar was the degenerate
case -- a packed array with a single `[0:0]` dimension. This shape has three faults:

- It cannot represent a packed array whose element is a packed aggregate. `byte_t [1:0]` (an array
  of an 8-bit packed struct) has no scalar atom; the flat node can only fake it by treating the
  struct's bits as a synthetic dimension and deriving the atom from the element's 4-state-ness, with
  the element's type identity recovered later from slang's per-expression types rather than carried
  in the type.
- It flattens, inside HIR, two things that are HIR-faithful only when kept structured: the dimension
  nest (a multi-dim packed array is `array of arrays` of bits in slang and the LRM) and the scalar
  (slang keeps `ScalarType` as a distinct leaf; HIR collapsed it into a degenerate array). `hir.md`
  invariant 3 reserves flattening into lower-level primitives for MIR.
- It makes the packed array the only array family whose element is not a `TypeId`. Unpacked,
  dynamic, queue, and associative arrays all reference their element by `TypeId`. The flat packed
  node breaks that symmetry and, with it, the homogeneous interned type graph the stable-identity
  architecture depends on.

## Findings that shaped the design

### F1. The LRM and slang define a packed array recursively

`LRM 7.4.1`: a packed array's element may be a single-bit type, an enumerated type, "and recursively
other packed arrays and packed structures." A packed array "appears as a primary [and] is treated as
a single vector." slang mirrors this: a `PackedArrayType` carries one `range` and an `elementType`
that nests one level per declared dimension, bottoming out at a `ScalarType`, an `EnumType`, or a
packed `Struct` / `Union`. There is no flat-dimensions accessor; the nesting is the model.

**Consequence:** the LRM-faithful HIR shape is single-dim per type node, nested via the element
type, with the element named by its own type identity -- the same shape already adopted for unpacked
arrays.

### F2. HIR is required to preserve LRM structure, not flatten it

`hir.md` invariant 3: HIR preserves LRM-level constructs without flattening them into lower-level
primitives; flattening into a generic vocabulary is MIR's job. The flat `(atom, dims)` node does two
flattenings inside HIR: it collapses the dimension nest onto one node, and it collapses the scalar
leaf into a degenerate array. Both are MIR-level projections performed one layer too early.

**Consequence:** the flatten belongs at HIR-to-MIR, not at AST-to-HIR. HIR carries the recursive
shape; the translation to a flat vector is the next layer's work.

### F3. The stable-identity architecture wants one homogeneous interned type graph

`north_star.md` invariant 3 makes incremental and parallel compilation first-class.
`incremental_build.md` expresses compilation as a memoized query graph keyed by ownership-based
stable keys and semantic fingerprints, and forbids raw pointer identity and global sequential ids as
keys. `identity_and_ownership.md` requires identity to be self-sufficient for resolution: given an
id, a consumer resolves the target with no side table.

A type graph that satisfies this is homogeneous: every node is an interned entity with a stable
`TypeId`, and every reference between nodes -- including a packed array's element and the scalar
terminal -- is a `TypeId`. A fingerprint then recurses uniformly over `TypeId` edges, and equal
sub-types are structurally shared (the scalar `logic` is interned once and shared by every type that
bottoms at it).

**Consequence:** the scalar terminal must be a first-class interned leaf type with its own `TypeId`,
not an inline payload embedded as a graph edge. A representation where the element edge is sometimes
a `TypeId` and sometimes an inline atom is heterogeneous and leaves the scalar without a stable
identity.

### F4. The flat vector belongs to the value layer, produced by lowering

`integral-representation.md` fixes the runtime / `backend::cpp` value as one flat
`lyra::value::PackedArray` constructed from `(bit_width, is_signed, is_four_state)`, and keeps MIR's
`PackedArrayType` as the flat `(atom, signedness, dims, form)` dispatch axis. That decision is about
the value and MIR layers and stays unchanged. The flat shape is produced by flattening the recursive
HIR type at HIR-to-MIR.

**Consequence:** HIR and MIR carry different packed-array shapes -- recursive at HIR (faithful),
flat at MIR (the value substrate). This is the same per-layer asymmetry as a loop at HIR becoming a
callable at MIR. The substrate asymmetry that makes unpacked arrays recursive at both layers does
not apply: a packed array's runtime value is a flat bit plane, so MIR stays flat while HIR is
recursive.

## The decision

Packed-type representation follows these invariants:

1. **HIR has a scalar bit leaf type.** A single bit is `ScalarBitType { atom }`, interned with its
   own `TypeId`. A bare scalar (`logic`, `bit`, `reg`) is this leaf; so is the terminal of any
   packed array. The scalar is a first-class type entity, not an inline payload.

2. **HIR represents a packed array with a single dim per node, element by `TypeId`.**

   ```text
   hir::ScalarBitType  { atom: BitAtom }
   hir::PackedArrayType { dim: PackedRange, element_type: TypeId, signedness, form }
   ```

   `logic [3:0]` is a `PackedArrayType` whose `element_type` is the interned `logic` scalar.
   `logic [3:0][7:0]` is an outer node whose element resolves to an inner `PackedArrayType`.
   `byte_t [1:0]` is a node whose `element_type` is the `byte_t` packed struct. A predefined-width
   integer (`int`, `byte`) is a single-dim packed array over the scalar bit, with `form` recording
   the syntactic origin. AST-to-HIR consumes slang's nested type 1:1 by recursing through
   `InternType` on the element; there is no dimension-flattening walk.

3. **Derived integral properties are computed against the type pool, not stored.** Bit width and
   4-state-ness of a packed array resolve `element_type` through the pool (one bit for a scalar
   leaf, the element's own width times the dim element count for an array). They are not cached on
   the node. Signedness is a per-node field; the outermost node is authoritative (LRM 7.4.1:
   elements are unsigned unless of a named signed type).

4. **MIR stays flat; HIR-to-MIR flattens.** `mir::PackedArrayType { atom, signedness, dims, form }`
   is unchanged (`integral-representation.md`). HIR-to-MIR projects the recursive HIR type onto it:
   a scalar leaf becomes a one-bit flat vector; a packed array becomes the element's flat projection
   with this node's dim prepended; a packed aggregate element becomes its single-vector projection.
   The dimension-flattening that used to live in AST-to-HIR lives here, where projecting SV
   structure into a generic vocabulary belongs.

5. **A packed aggregate's single-vector projection is a packed array over the scalar bit.** A packed
   struct's or union's `base` is `PackedArrayType { dim: [width-1:0], element_type: <scalar bit> }`.
   Its field / member table is unchanged; member access still projects to a constant-bounds slice
   over the base.

## Consequences

- A packed array whose element is a packed struct, union, or enum is represented faithfully -- the
  element is named by its `TypeId`. Field and member access through an element select compose
  naturally: the element select yields the aggregate type, whose field table drives the inner slice.
- The AST-to-HIR dimension-flattening walk and its scalar-versus-aggregate branch are deleted; the
  packed-array case recurses through `InternType` on the element, one node per declared dimension.
- The packed array joins the other array families: its element is a `TypeId`, the type graph is
  homogeneous, every node is interned, and the scalar bit is a stably identified entity shared
  across every type that bottoms at it.
- Bit width and 4-state-ness of a packed array become pool-relative queries. Their consumers thread
  the type pool, the same way a layout query takes the type context.
- MIR, the value layer, and the backend are unchanged: HIR-to-MIR still produces the same flat
  `mir::PackedArrayType`.

## Alternatives considered

**Element as `variant<BitAtom, TypeId>` with no scalar leaf type.** The packed array would carry its
element as either an inline scalar atom or a `TypeId`. Rejected. The element edge is heterogeneous:
sometimes an interned entity, sometimes an inline payload that is not an entity and has no `TypeId`.
The scalar then has no stable identity, contradicting the homogeneous interned type graph the
stable-identity architecture requires (`incremental_build.md`, `identity_and_ownership.md`), and the
packed array stays the only array family whose element is not a uniform `TypeId`. It is less
invasive only because it leaves the existing "scalar as a degenerate packed array" flatten in place
-- the very flatten this decision removes.

**Flat dimensions at HIR (status quo).** A single node carrying a scalar atom and a vector of
dimensions. Rejected. It flattens the dimension nest and the scalar leaf into MIR's vocabulary
inside HIR, violating `hir.md` invariant 3, and it cannot name a non-scalar element type at all, so
a packed array of a packed aggregate is unrepresentable except by faking a synthetic dimension and
recovering the element type from slang's per-expression types.

## Cross-references

- [integral-representation](integral-representation.md) -- the MIR / value-layer flat packed shape,
  unchanged; this decision flattens the recursive HIR type onto it at HIR-to-MIR.
- [unpacked-array-representation](unpacked-array-representation.md) -- the recursive single-dim
  element-by-`TypeId` shape this aligns the packed family with; the packed / unpacked substrate
  asymmetry now lives at the MIR / value layer, not at HIR.
- [mir-type-interning](mir-type-interning.md) -- the canonical-`TypeId` interner this homogeneous
  type graph feeds.
- `architecture/hir.md` (invariant 3: HIR preserves LRM structure; flattening is MIR's job).
- `architecture/incremental_build.md`, `architecture/identity_and_ownership.md`,
  `architecture/north_star.md` (the stable-identity / interned-type-graph rationale).
