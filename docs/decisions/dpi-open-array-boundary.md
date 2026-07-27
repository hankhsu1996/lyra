# An open array crosses as a canonical boundary object, not a borrow of the actual

Date: 2026-07-27 Status: accepted

## Context

A DPI-C open array (LRM 35.5.6.1) is an imported subroutine's formal with one or more unsized
dimensions, written `[]`. It relaxes argument matching so one foreign function serves actuals of
different sizes. The C side receives a handle rather than a pointer and reaches the array through a
library surface: dimension queries, a whole-array pointer, element pointers, and element accessors
in canonical form (Annex H.12).

Every other DPI argument Lyra already carries crosses either as a machine scalar or as a canonical
buffer holding one packed value. An open array is the first argument whose extent is not fixed by
the declaration, and the first the foreign side navigates rather than reads whole. That raises two
questions this entry settles: what the C side is actually given, and where the formal's shape lives.

## Findings that shaped the design

### F1. The LRM specifies the representation, and exempts open arrays from C layout

Annex H.11.4 requires every unpacked array crossing the boundary to have C-compiler layout -- with
one exception:

> Unpacked arrays, with the exception of formal arguments specified as open arrays, shall have the
> same layout as used by a C compiler.

Annex H.7.3 then states what an open array's representation is instead:

> For a stand-alone array passed as an actual to an open array formal
>
> - If the element type is a 2- or 4-state scalar or packed type, then the representation is in
>   canonical form.
> - Otherwise, the representation is C compatible.

So the canonical form is not a workaround for a simulator's internal layout; it is what the standard
prescribes for an open array of integral elements. LRM 35.5.4 says the same thing from the other
side: `"DPI-C"` passes arguments in canonical format, where the deprecated `"DPI"` passed them in
actual simulator representation.

### F2. Lyra's value representation is never C layout

An integral value is one fat self-describing object carrying width, signedness, and state domain as
runtime fields ([integral-representation](integral-representation.md)), so an SV `byte` is not a C
`char`. An unpacked array is a vector of such elements, one container layer per declared dimension
([unpacked-array-representation](unpacked-array-representation.md)). On the execution backend every
aggregate is a runtime-owned opaque value
([jit-aggregate-realization](jit-aggregate-realization.md)).

No backend has C layout for an SV array. Combined with F1 this decides the whole family: the sized
unpacked array formal, which H.11.4 requires to have C layout, cannot be supported; the open array,
which H.11.4 exempts, can. Open arrays are the only unpacked-array DPI formal Lyra can serve, which
inverts the intuition that the unsized form is the harder one.

### F3. The pointer accessors are conditional, and the condition is a classification Lyra already has

Annex H.12.4 makes the whole-array and element pointers conditional:

> the above functions shall return NULL if the representation of the array elements differs from the
> representation of individual values of the same type.

An individual `byte` crosses as a C `char` (Table H.1) while a canonical element of a `byte` array
is a 32-bit group (H.7.7), so those differ and the pointer is NULL. An individual `bit [7:0]`
crosses by reference as `svBitVecVal*` -- canonical -- so those agree and the pointer is valid. The
dividing line is exactly whether the element's own DPI carrier is a canonical vector rather than a
by-value scalar, which is the type-shape classification (LRM 35.6.1.1 WYSIWYG) the boundary already
computes for every formal.

### F4. The formal's unsized shape is never the type of a value that crosses

LRM 35.6.1.1: "The unsized ranges of open arrays are determined at a call site; the rest of the type
information is specified at the import declaration." Every consumer of a formal's SV type -- the
boundary temporary, the prototype that shapes a marshaled value, the write-back destination -- needs
the actual's type, not the formal's. A type describing the formal's unsized shape would therefore be
a type no consumer of the SV type system may use.

`mir.md` states where such a fact belongs: the ABI classification of a foreign call's formal is a
property of the signature and belongs on the callable's declaration.

### F5. Per-dimension ranges follow the declaration, not normalization

Annex H.7.5 excludes open arrays' unpacked dimensions from normalization, and H.7.6 fixes each
dimension's reported range:

> The range of a sized dimension in an open array formal argument is specified by the import or
> export declaration. Each unsized, unpacked dimension has the same range as the corresponding
> dimension of the actual argument. An open array formal argument's unsized, packed dimension has
> the linearized, normalized range of all the actual's packed dimensions.

So an open array's sized unpacked dimension reports its declared range (`[3:1]`), not a normalized
one. Normalization applies to a formal that is a sized array throughout -- which is not an open
array at all. Both sources are static at lowering: one from the declaration, one from the actual's
type.

## Decision

### 1. An open array crosses as a canonical boundary object owning its own storage

The call site materializes the actual into a contiguous canonical image of the array -- every
element in its Annex H.7.7 form, dimensions flattened in the LRM 7.6 left-to-right element order --
and hands the foreign side a handle to it. A write-back direction reconstructs one SV value from
that image after the call and stores it through the actual's ordinary write path.

Nothing aliases live SV storage. The boundary object is the same ABI temporary every other carrier
is: produced by marshal-in, consumed by the foreign call and marshal-out, gone at the end of the
lowered-call window ([dpi-foreign-boundary](dpi-foreign-boundary.md) decision 3).

The object is monomorphic -- a canonical image is element-type-independent -- so it needs no type
parameter, no accessor table, and no per-element-type generated code. Its two-state and four-state
forms are one type distinguished by a runtime field, because the C side spells both
`svOpenArrayHandle` and the ABI draws no distinction; this is the same shape choice
[integral-representation](integral-representation.md) made for packed values.

### 2. Whether the pointer accessors work is a property of the element, answered from its carrier

The whole-array and element pointers are served when the element's canonical form is also how an
individual value of that type crosses, and are NULL otherwise (F3). The boundary object carries that
one fact; lowering supplies it from the element's own carrier classification. A NULL return is the
LRM-sanctioned answer, not a gap: the canonical and scalar element accessors are the paths the
standard makes unconditional, and they serve every supported element type.

The object may therefore expose contiguous canonical storage, but exposing it is not part of its
contract. A future realization that is not contiguous changes only what the pointer accessors
return.

### 3. The formal's open-array shape lives on the ABI carrier, not in the type system

The carrier records only what the formal's SV type does not: whether the sole packed dimension is
unsized, and for each unpacked dimension the range the declaration fixes or its absence where the
actual supplies it (F5). The element type, its width, and its state domain stay on the formal's SV
type and are read from there.

There is one record of the shape, so there is nothing for a second record to drift from. The
alternative -- an open-array type in HIR and MIR -- is rejected in full below.

### 4. A DPI argument has two lowering shapes, not one per carrier

An argument either crosses by value (a scalar: the SV value converts to a machine value and is
passed directly) or through a boundary object (a canonical buffer for one packed value, a canonical
image for an open array: construct a local from the actual, pass its pointer or handle, and for a
write-back direction reconstruct and store afterwards). The open array joins the second shape rather
than adding a third.

## Rejected alternatives

- **A handle aliasing the live SV value.** The first shape considered: the descriptor holds a
  pointer to the actual's storage plus its declared ranges, and each foreign element access
  transcodes on demand. It is O(1) at the call where the canonical image is O(N). Rejected because
  it is a borrow of an aggregate's interior, which the value model does not admit
  ([slice-value-semantics](slice-value-semantics.md)); because a fat element has no C representation
  so every pointer accessor would return NULL, losing the whole-array and element-pointer paths the
  LRM's own examples use; and because reaching an element of an arbitrary element type from a
  type-erased handle needs either a generated accessor table or per-element-type runtime code, where
  the canonical image needs neither. The O(N) marshal is also the order of the usage: open arrays
  exist so foreign code can walk arrays of any size.

- **Native C element layout for elements whose individual form is a C scalar** -- a `byte` array
  laid out as `char[]` rather than one 32-bit group per element. This is what a `char*`-assuming
  foreign source would want, and other simulators do it. Rejected because H.7.3 states the
  representation directly for exactly this case: an open array whose element type is a 2- or 4-state
  scalar or packed type is in canonical form. Adopting native layout would also add a layout mode to
  the boundary object -- a second axis -- to serve an access path H.12.4 already permits to be NULL.
  The consequence to accept: a foreign source that bypasses the canonical accessors and treats
  `svGetArrayPtr` on a narrow-element array as a C array is not portable, and against Lyra receives
  the NULL that tells it so.

- **An open-array type in the HIR and MIR type systems**, so the formal's shape rides its SV type
  and the carrier becomes a marker. Rejected on F4: the formal's unsized shape is not the type of
  any value that crosses, so this hands every consumer of a formal's SV type a type none of them may
  use, and grows a never-taken arm in every dispatch over the type variant. `mir.md` puts a formal's
  ABI classification on the callable's declaration for this reason.

- **A named argument-lowering protocol object** with construct / argument / commit operations, one
  implementation per carrier. The two shapes of decision 4 are real, but an interface with two
  implementations, justified by species that do not exist, is surface built ahead of its consumers.
  The shapes are two functions until a third species makes them three.

- **Sized unpacked array formals alongside open ones.** They look like the easier half of the same
  feature and are the opposite: H.11.4 requires them to have C-compiler layout, which a fat value
  cannot provide (F2).

## Consequences

- The supported element types are exactly the 2- and 4-state scalar and packed types -- the ones
  H.7.3 puts in canonical form. An element type H.7.3 puts in C-compatible form (`real`,
  `shortreal`, `string`, `chandle`, an unpacked struct) is legal SystemVerilog that Lyra does not
  yet accept, because serving it requires C layout for that element; the diagnostic says so rather
  than implying the LRM forbids it.
- Both backends consume the same MIR: a boundary-object local constructed from the actual, a handle
  argument, and a reconstruct-and-store for a write-back direction. Only the realization of the
  boundary object differs.
- An exported subroutine cannot take an open array (LRM 35.5.6.1, H.8.2), and the frontend already
  rejects the declaration that would produce one, so the boundary carries no export-side arm.

## Cross-references

- LRM 35.5.6.1 (open arrays), 35.6.1.1 (WYSIWYG; ranges determined at the call site), 35.5.4
  (`"DPI-C"` passes canonical format), 35.5.5 (a result is a small value).
- Annex H.7.3 (data representation), H.7.5 / H.7.6 (normalized and declared ranges), H.7.7
  (canonical representation of packed arrays), H.8.6 (argument passing by handle), H.11.4 (C layout
  for unpacked arrays), H.12 (the open-array surface).
- [dpi-foreign-boundary](dpi-foreign-boundary.md) -- the callable model, the ABI-temporary carrier,
  and the marshaling-as-MIR-call rule this extends.
- [integral-representation](integral-representation.md),
  [unpacked-array-representation](unpacked-array-representation.md),
  [jit-aggregate-realization](jit-aggregate-realization.md) -- the value representations that make C
  layout unavailable.
- [slice-value-semantics](slice-value-semantics.md) -- the value-not-borrow access model the
  aliasing alternative would have contradicted.
- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- the declared range as a
  type-derived operand materialized at lowering, which the dimension operands follow.
