# Slice read materializes a value; the access model is value, not borrow

## Date

2026-06-19

## Status

Accepted

## Context

Aggregate access has two sides. A read produces a value; a write targets a location. For a
whole-variable access this split is invisible -- `x = arr` reads the value, `arr = y` writes the
variable. Introducing the slice `arr[base+:w]` (LRM 7.4.5 / 7.4.6) made the split visible: the same
syntactic construct appears as a read on one side of an assignment and a write on the other, and the
runtime realizes the two differently -- a read returns an owned value, a write returns a transient
write-back proxy.

This raised a design question worth recording: now that a slice -- a window into an existing
aggregate -- exists, should the access model shift to a borrow / view model in the style of Rust,
where a slice is uniformly a view (`&[T]` / `&mut [T]`) and a read is `view` then `to_owned`, a
write is `view` then scatter? The decisions that set "read produces a value" predate slices, so they
were re-evaluated against this rather than treated as settled. The write side (a location, realized
by reference) was not in dispute; the live question was the read side.

The conclusion below is re-derived from SystemVerilog's semantics and the C++ runtime's reality, not
from the earlier decisions. It happens to agree with them, but the rationale stands on its own.

## Findings

### F1. A SystemVerilog slice is not a Rust slice

A Rust `&v[1..4]` is a borrow of existing contiguous memory: `view[i]` is `*(ptr + i)`, zero-cost
and branch-free, and an out-of-range range panics -- the borrow is always in bounds. A SystemVerilog
slice is the opposite kind of object. LRM 7.4.5 + Table 7-1 define it as a total function of the
base: an out-of-range position reads the element type's default and an x / z offset makes the whole
slice default. The slice is therefore a logical window that may include positions with no backing
storage, whose values are synthesized on read. A partially synthesized window has no pure-pointer
representation; its natural form is a materialized value -- the synthesized and real elements
gathered into one result.

### F2. SystemVerilog is a value-semantics language

LRM 7.6 makes array assignment a copy and LRM 7.7 makes a by-value array argument a copy ("a copy of
the array is passed ... true for all array types"). The source language exposes no user-level
reference or lifetime concept except the scoped `ref` argument. Owned values are the natural
lowering of such a language. A borrow / view model serves the ownership-and-borrowing semantics of a
language that has them; importing it into a value-semantics source language adds a concept the
language does not use.

### F3. Emitted C++ has no borrow checker

Rust's `&[T]` is safe only because the borrow checker proves the borrow does not outlive its owner.
The emitted C++ has no such proof. A view over resizable storage that escapes its source dangles,
and nothing in the emitted code catches it:

```
auto v = arr.Slice(1, 3);   // a view would hold arr's element-vector pointer
arr = new[100];             // arr's storage reallocates -> v dangles
... use v ...               // undefined behavior, uncatchable
```

The SystemVerilog `x = arr[i+:3]; arr = new[N];` is legal and must be safe. Materializing on read
makes the dangle impossible -- the reader owns an independent copy. The write proxy avoids the same
hazard by being move-only and consumed within its statement, never stored. Rust's elegance here is
the pairing of a zero-cost borrow with a checker that makes it safe; emitted C++ can take the borrow
but not the checker, which leaves only the hazard.

### F4. The emit is expression-oriented and composes values

A slice read flows into value contexts: an assignment right-hand side, an equality operand, a
by-value argument, a `foreach`. For a slice expression to be usable wherever an aggregate value is
expected, its result must be a value or must materialize at the point of use. A lazy view delivers
none of the saving it promises: it either materializes at each use (no copy avoided, plus an
implicit conversion), or forces an explicit `to_owned` at every site (which breaks composition and
pushes a read-vs-write branch back into the backend), or requires every aggregate operator to accept
a view (a large surface duplicated for a marginal copy saved on a typically small slice).

### F5. Read-as-value with write-as-reference is the value-semantics asymmetry

The asymmetry is not a defect. C++ itself reads `int x = a[i];` as a value and writes `a[i] = x;` as
a location; the same indexed construct is a value on one side and an lvalue on the other. Rust
unifies the two with `&` / `&mut` because it is borrow-oriented. A value-semantics language does
not, and a runtime modeling one should not invent the unification.

## Decision

1. **A slice read is a materialized owned value.** The const slice path returns an owned fixed-size
   unpacked array; the LRM 7.4.5 / Table 7-1 out-of-range and x / z behavior is computed inside the
   slice operation, the same way packed bit-extraction lives inside the packed slice. The corner
   cases live in the runtime operation, never exposed as a view accessor a consumer must interpret.

2. **A slice write is a transient write-back proxy.** The non-const slice path returns a move-only
   proxy whose assignment scatters the values into the source storage; an out-of-range position is
   skipped and an x / z offset is a whole no-op. The proxy is consumed within its statement and
   never stored.

3. **A borrow / view is not the access model and is rejected as the primary form**, for F1 through
   F5: a SystemVerilog slice is a partially synthesized window, not a memory borrow; the source
   language is value-semantics; the emitted target has no borrow checker; the expression-oriented
   emit composes values.

4. **A view may be added later as an opt-in fast path**, never the foundation. If profiling shows a
   read-only consumer (aggregate equality, a reduction, a `foreach`) paying materialization cost
   that matters, that consumer may take a view input, the way `std::span` / `std::string_view` layer
   on `std::vector` / `std::string` without displacing them. The owned value stays the default and
   the foundation.

## Consequences

- Read-as-value and write-as-reference are uniform across whole-variable access, element access, and
  slice access; a slice looks like every other aggregate access, not like a borrow.
- The slice read / write asymmetry is intentional and is documented as such, so it is not mistaken
  for an inconsistency to be unified away.
- A future view fast path is additive: it does not require rebuilding the value layer, only adding a
  view-accepting overload where a measurement justifies one.

## Forbidden shapes

- A slice read realized as a borrow or view that the source language's value semantics would have
  copied. The read materializes.
- A slice view that is stored, returned, or otherwise escapes the statement that produced it.
  Without a borrow checker this is a dangling reference over storage that may reallocate.
- Lifting the LRM out-of-range / clamp / default-fill semantics out of the runtime slice operation
  into a view accessor that consumers must re-interpret. The corner cases belong inside the
  operation.
- Treating read-as-value / write-as-reference as an inconsistency and unifying it under a borrow
  model. That asymmetry is the shape of a value-semantics language.

## Cross-references

- LRM 7.4.5 + Table 7-1 (invalid-index read / write), 7.4.6 (operations on arrays), 7.6 (array
  assignment is a copy), 7.7 (a by-value array argument is a copy).
- `integral-representation.md` -- the fat value a materialized read produces.
- `runtime-shape-and-default-value.md` -- the element-shape shield slot that sources the synthesized
  default in an out-of-range slice position.
- `queue-operators.md` -- the parallel "read and write are different operations, chosen at lowering"
  result for queue access, reached independently for queues; this decision re-derives the read-side
  half from language semantics rather than citing it.
