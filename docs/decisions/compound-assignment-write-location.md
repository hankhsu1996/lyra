# Compound assignment lowers uniformly; every write target is an op=-able location

Date: 2026-06-28 Status: accepted

## Context

A compound assignment `lhs op= rhs` (LRM 11.4.1) has one semantic rule that distinguishes it from
`lhs = lhs op rhs`: **the left-hand side is evaluated exactly once** -- in particular every
left-hand subscript runs a single time, even one with a side effect (`a[f()] += b` calls `f()`
once).

The question is at which layer that "evaluate once" is realized. Two shapes were possible:

1. **Desugar at HIR-to-MIR.** Lower `lhs op= rhs` to an explicit read-modify-write
   (`store(place, load(place) op rhs)`), hoisting every subscript into a temp at the lowering so the
   read and write sites share it. This makes "once" a lowering concern and bakes a read-op-write
   shape into MIR.
2. **Carry the compound as a high-level node, realize "once" in the backend.** Lower to
   `AssignExpr{target, compound_op, value}` where `target` is an ordinary lvalue expression, and let
   each backend realize the operator on a single evaluation of the target -- the C++ `op=` evaluates
   its left operand once; a future LIR computes the target address once and does load-modify-store.

`slice-value-semantics.md` already settled the access model this rests on: a read materializes a
value, a write targets a location realized by a reference or a write-back proxy, and that asymmetry
is "uniform across whole-variable access, element access, and slice access." Under that model a
packed-array element write is `arr.ElementRef(i)` returning a `PackedArrayRef` proxy whose
`operator=` and `operator+=` read-modify-write through a captured position, and a struct-member
write is a `TupleGet` yielding a real `T&`. Both are op=-able locations, so for those targets shape
2 is already what the compiler does: `arr[i] += v` lowers to `AssignExpr{ElementRef(arr,i), +=, v}`
and renders to `arr.ElementRef(i) += v`, with "once" provided by C++.

Only two write targets did not fit, because they exposed no op=-able location: a string character
(realized as `getc` / `putc` method calls) and an unpacked-union member (realized as a whole-union
rebuild). For those, shape 1 was used as a local desugar -- which is the inconsistency this decision
removes.

## Decision

**Compound assignment lowers uniformly to `AssignExpr{target, compound_op, value}` for every target,
and "evaluate the left-hand side once" is the backend's mechanical realization, not a HIR-to-MIR
desugar.** This requires every write target to be an op=-able location, so the two holdouts gain
write-side locations.

1. **The write side of any element / member access is a location.** A read materializes a value; a
   write yields a write-back location -- a reference for storage that exists in place, a proxy for
   storage that is synthesized, overlapping, or sub-addressed. This is the existing read-value /
   write-location model (`slice-value-semantics.md`), now applied to every element and member kind
   without exception. The "no element lvalue" special case is retired.

2. **String character access is a matched value / reference pair, like a packed array element.** The
   indexed read `s[i]` is `String::Element(i)` (the character value); the write `s[i] = ...` is
   `String::ElementRef(i)`, a `StringCharRef` write-back proxy that captures the index once, routes
   `operator=` through `putc`, and reads-modifies-writes for each compound operator. A string
   exposes no in-place character reference (`putc` enforces the out-of-range / NUL rules, LRM
   6.16.2), so the write side is a proxy. The explicit `s.getc(i)` / `s.putc(i, c)` methods (LRM
   6.16.3 / 6.16.2) stay for the method-call forms; `Element` / `ElementRef` are the indexing forms.

3. **A union member access is a matched value / reference pair, like a struct member.** The read
   `u.f` is `UnionGetExpr` rendering `Union::Get<I>()` (the active-member value, the inactive-member
   default if `I` is not active); the write is `UnionGetRefExpr` rendering `Union::GetRef<I>()`, a
   reference to the active member's storage that makes `I` active first if it is not. `Get` /
   `GetRef` is the same verb a struct member uses (`std::get<I>` is the standard accessor for both a
   `std::tuple` and a `std::variant`, the runtime realizations of struct and union). The two are
   separate access forms because a union member's read realization (a value) differs from its write
   realization (an activating reference) -- whereas a struct member's read and write are one `T&`,
   so a struct collapses to a single `Get`. The write reference is a real reference, not a proxy, so
   `u.f = v`, `u.f op= v`, and a nested `u.f.g = v` or `u.h[i] = v` all compose on it exactly as a
   struct member's reference does. The only difference from a struct member is the activation step;
   the active-member representation (not a byte overlay, see `unpacked-union-representation.md`) is
   why a write activates the member rather than reinterprets shared bytes.

4. **MIR carries one compound shape.** `AssignExpr{target, compound_op, value}` is the single
   representation; `target` is the lvalue expression (a container-access chain ending in
   `kElementRef` for a string character, a `UnionGetRefExpr` for a union member, a `TupleGetExpr`
   for a struct member, an `ElementRef` chain for an array element). There is no read-op-write
   desugar in MIR, and no per-target compound lowering. The HIR-to-MIR string-element and
   union-member assignment special cases, and the subscript-hoisting machinery that supported them,
   are deleted.

5. **"Evaluate once" is the backend's mechanical job.** The C++ backend renders
   `AssignExpr{target, op, value}` as `<target> op= <value>`; C++ evaluates the target expression --
   including every nested subscript -- exactly once before applying the operator. A future LIR
   backend computes the target's address once and does load-modify-store. The lowering states the
   semantic (`compound_op` on `AssignExpr`); each backend realizes "once" by a fixed function of
   that node, with no decision logic (`backend_contract.md`).

## Rejected

- **Desugar compound to read-op-write at HIR-to-MIR (shape 1), with per-target subscript hoisting.**
  It bakes one realization into MIR, gives the same SystemVerilog operation two MIR shapes (a
  high-level `AssignExpr` for array elements, a desugared read-op-write for string / union), and
  forces the "evaluate once" obligation to be re-implemented at every no-lvalue target -- the place
  it was missed. The uniform high-level node plus a backend-realized "once" is the shape the LLVM-IR
  cross-check wants (`backend_contract.md` invariant 5): one node, one mechanical lowering, for
  every target.

- **Keep string character / union member as non-op=-able and special-case their compound forever.**
  This is the inconsistency above; it scales a special case across every operation (simple write,
  compound, future `ref` / `output` binding) instead of giving the target one write-location form
  that all operations consume.

## Consequences

- One runtime write-back proxy, `StringCharRef`: a string character has no in-place reference
  (`putc` enforces the out-of-range / NUL-byte rules, LRM 6.16.2), so its write side follows the
  `PackedArrayRef` proxy shape. A union member needs no proxy -- `Union::GetRef<I>()` returns a real
  reference to the active member's storage.
- Two access families, each a value / reference pair named `X` / `XRef`. Subscript access by a
  runtime index: array / string element `Element` / `ElementRef`, slice `Slice` / `SliceRef`.
  Positional member access by a compile-time index: struct and union member `Get` / `GetRef`
  (`std::get<I>` is the standard accessor for both the `std::tuple` and the `std::variant` that back
  them). A struct member collapses to a single `Get` (its value and reference realizations are the
  same `T&`, the receiver's const-ness picking read from write); a union member keeps both forms
  because its read (a value) and write (an activating reference) differ.
- A union member read is `UnionGetExpr` (renders `Get<I>`); the write is `UnionGetRefExpr` (renders
  `GetRef<I>`) -- two access forms, by compile-time index, parallel to `kElement` / `kElementRef`.
- `value-type-concepts.md` is updated: `String` now participates in the `Indexable` concept --
  `Element` is the indexed character read, `ElementRef` (-> `StringCharRef`) the write; `getc` /
  `putc` stay for the explicit method calls.
- `unpacked-union-representation.md` decision points 4 / 5 are revised: union member access is a
  read form (`UnionGetExpr`) and a write form (`UnionGetRefExpr`), not a target that "never survives
  as a writable place"; the write renders to a reference to the active member. The union value model
  (active-member variant) is unchanged; only the member-write realization changes.
- `operators.md` W13 closes structurally: the left-hand index is evaluated once because the target
  is a single lvalue the backend renders once, for every target kind, with no lowering desugar.

## Cross-references

- `../architecture/backend_contract.md` -- render is a fixed function of one node; the LLVM-IR
  cross-check that wants one uniform compound shape; the wrapper-API pattern (redesign the runtime
  API so MIR calls through existing primitives, then render is uniform).
- `../architecture/lir.md` -- LIR owns addresses and load / store; "evaluate the address once" is
  realized there for the LIR path.
- [slice-value-semantics](slice-value-semantics.md) -- the read-value / write-location model this
  extends to string characters and union members.
- [value-type-concepts](value-type-concepts.md) -- the value-type surface; updated for `String`'s
  write-side element reference.
- [unpacked-union-representation](unpacked-union-representation.md) -- the union representation;
  member-write realization revised here.
- LRM anchors: 11.4.1 (compound assignment, evaluate left-hand side once), 7.3 (unpacked union),
  6.16.2 / 6.16.3 (string putc / getc).
