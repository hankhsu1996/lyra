# Operators

Tracks the operator surface that does not live in `integral.md`: set membership, wildcard / case
equality, selectors (bit-select, non-indexed and indexed part-select) on both read and write sides,
concatenation, replication, compound assignment, and the `++` / `--` family. Each archive item
checkbox in `architecture-reset.md` is checked when its `*.yaml` cases reproduce on the current
pipeline.

Done when:

- `operators/inside`, `operators/wildcard_equality`, `operators/case_equality`, `operators/concat`,
  `operators/replicate`, `operators/compound_assignment`, and the `++` / `--` portion of
  `operators/unary` reproduce.
- `datatypes/packed/indexed_part_select` reproduces (the selector surface lives here even though the
  archive groups it under `datatypes/packed`).

The numeric IDs (W1..W12) imply execution order: each later cut assumes the earlier cuts have
landed. Where a cut is independent the text says so.

## Sub-Steps

### Membership and equality

- [x] W1 -- `inside` set-membership. Add `hir::InsideExpr` carrying a left operand and an item list,
      where each item is either a singular expression or a range `(lo, hi)`. HIR -> MIR desugars to
      a logical-OR chain of comparisons over the lowered left operand: value items compare with
      `==`; range items use `(>= lo) && (<= hi)`. MIR gains no new variant, cpp emit gains no new
      path, and the runtime gains no new helper -- the OR-chain rides on the existing `PackedArray`
      binary operators. LRM 11.4.13 four-state corner ("no match but some compare X yields `1'bx`")
      is satisfied automatically because `PackedArray::LogicalOr` already implements the LRM 11.4.7
      truth table. The left operand is lowered once to a MIR `ExprId` referenced by each comparison;
      cpp emit re-renders each reference, so plain var-refs read the variable repeatedly
      (idempotent) while side-effecting left operands would re-evaluate -- the archive test surface
      uses only var-refs. A procedural-temp snapshot pattern (see C3 case selector) covers the
      side-effect case if it becomes necessary.

- [x] W2 -- Wildcard equality `==?` / `!=?`. The HIR / MIR `BinaryOp::kWildcardEquality` /
      `kWildcardInequality` enums and the AST -> HIR -> MIR routing were already in place; this cut
      adds the runtime helper `PackedArray::WildcardEquals` (asymmetric per LRM 11.4.6: X / Z in the
      right operand are wildcards, X / Z in the left operand are not, so the result is 1'bx when the
      left operand carries an X / Z that meets a known right-operand bit and no other bit definitely
      mismatches). cpp emit lowers `kWildcardEquality` to `WildcardEquals(...)` and
      `kWildcardInequality` to `WildcardEquals(...).LogicalNot()`. W1's value-item path retrofits
      from `kEquality` to `kWildcardEquality` so wildcard items in `inside` work. Closes
      `operators/wildcard_equality/four_state.yaml` and unblocks `control-flow.md` C11.

- [ ] W3 -- Case equality `===` / `!==`. Add `BinaryOp::kCaseEq` / `kCaseNeq` plus a
      `PackedArray::CaseEquals` helper (bit-exact 4-state compare, X matches X). Independent of W1
      and W2. Closes `operators/case_equality/default.yaml`.

### Selectors

LRM distinguishes two syntactically distinct selector operators with overlapping but **not
identical** operand domains. HIR and MIR mirror that distinction with **two separate expression
variants**, not one merged node.

**Element-select** `expr[idx]` -- single-bracket index. LRM 7.4.5 names this "element" of the array;
LRM 11.5.1 specializes it to "bit-select" when the operand is a 1D vector (result is 1 bit).

**Range-select** `expr[hi:lo]` / `expr[base +: w]` / `expr[base -: w]`. LRM 7.4.5 names this "slice"
of the array; LRM 11.5.1 specializes it to "part-select" when the operand is a 1D vector.

LRM operand-domain validity:

| Operand type                            | element-select       | range-select                                   |
| --------------------------------------- | -------------------- | ---------------------------------------------- |
| 1D packed integral (vector)             | bit-select (1 bit)   | part-select (multi-bit)                        |
| multi-dim packed, packed struct / union | element (sub-vector) | slice / part-select                            |
| unpacked array, dynamic array, queue    | element              | slice                                          |
| **associative array**                   | element (key-typed)  | **illegal** (LRM 7.4.6)                        |
| **string**                              | byte (LRM 6.16)      | **illegal** (LRM 6.16 lists only `Str[index]`) |
| real / shortreal                        | illegal (LRM 6.12)   | illegal (LRM 6.12)                             |
| scalar `reg` / `logic` / `bit`          | illegal (LRM 11.5.1) | illegal (LRM 11.5.1)                           |

The asymmetry on associative arrays and strings (element-select valid, range-select not) is the
reason for two IR nodes rather than one merged variant. A single merged node would force every
visitor that touches selectors to carry a cross-cutting validity check ("this arm is only legal on
these operand types"). Two nodes keep name and validity domain in lockstep, and W7's lvalue-side
extensions can be expressed without arm-by-arm guards.

HIR shapes:

```
struct ElementSelectExpr {                       // expr[idx]
  ExprId base_value;
  ExprId index;
};

struct RangeConstantBounds    { ExprId msb_expr;   ExprId lsb_expr; };  // expr[hi:lo]
struct RangeIndexedUpBounds   { ExprId base_index; ExprId width;    };  // expr[base +: w]
struct RangeIndexedDownBounds { ExprId base_index; ExprId width;    };  // expr[base -: w]
using RangeBounds = std::variant<
    RangeConstantBounds, RangeIndexedUpBounds, RangeIndexedDownBounds>;

struct RangeSelectExpr {
  ExprId base_value;
  RangeBounds bounds;
};
```

MIR shape mirrors HIR 1:1. Each bounds field stays an `ExprId`, so HIR -> MIR is structurally a
pass-through (recursively lower each `ExprId`, repack into the same variant arm). No constant
evaluation or direction normalization happens at lowering -- slang has already validated the LRM
contract during AST construction (constants are constant, indexed widths are positive, simple-form
direction matches the operand's declared range; partial-OOB stays in the AST as a warning), and the
result `Expr.type` carries the slice's width, so render can read it directly.

Naming:

- `ElementSelectExpr` / `RangeSelectExpr` match slang's `ElementSelectExpression` /
  `RangeSelectExpression`. LRM 7.4.5 vocabulary aligns with slang on these names. **Not**
  "BitSelect": whether the result is 1 bit is operand-derived, not an operator property.

#### Runtime `PackedArray` API

The runtime exposes two read-side methods, named after the LRM 7.4.5 vocabulary ("element" /
"slice") in verb form for symmetry with the rest of CS practice (Python / NumPy / Rust):

```
auto Index(const PackedArray& index) const -> PackedArray;             // element-select (1 bit)
auto Slice(const PackedArray& lsb_bit, std::uint32_t width) const -> PackedArray;  // range-select
```

`Slice` codifies LRM 7.4.5's contract directly: "The size of the part-select or slice shall be
constant, but the position can be variable" -- `width` is compile-time `uint32_t`, `lsb_bit` is a
runtime `PackedArray` (so X / Z propagation falls out for free). The MSB position is implicit at
`lsb_bit + width - 1`; we do not surface it as a separate parameter because the LRM size constraint
makes it derivable.

`Slice` operates on a **canonical LSB-anchored bit address space**, dimension-agnostic at the API
boundary. Element-vs-bit accounting lives one layer up in the MIR type system, not in PackedArray.
For 1D operands element width is 1 bit, so source-form indices coincide with bit positions and
render passes them through verbatim. For future multi-dim packed operands (`bit [N:0][M:0] arr`
etc.), LRM 7.4.5 says slice applies to one dimension and the other dimensions are preserved --
element-level source-form positions must be multiplied by the inner element's bit width at render
time before reaching `Slice`. W5 only wires 1D; the multi-dim extension is part of the `packed.md`
workstream.

How `PackedArray` represents bits internally (flat word planes today; potentially sparse / pooled /
chunked once optimization work begins) is an implementation detail that the `Index` / `Slice`
contract is independent of. Both methods specify positions in the canonical bit address space and
return a fresh `PackedArray` whose bits are the LRM-defined result; any future storage layout change
must preserve this external contract.

`Index(idx)` is a convenience over `Slice(idx, 1)` for the 1-bit case, kept distinct so the call
site preserves the LRM-level operator name (element-select vs slice).

Both methods return a fresh `PackedArray` by value -- no views, consistent with every other
`PackedArray` op. Result is unsigned regardless of operand signedness (LRM 11.8; slang applies
`makeUnsigned` on the `Expr.type` we read).

#### Render rules

Width comes from the slice expression's MIR result type (`unit.GetType(expr.type).BitWidth()`),
which slang has already pinned at AST construction.

- `mir::ElementSelectExpr { base, index }` -> `(base).Index(<index rendered>)`.
- `mir::RangeSelectExpr` with `RangeConstantBounds { msb_expr, lsb_expr }` ->
  `(base).Slice(<lsb_expr rendered>, <width from result type>)`. `msb_expr` is preserved in MIR for
  dump / debug symmetry with HIR; render does not consult it (the width via result type carries the
  same information).
- `mir::RangeSelectExpr` with `RangeIndexedUpBounds { base_index, width }` ->
  `(base).Slice(<base_index rendered>, <width from result type>)`.
- `mir::RangeSelectExpr` with `RangeIndexedDownBounds { base_index, width }` ->
  `(base).Slice((<base_index rendered>) - PackedArray::FromInt(<width - 1>, <base_index shape>), <width from result type>)`.
  The literal `width - 1` is constructed to match `base_index`'s PackedArray shape so the runtime
  subtraction does not need cross-shape promotion.

#### LRM 11.5.1 / 7.4.5 corner-case rules (handled inside `PackedArray::Slice`)

- Any X / Z bit in the runtime `lsb_bit` propagates: result is all-X of `width` bits (4-state) or
  all-0 (2-state).
- Fully out-of-range read returns all-X / all-0.
- Partially out-of-range read returns in-range bits verbatim and X / 0 for the OOB bits.
- Bit-select / part-select of a scalar or real variable is illegal; slang rejects at AST
  construction, so HIR never sees the shape.

- [x] W4 -- Element-select read `v[idx]` (scope: 1D integral operand; result is 1 bit per LRM
      11.5.1). Wired end-to-end: `hir::ElementSelectExpr` -> `mir::ElementSelectExpr` ->
      `PackedArray::Index(idx)`. HIR / MIR introduce all three `RangeBounds` arms at the same time
      so the shape is locked across W4..W7; W5 wires `RangeSelectExpr` through HIR -> MIR and
      render. Implements LRM 7.4.5 / 11.5.1 OOB and X / Z propagation rules on the runtime side
      (`Slice` is the underlying helper; `Index(idx)` is `Slice(idx, 1)`).

- [x] W5 -- Range-select read covering all three bounds forms: non-indexed constant `v[msb:lsb]`,
      indexed-up `v[base +: w]`, indexed-down `v[base -: w]`. Wire `mir::RangeSelectExpr` mirroring
      HIR (three `RangeBounds` variants, all `ExprId` fields). HIR -> MIR is a pure structural
      pass-through. cpp render dispatches on the MIR bounds variant and emits
      `(base).Slice(lsb_bit, width)`; for indexed-down it constructs the LSB position as
      `base_index - PackedArray(width - 1)`. Runtime reuses `PackedArray::Slice`. Width at render
      comes from the result type, not from any MIR-side constant extraction. Closes
      `datatypes/packed/indexed_part_select/default.yaml` for the read half.

- [ ] W6 -- Selector lvalue (write side). Extend HIR / MIR `Lvalue` to carry a root `*Ref` plus an
      optional selector kind (Element or Range, mirroring the read-side shapes; multi-layer
      selectors are `packed.md`). Add `PackedArray::AssignSlice(lsb_bit, width, value)` /
      `PackedArray::AssignIndex(idx, value)` parallel to the existing `AssignFrom` partial-write
      convention. LRM 11.5.1 write rules: a fully-OOB write has **no effect** on the stored value
      (silent no-op, not an error); a partially-OOB write affects **only the in-range bits**; an X /
      Z position is treated as a fully-OOB write (no effect). Closes
      `datatypes/packed/indexed_part_select/default.yaml` for the write half.

### Construction

- [ ] W8 -- Concatenation read `{a, b, c}`. Add `hir::ConcatExpr` / `mir::ConcatExpr` with an
      ordered operand list. Signed operands contribute raw bits; unsized literals are rejected at
      slang frontend already (LRM 11.4.12). 4-state operands propagate by concatenating both value
      and unknown planes in lockstep. Add `PackedArray::Concat(span<view>)`.

- [ ] W9 -- Replication `{N{x}}`. Add `hir::ReplicationExpr` / `mir::ReplicationExpr`; multiplier
      must be a constant non-negative non-X/Z value (LRM 11.4.12.1). Zero multiplier collapses to
      empty and is legal only when nested inside a `ConcatExpr` with at least one positive-sized
      sibling. Add `PackedArray::Replicate(view, n)`. String replication is deferred to the string
      workstream.

- [ ] W10 -- Concatenation lvalue `{a, b, c} = ...`. Add a `ConcatLvalue` arm to `Lvalue` carrying
      an ordered list of sub-lvalues. HIR -> MIR snapshots the right-hand side into a `PackedArray`
      temp and distributes its bits MSB-first into each sub-target via the same selector machinery
      W7 provides.

### Assignment families

- [ ] W11 -- Compound assignment `+= -= *= /= %= &= |= ^= <<= >>= <<<= >>>=`. Add
      `hir::CompoundAssignStmt { Lvalue target; BinaryOp op; ExprId rhs }`. HIR -> MIR snapshots
      every index expression in `target` into fresh procedural temps before the read-modify-write so
      the target lvalue evaluates exactly once (LRM 11.4.1). Covers `arr[i++] += x` and
      partial-target shapes such as `data[i+:4] |= v` once W7 is in.

- [ ] W12 -- `++` / `--` (prefix and postfix). Add `hir::IncrDecrStmt` and `hir::IncrDecrExpr`
      (separate from `AssignExpr` because the expression form returns the pre- or post-value). HIR
      -> MIR uses the same evaluate-target-once snapshot pattern as W11. Closes the `++` / `--` gap
      noted in `architecture-reset.md` for `operators/unary`.

## Cross-references

- LRM anchors: 7.4.5 (indexing and slicing of arrays: vocabulary, invalid-index rules), 11.4.5 (case
  equality), 11.4.6 (wildcard equality), 11.4.10 (shift), 11.4.12 / 11.4.12.1 (concatenation,
  replication), 11.4.13 (set membership), 11.5.1 (bit-select / part-select / indexed part-select on
  packed).
- `integral.md` J14 archive sweep depends on W4..W6 for any binary / shift-overflow case that
  selects sub-bits before applying the operator.
- `control-flow.md` C11 unblocked by W2; C12 unblocked by W1 (basic) and W2 (wildcard items).
- `packed.md` P1..P5 extend W7's selector list to multi-layer and add field / variant selectors;
  they assume W7 has landed.
