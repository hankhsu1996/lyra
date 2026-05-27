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
identical** operand domains. HIR mirrors that distinction with **two separate expression variants**,
not one merged node.

**Element-select** `expr[idx]` -- single-bracket index. LRM names the result by operand type:
"bit-select" on a 1D packed integral (1 bit, LRM 11.5.1); "single element" on multi-dim packed /
unpacked / dynamic / queue arrays (LRM 7.4.5); byte on string (LRM 6.16); key-indexed element on
associative arrays (LRM 7.8).

**Range-select** `expr[hi:lo]` / `expr[base +: w]` / `expr[base -: w]`. LRM names the result
"part-select" when the operand is a 1D packed array (bit-level, LRM 11.5.1) and "slice" when the
operand is multi-dim packed / unpacked / dynamic / queue (element-level, LRM 7.4.5).

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
reason for two HIR nodes rather than one merged variant. A single merged node would force every
visitor that touches selectors to carry a cross-cutting validity check ("this arm is only legal on
these operand types"). Two nodes keep name and validity domain in lockstep, and W7's lvalue-side
extensions can be expressed without arm-by-arm guards.

HIR shapes (locked):

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

Bound payloads carry source-form `ExprId`s rather than pre-resolved storage bit positions; the
constant-evaluation and direction-resolution happen at HIR -> MIR, where the operand's MIR type is
in hand. HIR is locked in shape but not forced to be storage-normalized.

Naming:

- `ElementSelectExpr` matches slang's `ElementSelectExpression` and LRM 7.4.6 "element of the
  array". **Not** "BitSelect": whether the result is 1 bit is operand-derived, not an operator
  property.
- `RangeSelectExpr` matches slang's `RangeSelectExpression`. LRM uses different result names
  ("part-select" / "slice") per operand; the HIR node stays operand-neutral.

#### MIR: per-operator variants per storage class

MIR mirrors HIR's split, but variants are organized by **(operator x storage class)** pair so that
each MIR shape maps to exactly one runtime emit. For packed storage the operator distinction is
preserved at MIR even though the runtime call (`PackedArray::Select(off, w)`) happens to be the same
for both operators -- this keeps the operator-level intent visible in MIR dumps and prevents
asymmetry once non-packed storage classes arrive.

```
struct ElementSelectExpr {       // mir, packed-storage element-select (W4 wires this)
  ExprId base_value;
  ExprId index;                  // mirrors HIR; the operand was v[index] in source
};

// W5/W6 will add (operator-form bounds, mirroring HIR but with constants resolved
// to storage bit positions / widths where LRM requires constant operands):
struct RangeConstantBoundsMir   { int32_t msb_bit; int32_t lsb_bit; };  // direction-resolved
struct RangeIndexedUpBoundsMir  { ExprId base_index; uint32_t width; };
struct RangeIndexedDownBoundsMir{ ExprId base_index; uint32_t width; };
using RangeBoundsMir = std::variant<
    RangeConstantBoundsMir, RangeIndexedUpBoundsMir, RangeIndexedDownBoundsMir>;

struct RangeSelectExpr {         // mir, packed-storage range-select
  ExprId base_value;
  RangeBoundsMir bounds;
};
```

MIR carries source-form operands (just like `BinaryExpr` keeps `lhs` / `rhs` rather than a
pre-evaluated result). The derivation to the runtime call `Select(bit_offset, width)` happens at the
cpp render boundary:

- `mir::ElementSelectExpr { base, index }` on a packed-integral operand whose element type is
  `width` bits wide: render emits `(base).Select(index * width, width)`. For 1D bit-select
  (`width == 1`) this collapses to `(base).Select(index, 1)`; multi-dim packed materializes the
  multiplication.
- `mir::RangeSelectExpr` with `RangeConstantBoundsMir { msb_bit, lsb_bit }`: render emits
  `(base).Select(lsb_bit, msb_bit - lsb_bit + 1)`.
- `mir::RangeSelectExpr` with `RangeIndexedUpBoundsMir { base_index, width }`: render emits
  `(base).Select(base_index, width)`.
- `mir::RangeSelectExpr` with `RangeIndexedDownBoundsMir { base_index, width }`: render emits
  `(base).Select(base_index - width + 1, width)`.

Future non-packed storage classes get their own MIR variants reflecting different runtime models
(e.g. `mir::UnpackedElementLoad { array_base, element_index }` for unpacked element-select via
pointer + load, `mir::UnpackedSlice { ... }` for unpacked range-select, `mir::AssociativeLookup` for
associative element-select). The naming pattern is `<Operator><Adjective>Expr` where the adjective
distinguishes runtime model when needed.

Conversions at HIR -> MIR (1D packed integral operand):

- `hir::ElementSelectExpr { base, index }` -> `mir::ElementSelectExpr { base, index }`. Pure
  pass-through; no derived fields computed at this boundary.
- `hir::RangeSelectExpr` with `RangeConstantBounds { msb_expr, lsb_expr }` -> evaluate the
  constants, resolve the operand's declared range direction, produce
  `RangeConstantBoundsMir { msb_bit, lsb_bit }` (storage positions, `msb_bit >= lsb_bit`).
- `hir::RangeSelectExpr` with `RangeIndexedUpBounds { base_index, width_expr }` -> evaluate
  `width_expr` to `uint32_t`, produce `RangeIndexedUpBoundsMir { base_index, width }`.
- `hir::RangeSelectExpr` with `RangeIndexedDownBounds { base_index, width_expr }` -> evaluate
  `width_expr`, produce `RangeIndexedDownBoundsMir { base_index, width }`.

For multi-dim packed (future `packed.md` workstream), the same MIR variants are reused; the
operand's element-type width is queried at render time.

#### LRM 11.5.1 / 7.4.5 corner-case rules (runtime helper-side)

- Any X / Z bit in a runtime `bit_offset` propagates: read result is all-X of `width` bits (4-state)
  or all-0 (2-state).
- Fully out-of-range read returns all-X / all-0.
- Partially out-of-range read returns in-range bits verbatim and X / 0 for the OOB bits.
- The result of any selector is **unsigned** regardless of operand signedness (LRM 11.8; slang
  applies `makeUnsigned`); AST -> HIR records this on the `Expr.type`.
- Bit-select / part-select of a scalar or real variable is illegal; slang rejects at AST
  construction, so HIR never sees the shape.

- [x] W4 -- Bit-select read `v[idx]` (scope: 1D integral operand; result is 1 bit per LRM 11.5.1).
      Introduce **both** HIR variants -- `hir::ElementSelectExpr` and `hir::RangeSelectExpr` with
      all three `RangeBounds` arms -- so the HIR shape is locked across W4..W7 and later cuts only
      wire MIR paths. Wire `hir::ElementSelectExpr` end-to-end through `mir::ElementSelectExpr` and
      `PackedArray::Select(bit_offset, width)` (single read helper, named after LRM's "vector
      bit-select and part-select" verb, matching the existing verb-form API style: `CaseEqual`,
      `WildcardEquals`, `LogicalShiftRight`). `hir::RangeSelectExpr` is reachable from AST -> HIR
      but HIR -> MIR rejects with `kUnsupportedExpressionForm` until W5 / W6 add
      `mir::RangeSelectExpr`. Implements LRM 7.4.5 / 11.5.1 OOB and X / Z propagation rules on the
      runtime side.

- [ ] W5 -- Non-indexed (constant) part-select read `v[msb:lsb]`. Add `mir::RangeSelectExpr` and
      wire HIR -> MIR for `hir::RangeSelectExpr` carrying `RangeConstantBounds`. AST -> HIR routing
      is already in W4; this cut evaluates the bound constants, resolves the operand's declared
      range direction (LRM 11.5.1 `b_vect[0:7]` for `logic [0:31] b_vect` example), and emits
      `mir::RangeSelectExpr` with `RangeConstantBoundsMir`. Runtime path reuses
      `PackedArray::Select`.

- [ ] W6 -- Indexed part-select read `v[base +: width]` / `v[base -: width]`. Wire HIR -> MIR for
      `hir::RangeSelectExpr` carrying `RangeIndexedUpBounds` / `RangeIndexedDownBounds`. `width`
      must be a strictly positive constant integer expression (LRM 11.5.1); zero or negative widths
      are rejected at HIR -> MIR. cpp render derives `bit_offset = base_index - width + 1` for the
      down form. Runtime path reuses `PackedArray::Select` -- X / Z propagation on `base_index` and
      partial-OOB rules follow the W4 path automatically.

- [ ] W7 -- Selector lvalue (write side). Extend HIR / MIR `Lvalue` to carry a root `*Ref` plus an
      optional selector kind (Element or Range, mirroring the read-side shapes; multi-layer
      selectors are `packed.md`). Add `PackedArray::AssignSelect(bit_offset, width, value)` -- the
      single write helper, parallel to the existing `AssignFrom` partial-write convention. LRM
      11.5.1 write rules: a fully-OOB write has **no effect** on the stored value (silent no-op, not
      an error); a partially-OOB write affects **only the in-range bits**; an X / Z `bit_offset` is
      treated as a fully-OOB write (no effect). Closes
      `datatypes/packed/indexed_part_select/default.yaml` for both read and write halves.

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
