# Operators

Tracks the operator surface that does not live in `integral.md`: set membership, wildcard / case
equality, selectors (bit, range, indexed part-select) on both read and write sides, concatenation,
replication, compound assignment, and the `++` / `--` family. Each archive item checkbox in
`architecture-reset.md` is checked when its `*.yaml` cases reproduce on the current pipeline.

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

- [ ] W2 -- Wildcard equality `==?` / `!=?`. Add `BinaryOp::kWildcardEq` / `kWildcardNeq` plus a
      `PackedArray::WildcardEquals` helper that treats X / Z in the right-hand operand as
      do-not-care while still letting X / Z in the left-hand operand mismatch (LRM 11.4.6, 11.4.13).
      W1's value-item path retrofits to use `WildcardEquals` so wildcard items in `inside` work.
      Closes `operators/wildcard_equality/four_state.yaml` and `operators/inside/four_state.yaml`,
      and unblocks `control-flow.md` C11.

- [ ] W3 -- Case equality `===` / `!==`. Add `BinaryOp::kCaseEq` / `kCaseNeq` plus a
      `PackedArray::CaseEquals` helper (bit-exact 4-state compare, X matches X). Independent of W1
      and W2. Closes `operators/case_equality/default.yaml`.

### Selectors

- [ ] W4 -- Bit-select read `v[idx]`. Introduce `hir::SelectExpr` / `mir::SelectExpr` whose payload
      is a `Selector` variant. The variant arms are `kBit(index)`, `kRange(hi, lo)`,
      `kIndexedAsc(base, width)`, and `kIndexedDesc(base, width)`; this cut wires only the `kBit`
      arm end-to-end. Add `PackedArray::BitSelect(idx)` returning a 1-bit `PackedArray`.
      Out-of-bounds or X / Z index yields `1'bx` (4-state) or `1'b0` (2-state) per LRM 11.5.1.

- [ ] W5 -- Range-select read `v[hi:lo]`. Wire the `kRange` arm; both bounds must be constants. Add
      `PackedArray::RangeSelect(hi, lo)` returning a `(hi - lo + 1)`-bit `PackedArray`.
      Partially-out-of-range slices return X for the out-of-range bits when reading (LRM 11.5.1).

- [ ] W6 -- Indexed part-select read `v[i+:w]` / `v[i-:w]`. Wire the `kIndexedAsc` and
      `kIndexedDesc` arms; `width` is a constant positive integer, `base` is a runtime expression.
      Add `PackedArray::IndexedPartSelect(base, width, direction)`. Out-of-range and X / Z base
      follow the same LRM 11.5.1 rules as W4 / W5.

- [ ] W7 -- Selector lvalue (write side). Extend HIR / MIR `Lvalue` from a bare `*Ref` variant to a
      record carrying a root `*Ref` plus an optional `Selector` (multi-layer selectors are
      `packed.md`). Add `PackedArray::WriteBit`, `WriteRange`, `WriteIndexedPartSelect`. Closes
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

- LRM anchors: 11.4.5 (case equality), 11.4.6 (wildcard equality), 11.4.10 (shift), 11.4.12 /
  11.4.12.1 (concatenation, replication), 11.4.13 (set membership), 11.5.1 (bit-select / part-select
  / indexed part-select).
- `integral.md` J14 archive sweep depends on W4..W6 for any binary / shift-overflow case that
  selects sub-bits before applying the operator.
- `control-flow.md` C11 unblocked by W2; C12 unblocked by W1 (basic) and W2 (wildcard items).
- `packed.md` P1..P5 extend W7's selector list to multi-layer and add field / variant selectors;
  they assume W7 has landed.
