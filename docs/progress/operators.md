# Operators

Tracks the operator surface that does not live in `integral.md`: set membership, wildcard / case
equality, selectors (bit-select, part-select, indexed part-select) on both read and write sides,
concatenation, replication, compound assignment, and the `++` / `--` family.

Done when:

- `operators/inside`, `operators/wildcard_equality`, `operators/case_equality`, `operators/concat`,
  `operators/replicate`, `operators/compound_assignment`, and the `++` / `--` portion of
  `operators/unary` reproduce.
- `datatypes/packed/indexed_part_select` reproduces (the selector surface lives here even though the
  archive groups it under `datatypes/packed`).

## Actionable

- W14 -- a range-select read whose two bounds have different state domains aborts at runtime. The
  rest of the operator surface in scope is complete.

## Sub-Steps

The numeric IDs (W1..W12) imply execution order; where a cut is independent the text says so.

### Membership and equality

- [x] W1 -- `inside` set-membership (LRM 11.4.13). Singular and range items; ranges use
      `(>= lo) && (<= hi)`. The LRM 11.4.13 four-state corner ("no match but some compare yields
      `1'bx`") is preserved through the existing logical-OR truth table.
- [x] W2 -- Wildcard equality `==?` / `!=?` (LRM 11.4.6). Asymmetric: X / Z in the right operand are
      wildcards; X / Z in the left operand are not. Result is `1'bx` when the left operand carries X
      / Z that meets a known right-operand bit and no other bit definitely mismatches. W1's value
      items use `==?` so wildcard items in `inside` work.
- [x] W3 -- Case equality `===` / `!==` (LRM 11.4.5). Bit-exact 4-state compare (X matches X, Z
      matches Z, X does not match Z); deterministic bool.

### Selectors

LRM distinguishes two syntactically distinct selector operators with overlapping but not identical
operand domains:

- **Element-select** `expr[idx]` (LRM 7.4.5; LRM 11.5.1 names it "bit-select" when the operand is a
  1D vector).
- **Range-select** `expr[hi:lo]` / `expr[base +: w]` / `expr[base -: w]` (LRM 7.4.5; LRM 11.5.1
  names it "part-select" or "indexed part-select" when the operand is a 1D vector).

The two operators differ on operand validity (associative arrays and strings allow element-select
but not range-select; LRM 7.4.6 and 6.16), so the IR keeps them as separate variants rather than a
merged node.

- [x] W4 -- Element-select read `v[idx]`. Covers 1D bit-select and multi-dim element-select
      uniformly. LRM 7.4.5 / 11.5.1 OOB and X / Z propagation rules apply.
- [x] W5 -- Range-select read covering all three bounds forms: constant `v[msb:lsb]`, indexed-up
      `v[base +: w]`, indexed-down `v[base -: w]`.
- [x] W6 -- Selector lvalue (write side, including NBA + selector). LRM 11.5.1 write rules
      (fully-OOB no-op, partial-OOB only in-range bits, X / Z position no-op) all hold. Closes
      `datatypes/packed/indexed_part_select` for the write half.
- [ ] W14 -- A range-select read whose two bounds have different state domains aborts at runtime
      (e.g. `v[$bits(t)-1:1]`, where the `$bits`-derived bound is 4-state and the literal bound is
      2-state). The endpoint-ordering step compares the two bounds assuming a shared storage domain
      -- true for the common two-literal case, not when the bounds are typed differently. The fix
      reconciles the endpoints to a common domain before comparing; a broader audit should cover
      every comparison the selector lowering synthesizes. Pre-existing (a `localparam integer` bound
      reaches it too); surfaced by `$bits` in a part-select bound (`query-functions.md` Q1).

### Construction

- [x] W8 -- Concatenation read `{a, b, c}` (LRM 11.4.12). Result is unsigned; 4-state iff any
      operand is 4-state. Signed operands contribute raw bits; unsized literals are rejected at the
      frontend. Includes mixed widths, nested concat, signed operands, wide (>64-bit) results, and X
      / Z propagation.
- [x] W9 -- Replication `{N{x}}` (LRM 11.4.12.1). Multiplier is a constant non-negative non-X/Z
      literal. Zero multipliers are legal only inside a concat with at least one positive-sized
      sibling; the frontend's `void`-typed operand handling drops them naturally. Includes
      replication in concat, nested concat, nested replication, signed operands, wide results, and X
      / Z propagation. String replication lives in `datatypes.md` SC2.
- [x] W10 -- Destructuring assignment `{a, b, c} = rhs` / `<= rhs` (LRM 11.4.12 LHS form). RHS is
      evaluated once and bits are distributed MSB-first, so `{a, b} = {b, a}` swaps. Parts may be
      any writable lvalue (including W6 selector chains, e.g. `{a[7:4], b[7:4]} = rhs`). NBA form
      requires every part to be a structural target. Replication operands are rejected per LRM
      11.4.12.1. LRM 10.9 assignment-pattern LHS and LRM 11.4.14.3 streaming-unpack LHS are separate
      constructs, both out of scope.

### Assignment families

- [x] W11 -- Compound assignment `+= -= *= /= %= &= |= ^= <<= >>= <<<= >>>=`. LRM 11.4.1 "evaluate
      target only once" applies; falls out of the C++ eval-once rule on the compound expression.
      NBA + compound is rejected at slang parsing per LRM A.6.2. Closes
      `operators/compound_assignment` for whole-var, selector, and mixed-state target shapes.
- [x] W12 -- `++` / `--` (prefix and postfix, LRM 11.4.2). Behave as blocking assignments; postfix
      yields the operand's prior value, prefix yields the new value. Integer and real operands;
      selector chains (`array[i]++`, `++a[15:8]`) and observable structural roots are covered. NBA
      contexts (`b <= a++`, `var[i++] <= rhs`) evaluate the inc / dec exactly once at submit time.
      Replication / concatenation operands are rejected as targets per LRM 11.4.12.1.
- [x] W13 -- Compound assignment evaluates the left-hand side exactly once (LRM 11.4.1) for every
      target, including a side-effecting subscript (`a[f()] op= b`) at any nesting. Every write
      target -- whole var, array / string element, struct / union member -- is an op=-able
      write-back location, so compound lowers uniformly to `AssignExpr{target, op, value}` and the
      backend evaluates the single target once (C++ `op=`; a future LIR computes the address once).
      A string character write is a write-back proxy (`String::ElementRef`) and a union member write
      a reference to the active member (`Union::GetRef`); neither is a read-modify-write desugar at
      the lowering.

## Cross-references

- LRM anchors: 7.4.5 (indexing and slicing: vocabulary, invalid-index rules), 11.4.5 (case
  equality), 11.4.6 (wildcard equality), 11.4.10 (shift), 11.4.12 / 11.4.12.1 (concatenation,
  replication), 11.4.13 (set membership), 11.5.1 (bit-select / part-select / indexed part-select on
  packed).
- Unblocks: `control-flow.md` C11 (via W2), C12 (via W1 + W2); `integral.md` archive sweep relies on
  W4..W6 for sub-bit operands; `packed.md` P3..P5 extend the addressable-expression set with
  packed-struct field access and packed-union variant access.
