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

- [x] W3 -- Case equality `===` / `!==`. `BinaryOp::kCaseEquality` / `kCaseInequality` route through
      `PackedArray::CaseEqual` (bit-exact 4-state compare, X matches X, Z matches Z, X does not
      match Z). Closes `operators/case_equality/default.yaml` and `four_state.yaml` including
      2-state operands, mixed X/Z patterns, and width promotion.

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
these operand types"). Two nodes keep name and validity domain in lockstep.

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

The IR keeps source-form positions verbatim and defers all positional arithmetic to render. No
constant evaluation or direction normalization happens at lowering -- slang has already validated
the LRM contract during AST construction (constants are constant, indexed widths are positive,
simple-form direction matches the operand's declared range; partial-OOB stays in the AST as a
warning), and the result `Expr.type` carries the slice's width. MIR mirrors HIR 1:1.

The bit width of one element of the operand at any select layer is derived at render time from the
operand's `PackedArrayType.dims` stack:
`element_bw = operand.BitWidth() / operand.dims.front().ElementCount()`. For a 1D vector this is 1
(the LRM 11.5.1 bit-select case); for a multi-dim packed operand it is the inner dimensions' total
bit width. Render multiplies the source-form index/lsb by this factor to translate from
element-space to the flat-bit storage offset. The IR does not duplicate this derived value.

Naming:

- `ElementSelectExpr` / `RangeSelectExpr` match slang's `ElementSelectExpression` /
  `RangeSelectExpression`. LRM 7.4.5 vocabulary aligns with slang on these names. **Not**
  "BitSelect": whether the result is 1 bit is operand-derived, not an operator property.

#### Write-side `Lvalue` shape

A writable place is a root storage (var ref) plus an ordered chain of selectors that project into
it. An empty chain is a whole-var write; one or more selectors describe nested partial writes that
render flattens into a single LRM-bit-level partial assignment.

```
struct ElementLvalueSelector { ExprId index; };
struct RangeLvalueSelector   { RangeBounds bounds; };
using LvalueSelector = std::variant<ElementLvalueSelector, RangeLvalueSelector>;

struct Lvalue {
  std::variant<StructuralVarRef, ProceduralVarRef /* + LoopVarRef in HIR */> root;
  std::vector<LvalueSelector> selectors;  // outer-first
};
```

The chain shape (vector, not optional) reflects the LRM concept: a selector is a path into the root,
and the path can have any number of layers. AST -> HIR walks the slang nested AST peeling
outermost-first to build the chain. Per-layer element bit widths are not stored in the IR; render
walks the root's `PackedArrayType.dims` stack alongside the chain (each Element selector pops one
outer dim; each Range selector keeps the inner dims) to compute the factor when it needs one.

#### Runtime `PackedArray` API

`value::PackedArray` carries the source-level dim stack (`dims_`, outer-first) alongside its flat
bit storage. The dim stack is the API contract; the underlying storage layout (flat bit planes
today; potentially canonical-byte-aligned, vector-of-elements for huge outer dims, or any other
shape once optimization work begins) is private. The whole API is element-level -- `ElementAt` and
`Slice` take positions in the operand's outer-element units, and dispatch on `dims_` internally to
turn them into bit offsets. Emit never bakes element-width arithmetic into call sites, so future
storage refactors do not propagate beyond runtime internals. The chain API is uniformly method-style
(no `operator[]` overload) so every layer of a selector chain reads the same way.

```
// Low-level bit-level primitives (chain leaves resolve here).
auto ExtractBits(const PackedArray& lsb_bit, std::uint32_t bit_width) const
    -> PackedArray;
auto AssignSlice(const PackedArray& lsb_bit, std::uint32_t bit_width,
                 const PackedArray& value) -> void;

// Element-level chain entry points. Positions are in outer-element units;
// the runtime scales by element bit width derived from `dims_`.
auto ElementAt(const PackedArray& idx) -> PackedArrayRef;        // non-const
auto ElementAt(const PackedArray& idx) const -> PackedArray;     // const
auto Slice(const PackedArray& lsb_in_outer_elements,
           std::uint32_t count_in_outer_elements) -> PackedArrayRef;       // non-const
auto Slice(const PackedArray& lsb_in_outer_elements,
           std::uint32_t count_in_outer_elements) const -> PackedArray;    // const
```

For a 1D operand (`dims_.size() == 1`), one "element" is one bit, so `ElementAt` is the LRM 11.5.1
bit-select and `Slice` is the LRM 11.5.1 part-select at bit-level. For multi-dim, one "element" is
the inner subtype, and the runtime multiplies by the element bit width internally.

`PackedArrayRef` is a writable proxy carrying
`(PackedArray* root, PackedArray bit_offset, uint32_t bit_width, vector<PackedRange> dims)`.
Implicit `operator PackedArray()` materializes a fresh value via `root.ExtractBits`;
`operator=(const PackedArray&)` routes through `root.AssignSlice`. The same two chain methods
compose into another `PackedArrayRef` with the dim stack's outer popped (`ElementAt`) or its outer
count replaced (`Slice`). `bit_offset` is kept in a canonical 64-bit signed 4-state shape so chained
`+` / `*` between layers never collide on shape, and X/Z in any layer's index propagates to the
final write (LRM 11.5.1 "X/Z position is a no-op").

LRM 11.5.1 corner cases (X/Z position no-op, fully-OOB no-op, partial-OOB only in-range bits) live
inside `AssignSlice` and `ExtractBits`. The proxy chain only accumulates; the final read or write
call enforces the rules.

#### Observable-target write proxy

`Var<T>` is deliberately a thin storage + subscriber wrapper. It exposes `Get()` for reads and is
written through:

- `runtime::WriteVar(services, var, value)` -- whole-var commit + change detection + event firing.
- `var.Mutate(services)` -- partial commit. Returns a `ScopedMutation<T>` RAII handle that snapshots
  the current value, forwards `ElementAt` / `Slice` to the snapshot so a normal chain composes, and
  commits the (possibly mutated) snapshot through `WriteVar` in its destructor.

`ScopedMutation<T>` is non-copyable and non-movable: the contract is that the handle lives only
until the end of the constructing full expression. Returning it by value from `Var<T>::Mutate`
relies on C++17 mandatory copy elision -- the prvalue is materialized in the caller's storage with
no copy or move. The destructor runs at the statement's semicolon, after the chain has finished
mutating the snapshot, and routes the result through `WriteVar` so subscribers fire on change.

The selector chain itself stays on `value::PackedArray` / `PackedArrayRef`. `Var<T>` has no chain
API of its own; `Mutate` is purely the envelope that turns "mutate a snapshot then commit" into a
single chain expression. The shape `var.Mutate(svc).ElementAt(...).Slice(...) = rhs` is identical to
a procedural-local chain except for the `Mutate(svc)` adapter and the implicit commit at the end of
the statement.

#### Render rules

Render emits a chain of method calls; the runtime composes them. Render never computes element bit
widths -- the runtime decides via the operand's `dims_`. The chain methods are simply `ElementAt`
and `Slice`, both taking source-form outer-element positions:

Read (recursive compose -- each layer's render renders its `base_value` and appends its own method
call):

- `mir::ElementSelectExpr { base, index }` -> `(<base>).ElementAt(<index>)`.
- `mir::RangeSelectExpr` with `RangeConstantBounds { msb, lsb }` ->
  `(<base>).Slice(<lsb>, <count>U)` where `count = |msb - lsb| + 1` (slang elaborates msb and lsb to
  integer literals; render extracts them).
- `mir::RangeSelectExpr` with `RangeIndexedUpBounds { base_index, width }` ->
  `(<base>).Slice(<base_index>, <count>U)` where `count` = literal value of `width`.
- `mir::RangeSelectExpr` with `RangeIndexedDownBounds { base_index, width }` ->
  `(<base>).Slice((<base_index>) - <count-1 same-shape lit>, <count>U)`.

Write (iterative chain -- walk `lvalue.selectors`, append each layer's method call, end with
`= rhs`):

- Procedural-local target root: chain mutates the variable in place. Emit shape:
  `<root>.ElementAt(<idx>).Slice(<lsb>, <count>U) = <rhs>;`.
- Observable structural-var root: insert `.Mutate(*services_)` after the root so the
  `ScopedMutation` temporary commits through `WriteVar` at end-of-statement. Emit shape:
  `<root>.Mutate(*services_).ElementAt(<idx>).Slice(<lsb>, <count>U) = <rhs>;`.

Both branches share `RenderSelectorMethodCall` per layer; the only difference is the single
`.Mutate(*services_)` adapter inserted between the root and the first selector when the root is
observable.

#### Variable declaration emit

`PackedArray` exposes two constructors: a 1D shorthand `(bit_width, signed, four_state)` and a
dim-list `(initializer_list<PackedRange>, signed, four_state)`. Render emits the 1D form for
single-dim packed types (`bit [N-1:0]`) and the dim-list form for multi-dim packed types
(`bit [N-1:0][M-1:0]`...) so the runtime instance carries the correct `dims_` stack.

#### LRM 11.5.1 / 7.4.5 corner-case rules (handled inside `PackedArray::ExtractBits` / `AssignSlice`)

Read:

- Any X / Z bit in the runtime `lsb_bit` propagates: result is all-X of `width` bits (4-state) or
  all-0 (2-state).
- Fully out-of-range read returns all-X / all-0.
- Partially out-of-range read returns in-range bits verbatim and X / 0 for the OOB bits.

Write:

- Any X / Z bit in `lsb_bit` makes the entire write a silent no-op (the LRM "no effect on the data
  stored when written" rule).
- Fully out-of-range write is a silent no-op.
- Partially out-of-range write affects only the in-range bits; bits outside the operand's bounds are
  silently discarded.

Bit-select / part-select of a scalar or real variable is illegal; slang rejects at AST construction,
so HIR never sees the shape.

#### NBA on selector lvalues

`data[i+:8] <= v` routes through the same closure-based NBA machinery as a whole-var NBA. Each
`ExprId` reached through the selector chain is handled one of two ways:

- **Integer literals** are cloned verbatim into the closure body's expr store -- no capture entry is
  created, since the value cannot change between submit and fire.
- **Non-literal expressions** are captured by value into a fresh procedural var binding via
  `ByValueCapture`. Each capture gets a distinct name (`_lyra_nba_idx0`, `_lyra_nba_idx1`, ...)
  drawn from a counter that advances only on non-literal snapshots, so the emitted capture list has
  no collisions. The capture-name suffix is sequential among non-literal captures but may skip
  indices implicitly when literals appear between them in the selector chain.

Inside the closure body the chain is rebuilt with body-local refs (`ProceduralVarRef` for captures,
`IntegerLiteral` clones for literals), so when the NBA region fires it re-evaluates none of the
original outer-scope expressions -- the snapshotted values at submit time are what get written.

If the NBA target is observable, the body's chain inserts `.Mutate(*services_)` after the root (same
pattern as the blocking observable selector write) so the NBA-fired write commits through `WriteVar`
when the `ScopedMutation` temporary tears down at end-of-statement and triggers subscribers. The
closure stays a single lambda -- there is no nested mutation lambda.

- [x] W4 -- Element-select read `v[idx]`. Wired end-to-end through `hir::ElementSelectExpr` ->
      `mir::ElementSelectExpr` -> `PackedArray::Slice(<scaled>, ebw)`. Handles 1D bit-select (ebw=1)
      and multi-dim element-select (ebw=N) uniformly: render derives `ebw` from the operand's
      `PackedArrayType.dims` stack, so the IR carries no dim-derived duplicate. Chain composition is
      via nested `Slice` calls on `base_value`. Implements LRM 7.4.5 / 11.5.1 OOB and X / Z
      propagation rules on the runtime side.

- [x] W5 -- Range-select read covering all three bounds forms: non-indexed constant `v[msb:lsb]`,
      indexed-up `v[base +: w]`, indexed-down `v[base -: w]`. Wire `mir::RangeSelectExpr` mirroring
      HIR (three `RangeBounds` variants). HIR -> MIR is a pure structural pass-through. cpp render
      derives the operand's element bit width from `PackedArrayType.dims`, dispatches on the MIR
      bounds variant, scales positions, and emits a single `PackedArray::Slice` call. For multi-dim,
      the scaling translates element-space positions to bit positions; for 1D (`ebw==1`) the scaling
      is dropped. Runtime reuses `PackedArray::Slice`. Width at render comes from the result type.

- [x] W6 -- Selector lvalue (write side, including NBA + selector). Extended HIR / MIR `Lvalue` to
      carry a root `*Ref` plus a `vector<LvalueSelector>` chain. Added
      `PackedArray::AssignSlice(lsb_bit, width, value)` and the `ElementAt` / `Slice` chain entries
      on `PackedArray` / `PackedArrayRef`. Observable selector writes route through
      `Var<T>::Mutate(services)` which returns a `ScopedMutation<T>` RAII handle: it snapshots
      `var.Get()`, forwards chain methods, and commits via `WriteVar` in its destructor (relying on
      C++17 mandatory copy elision for the prvalue return). NBA on selector lvalues snapshots every
      non-literal `ExprId` in the chain into the closure body via `ByValueCapture`; integer literals
      are cloned verbatim. Render walks the chain and emits one method call per layer, with
      `.Mutate(*services_)` inserted between root and first selector when the root is observable.
      LRM 11.5.1 write rules (fully-OOB no-op, partial-OOB only in-range bits, X / Z position no-op)
      live inside `AssignSlice`. Closes `datatypes/packed/indexed_part_select/default.yaml` for the
      write half.

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
      temp and distributes its bits MSB-first into each sub-target via the same selector chain
      machinery W6 provides.

### Assignment families

- [x] W11 -- Compound assignment `+= -= *= /= %= &= |= ^= <<= >>= <<<= >>>=`. Compound stays as
      compound through HIR and MIR: `AssignExpr` carries an `optional<BinaryOp> compound_op` field
      that is `nullopt` for a simple `=` and set for `op=`. Slang's expanded
      `BinaryOp(LValueRef,     user_rhs)` tree is consumed at AST -> HIR -- the helper extracts the
      user-written rhs from the side that does not wrap `LValueReference`, then re-types it to the
      lhs type so the runtime stays at one shape (slang's per-operand promotion Conversions become
      elaboration artifacts that lowering does not reproduce). C++ emit produces `<chain> op= rhs`
      directly: `PackedArray`, `PackedArrayRef`, and `ScopedMutation<PackedArray>` each overload the
      arithmetic / bitwise compounds as `operator+=` etc., and the three shifts as `ShiftLeftAssign`
      / `LogicalShiftRightAssign` / `ArithmeticShiftRightAssign`. The LRM 11.4.1 "evaluate target
      only once" rule falls out of the C++ standard's eval-once rule on the compound assignment
      expression; no HIR-level snapshot pass is needed. NBA + compound is rejected at slang parsing
      (LRM A.6.2); InternalError catches grammar drift. Closes `operators/compound_assignment` for
      whole-var, selector, and mixed-state target shapes (`x += y`, `x[7:4] |= v`, `x[i +: 4] += k`,
      `int %= logic[3:0]`).

- [ ] W12 -- `++` / `--` (prefix and postfix). Add `hir::IncrDecrStmt` and `hir::IncrDecrExpr`
      (separate from `AssignExpr` because the expression form returns the pre- or post-value). Once
      `++` / `--` are accepted in index positions, the same eval-once rule will need explicit
      handling at the read-modify-write level; the natural home for that work is LIR's address-once
      lowering rather than an HIR snapshot pass.

## Cross-references

- LRM anchors: 7.4.5 (indexing and slicing of arrays: vocabulary, invalid-index rules), 11.4.5 (case
  equality), 11.4.6 (wildcard equality), 11.4.10 (shift), 11.4.12 / 11.4.12.1 (concatenation,
  replication), 11.4.13 (set membership), 11.5.1 (bit-select / part-select / indexed part-select on
  packed).
- `integral.md` J14 archive sweep depends on W4..W6 for any binary / shift-overflow case that
  selects sub-bits before applying the operator.
- `control-flow.md` C11 unblocked by W2; C12 unblocked by W1 (basic) and W2 (wildcard items).
- `packed.md` P3..P5 use W6's selector chain shape and add field / variant selectors on top; multi-
  dim packed array selectors are handled here, not in `packed.md`.
