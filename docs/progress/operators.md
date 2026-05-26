# Operators

Tracks scalar emit-token wiring and HIR/MIR shape work for operators that the unified
`lyra::value::PackedArray` runtime does not by itself cover. The integral runtime side --
arithmetic, comparison, shift, power, logical, reduction, bitwise, including 4-state X/Z propagation
-- is tracked in `integral.md`. This file is the home for the items that need either a new MIR shape
or a cpp-emit token rewrite. Each archive item checkbox in `architecture-reset.md` is checked when
its `*.yaml` cases reproduce on the current pipeline.

## Sub-Steps

### Unary

- [x] U1 -- Scalar unary token wiring on `int`: `+`, `-`, `!`, `~`. Via `UnaryOpToken` in
      `src/lyra/backend/cpp/render_expr.cpp`.
- [ ] U2 -- Pre/post increment `++`/`--`. Current `hir::UnaryOp`/`mir::UnaryOp` enums have no
      inc/dec variant; needs HIR/MIR shape decision (statement-level vs expression-level given SV
      side-effect ordering).
- [ ] U3 -- Scalar reductions on `int` operand (`&a`, `|a`, `^a`, `~&a`, `~|a`, `~^a` when `a` is
      `int`). The reduction method exists on `PackedArray`; only the cpp-emit token mapping for the
      narrow `int` shape needs to route through it.

### Binary

- [x] B1 -- Scalar token wiring for binary ops with direct C++ counterparts on `int`/`int -> int`:
      `- * / % == != < <= > >= & | ^ && ||`. Extends `BinaryOpToken` in
      `src/lyra/backend/cpp/render_expr.cpp`.
- [ ] B2 -- Scalar `~^`/`^~` (bitwise XNOR) as a binary operator (reductions of these tokens already
      work). The runtime side is `PackedArray::BitwiseXnor`; only the emit-token mapping from
      `mir::BinaryOp` is missing.
- [ ] B3 -- Scalar `->` (implication) and `<->` (equivalence): rewrite to `(!(lhs) || (rhs))` and
      `(!(lhs)) == (!(rhs))` in cpp emit, or add dedicated methods on `PackedArray`.

## Out of Scope

- Case equality `===`/`!==` and wildcard equality `==?`/`!=?` -- separate archive items
  (`operators/case_equality`, `operators/wildcard_equality`).
- `operators/binary_string` -- string compare runtime helpers, separate item.
- Concat, replicate, inside, compound assignment -- new MIR shapes; each is its own archive item and
  its own workstream.
- Mixed-width and shift/power/4-state correctness on the integral runtime -- tracked in
  `integral.md`.
