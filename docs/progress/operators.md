# Operators

Tracks scalar expression-operator work against `backend::cpp`. Covers archive items that share the
native render path (`RenderExprAsNative` in `src/lyra/backend/cpp/render_expr.cpp`):
`operators/binary`, `operators/unary`, `operators/shift_overflow`. Each archive item checkbox in
`architecture-reset.md` is checked when its `*.yaml` cases reproduce on the current pipeline.

## Sub-Steps

### Unary

- [x] U1 -- Scalar unary token wiring on `int`: `+`, `-`, `!`, `~`. Mirrors B1's structure via
      `UnaryOpToken` in `src/lyra/backend/cpp/render_expr.cpp`. Negative-literal shapes such as
      `int a = -7;` now flow through cpp emit; reductions remain Unsupported (see U3).
- [ ] U2 -- Pre/post increment `++`/`--`. Current `hir::UnaryOp`/`mir::UnaryOp` enums have no
      inc/dec variant; needs HIR/MIR shape decision (statement-level vs expression-level given SV
      side-effect ordering).
- [ ] U3 -- Scalar reductions on `int` operand (`&a`, `|a`, `^a`, `~&a`, `~|a`, `~^a` when `a` is
      `int`). Reduction unary works today only for packed lvalues via `RenderPackedReductionAssign`.
- [ ] U4 -- 4-state unary X/Z propagation (`operators/unary/four_state.yaml`). Depends on
      `logic`-operand unary path through the packed runtime.

### Binary

- [x] B1 -- Scalar token wiring for binary ops with direct C++ counterparts on `int`/`int -> int`:
      `- * / % == != < <= > >= & | ^ && ||`. Extends `BinaryOpToken` in
      `src/lyra/backend/cpp/render_expr.cpp`.
- [ ] B2 -- Scalar `~^`/`^~` (bitwise XNOR): rewrite as `~(lhs ^ rhs)` in the native render path. No
      new MIR shape.
- [ ] B3 -- Scalar `->` (implication) and `<->` (equivalence): rewrite to `(!(lhs) || (rhs))` and
      `(!(lhs)) == (!(rhs))`.
- [ ] B4 -- Scalar shifts `<<`, `>>`, `>>>` with LRM overflow semantics (`x << n` with `n >= width`
      yields 0; signed `>>>` is arithmetic). Closes `operators/shift_overflow` in
      `architecture-reset.md` as a side-effect.
- [ ] B5 -- Integer power `**`: runtime helper with LRM corner cases (`0**0 = 1`, negative exponent
      yields 0).
- [ ] B6 -- Mixed-width / packed-literal operands (`byte + int`, `32'h... < int`): route through the
      packed top-level binary path, not native `int` token path. Closes archived cases
      `addition_bit_and_int`, `comparison_signed_*`, `comparison_mixed_width_*`.
- [ ] B7 -- 4-state X/Z propagation for binary ops (`operators/binary/four_state.yaml`). Depends on
      a `logic`-operand binary path through the packed runtime.

## Out of Scope

- Case equality `===`/`!==` and wildcard equality `==?`/`!=?` -- separate archive items
  (`operators/case_equality`, `operators/wildcard_equality`).
- `operators/binary_string` -- string compare runtime helpers, separate item.
- Concat, replicate, inside, compound assignment -- new MIR shapes; each is its own archive item and
  its own workstream.
