# Integral Representation

Replace the form-driven, multi-type integral emit in `backend::cpp` with a single
`lyra::value::PackedArray` class that carries `(bit_width, is_signed, is_four_state)` and owns every
operator on integral values. Closes the silent-correctness bugs the prior cuts targeted without
introducing the five-bucket dispatch and view bridges. Background and trade-offs in
`../decisions/integral-representation.md`.

Done when:

- Every SystemVerilog integral type (`byte`, `shortint`, `int`, `longint`, `integer`, `time`, any
  `bit [N:0]`, any `logic [N:0]`, any `reg [N:0]`) emits as `lyra::value::PackedArray` and
  round-trips through `expect.variables`.
- `RenderTypeAsCpp`, the single `RenderExpr` path, `RenderRuntimeValueViewInit`, and related helpers
  no longer dispatch on `PackedArrayForm` or carry separate native / container code paths.
- The archive item set under `operators/binary`, `operators/unary`, `operators/shift_overflow`,
  `datatypes/integral`, `datatypes/packed`, `datatypes/wide_integral` reproduces.

## Defects this workstream closes

- `integer` and `time` silently lose X/Z because their old mapping was a 2-state native int.
- `byte`, `shortint`, `longint` were unrenderable in `backend::cpp`.
- `ConversionExpr` is stripped in cpp emit, hiding sign-extend / zero-extend bugs.
- `bit [N:0]` arithmetic, comparison, and shift are Unsupported because the packed path lacks these
  op families.
- Native and packed paths in cpp emit dispatch on `PackedArrayForm` (a syntactic origin marker),
  creating two parallel op surfaces that must be kept in sync.

## Cuts

### Cut 1 -- `PackedArray` skeleton + type emit redirect

Add `lyra::value::PackedArray` with the three-attribute constructor, internal SBO storage, the
existing copy / set / view-accessor surface (re-exposed for backwards interop with the format
pipeline), and a thin set of methods needed for declarations and assignments. `RenderTypeAsCpp`
emits `lyra::value::PackedArray` for every integral type. `RenderRuntimeValueViewInit` builds its
`RuntimeValueView` from `PackedArray` uniformly. No operator coverage yet; everything else in
`backend::cpp` continues to compile because variable declarations and `$display` are the only
consumers that need PackedArray at this point.

- [x] J1 -- Add `lyra::value::PackedArray` class. Storage variant: inline word(s) for
      `bit_width <= 64`, heap multi-word for `>64`; 4-state carries an unknown plane the same way.
- [x] J2 -- `RenderTypeAsCpp` returns `lyra::value::PackedArray` for every `PackedArrayType`,
      regardless of `form`. Catch-all unrenderable type returns `diag::Unsupported`.
- [x] J3 -- Container init helper retires (only `PackedArray` exists; every integral declares with
      `{bit_width, is_signed, is_four_state}`).
- [x] J4 -- `RenderRuntimeValueViewInit` produces its `RuntimeValueView` from `PackedArray`.
      `NarrowIntegral` / `FromBitView` / `FromLogicView` becomes one path.
- [ ] J5 -- `tests/cases/cpp/` declaration cases for `byte` / `shortint` / `longint` / `time` /
      `integer` (with X/Z); update / replace any existing tests that asserted native-int output
      text.

### Cut 2 -- Operator coverage on `PackedArray`

Add operator overloads / methods so cpp emit can render `BinaryExpr` and `UnaryExpr` directly
against `PackedArray`. The dispatch in `RenderExpr*` collapses to a single path.

- [x] J6 -- Bitwise (`&`, `|`, `^`, `~^`, `~`) as member operators or method on `PackedArray`.
- [x] J7 -- Arithmetic (`+`, `-`, `*`, `/`, `%`, `**`) with LRM corner cases (div-by-zero, power
      semantics). Narrow (`<=64`) only; wide arithmetic still throws `InternalError`.
- [x] J8 -- Comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`) returning a 1-bit `PackedArray`.
- [x] J9 -- Shift (`<<`, `>>`, `>>>`) with LRM overflow (`amount >= bit_width` yields 0 /
      sign-fill).
- [x] J10 -- Logical (`&&`, `||`, `!`) returning a 1-bit `PackedArray`. `->` and `<->` are a
      separate scalar-emit token wiring step tracked in `operators.md`.
- [x] J11 -- Reduction (`&a`, `|a`, `^a`, `~&a`, `~|a`, `~^a`) as `PackedArray` methods.
- [x] J12 -- 4-state X/Z propagation across arithmetic, comparison, shift, power, logical, and unary
      `-`. Bitwise / reduction already propagate via the view-based truth-table path. Narrow only.
- [ ] J13 -- Archive sweep: reproduce `operators/binary/default.yaml`, `operators/unary/...`,
      `operators/shift_overflow/...`, and their `four_state.yaml` companions.

### Cut 3 -- Expression dispatch consolidation

Remove `RenderExprAsNative` / `RenderExprAsPackedTopLevel` split. Single `RenderExpr` path, single
`ConversionExpr` arm.

- [x] J14 -- One `RenderExpr` path. `IsPackedExplicit`, `NeedsRuntimeContainerInit`, the
      `MakeBitView` / `BitViewToInt64` bridges (if they exist by then) all retire.
- [x] J15 -- `ConversionExpr` collapses to "construct a destination `PackedArray` from a source
      `PackedArray`" -- one case, handled by `PackedArray` itself.

## Cross-references

- Decision and findings: `../decisions/integral-representation.md`
- LRM anchors: 6.11 (Table 6-8 integer types), 6.11.2 (4-state to 2-state convert), 10.7 (assignment
  extension / truncation), 11.6 (expression bit lengths), 11.7 (signed expressions), 11.8
  (expression evaluation rules, 11.8.4 for x/z handling).
- Archive items closed: `operators/binary`, `operators/unary`, `operators/shift_overflow`,
  `datatypes/integral`, `datatypes/packed`, `datatypes/wide_integral`.
- Slang reference: `slang/ast/types/AllTypes.h:IntegralType`.
- Legacy archive reference: `archived/include/lyra/common/type.hpp:IntegralInfo`.

## Relationship to `operators.md`

`operators.md` now tracks the scalar-emit residue that the integral runtime does not own: items that
need either a new HIR/MIR shape (`++`/`--`) or a cpp-emit token rewrite (`~^` / `^~` binary, `->`,
`<->`, scalar reductions on `int`). The runtime side of every other binary / unary operator family,
including 4-state X/Z propagation, lives here.
