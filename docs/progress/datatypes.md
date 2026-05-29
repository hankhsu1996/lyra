# Datatypes

Tracks SystemVerilog data type coverage on the current pipeline. The integral family (`int`, `byte`,
`shortint`, `longint`, `time`, `integer`, `bit [N:0]`, `logic [N:0]`) lives in `integral.md` because
its work is a single PackedArray-runtime cut sequence. This file covers every other family:
`datatypes/enum`, `datatypes/string`, `datatypes/unpacked`, `datatypes/general` (dynamic array /
queue / associative), `datatypes/real`, `datatypes/default_init`, `datatypes/representation`.

Each archive item checkbox in `architecture-reset.md` is checked when its `*.yaml` cases reproduce
on the current pipeline.

## Actionable

Real C1 / C2 / C3 / C4 are complete. Other families remaining: `datatypes/string`,
`datatypes/unpacked`, `datatypes/general`, `datatypes/default_init`, `datatypes/representation`.

## Enum

Covers archive item `datatypes/enum` (sub-folders `enum`, `enum_implicit_conversion`,
`enum_methods`). Slang models enum as `EnumType : public IntegralType, public Scope`. Our HIR
mirrors that with a `hir::EnumType` carrying a `base_type` (a `PackedArrayType`) and an ordered
member table. HIR -> MIR aliases the enum's MIR `TypeId` to its base type's so MIR and the cpp
backend see only `lyra::value::PackedArray`.

Enum methods are modelled by the general "types own method tables" mechanism: `EnumType.methods`
carries six `hir::Method` entries (each with `hir::BuiltinMethod` data), populated at AST -> HIR
enum type construction. Call dispatch goes through `hir::MethodRef{receiver_type, method_id}` -- a
`SubroutineRef` variant alternative that is the dot-syntax counterpart of `StructuralSubroutineRef`
(free user calls) and `SystemSubroutineRef` (system tasks). The same mechanism is the path for
future `string` / `queue` builtins and user class methods; only the `Method::data` variant
alternative differs (`BuiltinMethod` for LRM intrinsics, `StructuralMethod` for user-defined
bodies).

Behavior lives in the runtime SDK (`include/lyra/value/enum_ops.hpp`): six hand-written `inline`
functions (`EnumFirst` / `EnumLast` / `EnumNum` / `EnumName` / `EnumNext` / `EnumPrev`) operating on
a generic `EnumMetadata{members, is_four_state}`. HIR -> MIR translates `MethodRef` to a
`mir::BuiltinMethodCallee{enum_metadata_id, kind}` callee and populates a per-enum-type
`EnumMetadata` entry on `mir::CompilationUnit::enum_metadata` (cached by HIR `TypeId`). cpp emit
walks `enum_metadata` to emit `inline constexpr lyra::value::EnumMember[]` plus an
`inline constexpr lyra::value::EnumMetadata` global per entry at TU top, and renders each call site
as `lyra::value::Enum<Method>(lyra_enum_metadata_N, ...)`. The architecture choice between inline
expansion vs runtime ops library is documented in `enum-method-architecture.md` (root).

- [x] E1 -- Type plumbing, member references, method dispatch. `LowerType` recognises
      `slang::ast::EnumType` and populates the enum type's six-entry method table (`first` / `last`
      / `num` / `next` / `prev` / `name`). AST `NamedValueExpression` whose symbol kind is
      `EnumValue` lowers directly to `hir::IntegerLiteral` of the backing type. `v.first()` /
      `v.last()` / `v.num()` lower at AST -> HIR via a uniform call dispatch that looks up the
      method by name on the receiver's type, producing
      `CallExpr{callee=MethodRef{receiver_type, method_id}, arguments=[receiver]}`. HIR -> MIR
      translates each `MethodRef` to a `mir::BuiltinMethodCallee` referring to the receiver type's
      `EnumMetadata`. HIR -> MIR's `TranslateTypeData` has an `EnumType` arm that copies the base's
      translated `mir::TypeData`, so the orchestrator stays uniform. Conversions enum -> integral
      are transparent; integral -> enum requires explicit cast (LRM 6.19.3). Covers all 11 cases of
      `enum/default.yaml` (basic, sequential, explicit, mixed, comparison, arithmetic, explicit base
      type, display, range count, range bounds, range mixed);
      `enum_implicit_conversion/default.yaml` cases that are legal under LRM 6.19.3
      (`explicit_cast_works`, `enum_to_int_unchanged` via `enum_arithmetic`); and
      `enum_methods/default.yaml` cases for `first` / `last` / `num` / `num_auto_values`. Also ships
      `enum_passed_to_system_function` as a regression guard for the AST -> HIR Call dispatch (an
      enum-typed first argument to a `$`-prefixed system call must fall through to the system
      subroutine path when no method of that name exists on the receiver). Unblocks C16 in
      `control-flow.md`.
- [x] E2 -- Runtime methods `next` / `prev` with optional step (LRM 6.19.5.3 / 6.19.5.4). AST -> HIR
      produces `CallExpr{callee=MethodRef{...}, arguments=[receiver, step?]}`; AST -> HIR fills in
      the default `1` for the missing step (each enum's method table carries the formal parameter
      with its default `IntegralConstant`). HIR -> MIR translates `MethodRef` to a
      `BuiltinMethodCallee` referencing the type's `EnumMetadata` plus the method kind. cpp emit
      renders the call as `lyra::value::EnumNext(metadata, receiver_value, step)` /
      `lyra::value::EnumPrev(metadata, receiver_value, step)` and wraps the `std::int64_t` result in
      a `PackedArray` of the enum's base shape. Non-member receivers fall through the runtime lookup
      to a zero default (Table 6-7 4-state X behavior is a runtime gap tracked separately). Covers
      `enum_methods/default.yaml` cases `enum_next`, `enum_next_wrap`, `enum_prev`,
      `enum_prev_wrap`, `enum_next_step`, `enum_next_step_wrap`, `enum_prev_step`,
      `enum_prev_step_wrap`, plus `enum_next_runtime_step` as a regression guard for non-literal
      step.
- [x] E3 -- Method `name()` (LRM 6.19.5.6). Returns the string name of the current value, empty
      string when the value is not a member. cpp emit renders the call as
      `lyra::value::EnumName(metadata, receiver_value)`; the SDK function returns `std::string`
      directly. Covers `enum_methods/default.yaml` case `enum_name`.

### Cross-references

- LRM 6.19 (Enumerations), 6.19.3 (conversions), 6.19.5 (methods).
- Archive items: `datatypes/enum/{enum,enum_implicit_conversion,enum_methods}`.
- Unblocks: C16 (enum-typed `case` selectors) in `control-flow.md`.
- Slang reference: `slang/include/slang/ast/types/AllTypes.h` -- `EnumType`, `EnumValueSymbol`.
- Legacy archive reference: `archived/include/lyra/common/type.hpp` -- `EnumInfo`, `EnumMember`.

## Real

Covers archive item `datatypes/real` (sub-folders `real_types`, `shortreal_types`,
`realtime_types`). Per LRM 6.12, `real` is C `double`, `shortreal` is C `float`, and `realtime` is
synonymous with `real`. Three runtime variants on `RuntimeValueView` (`Real64ValueView`,
`Real32ValueView`) plus IEEE 754 round-trip-precision literal rendering (17 sig-digits for double, 9
for float). Real values bypass the `Var<T>` wrapper because LRM 6.12 forbids edge event controls on
real variables.

- [x] C1 -- Declarations, blocking assignment, and `$display` formatting (`%f` / `%e` / `%g`). HIR
      `RealLiteral` and MIR `RealLiteral` carry a `double` value; cpp emit renders `real` and
      `realtime` to `double`, `shortreal` to `float`. `support::FormatDirectiveKind` and
      `mir::FormatKind` split into `kRealDecimal` / `kRealExponential` / `kRealGeneral` so each
      conversion specifier is a distinct semantic kind. `lower_print.cpp` routes the three real
      kinds through `RuntimeValueView` real variants; `value::FormatValue` formats them via
      `std::format` with `{:.{}f}`, `{:.{}e}`, `{:.{}g}` and default precision 6 (matches printf and
      LRM Table 21-2). Covers the declaration / assignment / display cases across all three archive
      subfolders (`real_declaration_*`, `*_local_variable`, `*_assignment` for
      `real`/`shortreal`/`realtime`).
- [x] C2 -- Operators legal on real per LRM 11.3.1 plus structural-level initializers
      (`real a = 1.5;` at module scope). cpp emit splits `RenderBinaryOp` / `RenderUnaryOp` into
      integral and real renderers. Real arithmetic uses native C++ ops; `**` uses `std::pow` (C++
      overload resolution picks `(float, float) -> float` and `(double, double) -> double`, matching
      LRM 11.3.1 result typing). The C++ bool result of a real relational / equality / logical op is
      wrapped in `lyra::value::PackedArray::FromInt(..., 1, false, false)` so the runtime sees the
      1-bit integral shape the rest of the pipeline expects. shortreal <-> real conversions go
      through `ConversionExpr`: same-precision is pass-through; cross-precision emits
      `static_cast<float>` / `static_cast<double>` so C++ never sees an implicit narrowing.
      `<cmath>` joins the emit prologue. Structural-level initializers are wired end-to-end:
      `hir::StructuralVarDecl` carries an optional `initializer` ExprId; AST -> HIR extracts
      `var.getInitializer()` for real-family types via `LowerStructuralExpr`; HIR -> MIR appends an
      `ExprStmt(AssignExpr)` per initialized var into the scope's `constructor_scope`; cpp emit's
      existing fall-through `(target = value)` arm lands the assignment in the C++ constructor body.
      This is safe for real-family vars because they do not wrap in `Var<T>` and therefore do not
      need `services_`. Integral structural initializers remain a known gap (their
      `Var<PackedArray>` write path needs `services_`, which is not bound until `Bind()`).
- [x] C3 -- Cross-family conversions. cpp emit's `RenderConversionExpr` gains an integral -> real
      arm (`static_cast<double>((operand).ToInt64())`; `PackedArray::ToInt64` collapses X / Z bits
      to 0 per LRM 6.12.1) and a real -> integral arm
      (`PackedArray::FromInt(std::llround(operand), <shape>)`; `std::llround`'s round-half-away-
      from-zero rule matches LRM 6.12.1 exactly, contrasting with the archive's incorrect LLVM
      `FPToSI` truncate path). Same-shape and unhandled-pair conversions fall through to the operand
      unchanged: slang emits identity conversions (e.g., `string` -> `string`) and string-literal
      lifts (`bit[N-1:0]` -> `string`) where the operand already renders as the destination's C++
      type. Widths > 64 bits are caught by `PackedArray::FromInt` / `ToInt64`'s own width invariants
      (follow-up: wide-int conversion via the FromWords path). Drive-by fix: `PackedArray::ToInt64`
      previously read `ValueWords()` raw, leaving X / Z positions whatever bit pattern slang stored
      in the value plane; it now masks by `~UnknownWords()` so its docstring's "X/Z bits map to 0"
      promise actually holds. Tests: `int_to_real`, `int_to_real_negative`,
      `real_to_int_round_down`, `real_to_int_round_up`, `real_to_int_half_positive`,
      `real_to_int_half_negative`, `int_to_shortreal`, `shortreal_to_int`, `int_literal_to_real`,
      `byte_to_real`, `longint_to_real`, `logic_xz_to_real`.
- [x] C4 -- LRM-illegal forms on real (LRM 6.12 + 11.3.1 / Table 11-1). Slang filters 10 of 11 at
      AST construction (edge event control on real, bit-select / part-select of real, real index in
      bit-select, modulus `%`, bitwise / reduction / shift, wildcard `==?` / `!=?`, plus the
      corresponding compound assignment forms). The one slang anomaly is case equality `===` / `!==`
      on real (slang allows `bothNumeric` even though LRM Table 11-1 lists "Any except real and
      shortreal"). That path falls through `RenderBinaryOpReal`'s default arm, which already returns
      `diag::Unsupported` with an LRM 11.3.1 citation -- so the C2 defensive renderer is the
      rejection mechanism with no additional code required. Negative test
      `errors/real_case_equality_unsupported` guards this contract.

### Cross-references

- LRM 6.12 (Real, shortreal, realtime), 6.12.1 (Conversion), 11.3.1 (Operators with real operands),
  Table 11-1 (Operators and data types), 11.4.3 (Arithmetic), 11.4.4 (Relational), 11.4.5
  (Equality), 11.4.7 (Logical), 11.4.11 (Conditional), 21.2 / Table 21-2 (Format specifiers).
- Archive items: `datatypes/real/{real_types,shortreal_types,realtime_types}`.
- Display sub-step `display.md` DI3 (`%f` / `%e` / `%g`) lands together with C1.
- IEEE 754 round-trip widths (17 / 9) keep emitted constants bit-identical across recompile;
  documented inline in `render_expr.cpp`.
