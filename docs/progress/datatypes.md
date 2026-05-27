# Datatypes

Tracks SystemVerilog data type coverage on the current pipeline. The integral family (`int`, `byte`,
`shortint`, `longint`, `time`, `integer`, `bit [N:0]`, `logic [N:0]`) lives in `integral.md` because
its work is a single PackedArray-runtime cut sequence. This file covers every other family:
`datatypes/enum`, `datatypes/string`, `datatypes/unpacked`, `datatypes/general` (dynamic array /
queue / associative), `datatypes/real`, `datatypes/default_init`, `datatypes/representation`.

Each archive item checkbox in `architecture-reset.md` is checked when its `*.yaml` cases reproduce
on the current pipeline.

## Actionable

| Item | Blocked on                                             |
| ---- | ------------------------------------------------------ |
| E3   | `datatypes/string` -- `name()` returns a string value. |

E1 and E2 are actionable now.

## Enum

Covers archive item `datatypes/enum` (sub-folders `enum`, `enum_implicit_conversion`,
`enum_methods`). Slang models enum as `EnumType : public IntegralType, public Scope`. Our HIR
mirrors that with a `hir::EnumType` carrying a `base_type` (a `PackedArrayType`) and an ordered
member table. HIR -> MIR aliases the enum's MIR `TypeId` to its base type's so MIR and the cpp
backend see only `lyra::value::PackedArray`. Enum methods are modelled as built-in class methods
under a uniform `CallExpr` shape: a new `hir::BuiltinMethodRef` variant of `SubroutineRef` carries
the method kind, and `CallExpr::arguments[0]` is the receiver (variadic for `next` / `prev`).

- [x] E1 -- Type plumbing, member references, compile-time methods. `LowerType` recognises
      `slang::ast::EnumType`; AST `NamedValueExpression` whose symbol kind is `EnumValue` lowers
      directly to `hir::IntegerLiteral` of the backing type. `v.first()` / `v.last()` / `v.num()`
      lower at AST -> HIR to
      `CallExpr{callee=BuiltinMethodRef{kEnumFirst|kEnumLast|kEnumNum},     arguments=[receiver]}`
      without folding; the fold happens at HIR -> MIR using the receiver's `hir::EnumType` member
      table. HIR -> MIR's `TranslateTypeData` has an `EnumType` arm that copies the base's
      translated `mir::TypeData`, so the orchestrator stays uniform. Conversions enum -> integral
      are transparent; integral -> enum requires explicit cast (LRM 6.19.3). Covers all 11 cases of
      `enum/default.yaml` (basic, sequential, explicit, mixed, comparison, arithmetic, explicit base
      type, display, range count, range bounds, range mixed);
      `enum_implicit_conversion/default.yaml` cases that are legal under LRM 6.19.3
      (`explicit_cast_works`, `enum_to_int_unchanged` via `enum_arithmetic`); and
      `enum_methods/default.yaml` cases for `first` / `last` / `num` / `num_auto_values`. Also ships
      `enum_passed_to_system_function` as a regression guard for the AST -> HIR Call dispatch (an
      enum-typed first argument to a `$`-prefixed system call must fall through to the system
      subroutine path, not the built-in method path). Unblocks C16 in `control-flow.md`. Methods
      `next` / `prev` / `name` return `diag::Unsupported` at HIR -> MIR until E2 / E3.
- [ ] E2 -- Runtime methods `next` / `prev` with optional step. AST -> HIR already produces
      `CallExpr{callee=BuiltinMethodRef{kEnumNext|kEnumPrev}, arguments=[receiver, step?]}` (no
      shape change). HIR -> MIR's `LowerBuiltinMethodCall` adds Next / Prev arms that lower each
      call to a value-to-value mapping over the enum's declaration-order member table, wrapping at
      the boundary. cpp emit emits one helper per enum type at the top of the translation unit; call
      sites invoke it. Covers `enum_methods/default.yaml` cases `enum_next`, `enum_next_wrap`,
      `enum_prev`, `enum_prev_wrap`, `enum_next_step`, `enum_next_step_wrap`, `enum_prev_step`,
      `enum_prev_step_wrap`.
- [ ] E3 -- Method `name()`. Returns the string name of the current value (empty string when the
      value is not a member). **Depends on** `datatypes/string` for the runtime string type. HIR ->
      MIR's `LowerBuiltinMethodCall` Name arm replaces the current `Unsupported` with a string
      lookup; cpp emit emits a per-enum-type `value -> string` helper. Covers
      `enum_methods/default.yaml` case `enum_name`.

### Cross-references

- LRM 6.19 (Enumerations), 6.19.3 (conversions), 6.19.5 (methods).
- Archive items: `datatypes/enum/{enum,enum_implicit_conversion,enum_methods}`.
- Unblocks: C16 (enum-typed `case` selectors) in `control-flow.md`.
- Slang reference: `slang/include/slang/ast/types/AllTypes.h` -- `EnumType`, `EnumValueSymbol`.
- Legacy archive reference: `archived/include/lyra/common/type.hpp` -- `EnumInfo`, `EnumMember`.
