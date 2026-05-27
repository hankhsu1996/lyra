# Datatypes

Tracks SystemVerilog data type coverage on the current pipeline. The integral family (`int`, `byte`,
`shortint`, `longint`, `time`, `integer`, `bit [N:0]`, `logic [N:0]`) lives in `integral.md` because
its work is a single PackedArray-runtime cut sequence. This file covers every other family:
`datatypes/enum`, `datatypes/string`, `datatypes/unpacked`, `datatypes/general` (dynamic array /
queue / associative), `datatypes/real`, `datatypes/default_init`, `datatypes/representation`.

Each archive item checkbox in `architecture-reset.md` is checked when its `*.yaml` cases reproduce
on the current pipeline.

## Actionable

E2 and E3 are actionable now; no remaining dependencies on other workstreams.

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

- [x] E1 -- Type plumbing, member references, compile-time methods. `LowerType` recognises
      `slang::ast::EnumType` and populates the enum type's six-entry method table (`first` / `last`
      / `num` / `next` / `prev` / `name`). AST `NamedValueExpression` whose symbol kind is
      `EnumValue` lowers directly to `hir::IntegerLiteral` of the backing type. `v.first()` /
      `v.last()` / `v.num()` lower at AST -> HIR via a uniform call dispatch that looks up the
      method by name on the receiver's type, producing
      `CallExpr{callee=MethodRef{receiver_type, method_id}, arguments=[receiver]}` without folding;
      the fold happens at HIR -> MIR's `LowerBuiltinMethodCall` using the receiver's `hir::EnumType`
      member table. HIR -> MIR's `TranslateTypeData` has an `EnumType` arm that copies the base's
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
      `control-flow.md`. Methods `next` / `prev` / `name` return `diag::Unsupported` at HIR -> MIR
      until E2 / E3.
- [ ] E2 -- Runtime methods `next` / `prev` with optional step (LRM 6.19.5.3 / 6.19.5.4). AST -> HIR
      already produces `CallExpr{callee=MethodRef{...}, arguments=[receiver, step?]}` (no shape
      change). HIR -> MIR's `LowerBuiltinMethodCall` replaces the current `Unsupported` arms for
      `kEnumNext` / `kEnumPrev` with a nested `ConditionalExpr` chain over the enum's
      declaration-order member table: each branch matches one member by equality and produces the
      wrapped `(i +/- step) mod count` target value as an `IntegerLiteral`; the default branch is
      the enum's default initial value (per LRM 6.19.5, Table 6-7, for non-member receivers). `step`
      is typed `int unsigned` by slang at AST level and must be a literal in this cut; non-constant
      step returns `diag::Unsupported`. Covers `enum_methods/default.yaml` cases `enum_next`,
      `enum_next_wrap`, `enum_prev`, `enum_prev_wrap`, `enum_next_step`, `enum_next_step_wrap`,
      `enum_prev_step`, `enum_prev_step_wrap`.
- [ ] E3 -- Method `name()` (LRM 6.19.5.6). Returns the string name of the current value, empty
      string when the value is not a member. HIR -> MIR's `LowerBuiltinMethodCall` Name arm replaces
      the current `Unsupported` with a nested `ConditionalExpr` chain mapping each
      `members[i].value` to `mir::StringLiteral{members[i].name}`, with default branch
      `mir::StringLiteral{""}`. Uses only existing `mir::ConditionalExpr`,
      `mir::BinaryExpr{kEquality}`, and `mir::StringLiteral` -- all already supported end-to-end
      through the cpp backend; no dependency on the `datatypes/string` workstream. Covers
      `enum_methods/default.yaml` case `enum_name`.

### Cross-references

- LRM 6.19 (Enumerations), 6.19.3 (conversions), 6.19.5 (methods).
- Archive items: `datatypes/enum/{enum,enum_implicit_conversion,enum_methods}`.
- Unblocks: C16 (enum-typed `case` selectors) in `control-flow.md`.
- Slang reference: `slang/include/slang/ast/types/AllTypes.h` -- `EnumType`, `EnumValueSymbol`.
- Legacy archive reference: `archived/include/lyra/common/type.hpp` -- `EnumInfo`, `EnumMember`.
