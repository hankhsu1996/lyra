#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto IsRealFamilyKind(mir::TypeKind k) -> bool {
  return k == mir::TypeKind::kReal || k == mir::TypeKind::kShortReal ||
         k == mir::TypeKind::kRealTime;
}

// `Real(operand)` / `ShortReal(operand)` invokes the explicit `RealValue`
// converting constructor. Used both for cross-precision real reshape and as
// the outer step of the integral-to-real bridge (Real(packed.ToInt64())).
auto BuildRealConstructorCall(mir::ExprId operand_id, mir::TypeId dst_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{.callee = mir::Construct{}, .arguments = {operand_id}},
      .type = dst_type};
}

auto BuildToInt64Call(const mir::CompilationUnit& unit, mir::ExprId operand_id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kToInt64},
              .arguments = {operand_id}},
      .type = unit.builtins.int32};
}

auto BuildRoundCall(const mir::CompilationUnit& unit, mir::ExprId operand_id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kRound},
              .arguments = {operand_id}},
      .type = unit.builtins.int32};
}

// The destination representation a packed factory call lands its result into,
// carried as an ordinary MIR value of the destination type -- a default literal
// -- so the representation reaches the runtime through the argument list, not
// composed by the backend from type payload. Its contents are overwritten by
// the factory; only its type's declared shape matters.
auto BuildPackedShapePrototype(
    mir::Block& block, const mir::PackedArrayType& dst_pa, mir::TypeId dst_type)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::IntegerLiteral{.value = DefaultIntegralConstant(dst_pa)},
          .type = dst_type});
}

// `PackedArray::FromInt(int_value, prototype)` -- the static factory used by
// the real-to-integral path: lands `int_value` into the prototype's declared
// representation.
auto BuildPackedArrayFromInt(
    mir::Block& block, mir::ExprId int_value,
    const mir::PackedArrayType& dst_pa, mir::TypeId dst_type) -> mir::Expr {
  const mir::ExprId prototype =
      BuildPackedShapePrototype(block, dst_pa, dst_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFromInt,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = {int_value, prototype}},
      .type = dst_type};
}

// `PackedArray::ConvertFrom(src, prototype)` -- reshape `src` into the
// prototype's declared representation (width / signedness / state domain /
// dimension stack).
auto BuildPackedArrayConvertFrom(
    mir::Block& block, mir::ExprId src_id, const mir::PackedArrayType& dst_pa,
    mir::TypeId dst_type) -> mir::Expr {
  const mir::ExprId prototype =
      BuildPackedShapePrototype(block, dst_pa, dst_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kConvertFrom,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = {src_id, prototype}},
      .type = dst_type};
}

// `String::FromPackedArray(bits)` / `String::FromByteArray(bytes)` static
// factories.
auto BuildStringFromFactory(
    const mir::CompilationUnit& unit, mir::ExprId src_id, support::BuiltinFn id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = id,
                      .qualification =
                          mir::TypeQualifier{.type = unit.builtins.string}},
              .arguments = {src_id}},
      .type = unit.builtins.string};
}

}  // namespace

auto BuildValueConversion(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::Expr {
  const mir::Expr& operand_expr = block.exprs.Get(operand_id);
  const mir::TypeId src_type = operand_expr.type;
  if (src_type == dst_type) {
    return operand_expr;
  }
  const auto& src_ty = unit.types.Get(src_type);
  const auto& dst_ty = unit.types.Get(dst_type);
  const auto src_kind = src_ty.Kind();
  const auto dst_kind = dst_ty.Kind();

  // Real-family reshape (e.g. `shortreal` <-> `real`): the `RealValue<Other>`
  // explicit converting ctor handles it; same-precision is identity.
  if (IsRealFamilyKind(src_kind) && IsRealFamilyKind(dst_kind)) {
    if (src_kind == dst_kind) {
      return operand_expr;
    }
    return BuildRealConstructorCall(operand_id, dst_type);
  }

  // Integral -> real: read out the host int64, build the real from it.
  if (src_ty.IsIntegralPacked() && IsRealFamilyKind(dst_kind)) {
    const mir::ExprId int_id =
        block.exprs.Add(BuildToInt64Call(unit, operand_id));
    return BuildRealConstructorCall(int_id, dst_type);
  }

  // Real -> integral: round to int64, then `PackedArray::FromInt(...)` lands
  // the rounded value into the destination shape.
  if (IsRealFamilyKind(src_kind) && dst_ty.IsIntegralPacked()) {
    const mir::ExprId rounded_id =
        block.exprs.Add(BuildRoundCall(unit, operand_id));
    return BuildPackedArrayFromInt(
        block, rounded_id, dst_ty.AsIntegralPacked(), dst_type);
  }

  // Integral -> integral: width / signedness / state reshape, with an enum
  // wrap at either end if the LRM 6.19.3 type axis crosses the enum class
  // boundary. The shape-change inner step is `PackedArray::ConvertFrom`; the
  // enum-wrap outer step is a Construct that calls the destination
  // class's converting ctor (enum class for dst-enum, plain PackedArray for
  // src-enum slicing).
  if (src_ty.IsIntegralPacked() && dst_ty.IsIntegralPacked()) {
    const auto& src_pa = src_ty.AsIntegralPacked();
    const auto& dst_pa = dst_ty.AsIntegralPacked();
    // Representation equality across every axis the value carries -- width,
    // signedness, state domain, and the dimension stack. A same-width
    // dims-only difference (a flat vector reaching a packed-of-packed
    // destination) is a real reshape the front end draws no conversion for, so
    // it must reshape here.
    const bool same_shape = src_pa.signedness == dst_pa.signedness &&
                            src_pa.atom == dst_pa.atom &&
                            src_pa.dims == dst_pa.dims;
    const bool src_is_enum = src_ty.IsEnum();
    const bool dst_is_enum = dst_ty.IsEnum();
    mir::ExprId body_id = operand_id;
    if (!same_shape) {
      body_id = block.exprs.Add(
          BuildPackedArrayConvertFrom(block, operand_id, dst_pa, dst_type));
    }
    if (dst_is_enum || src_is_enum) {
      return mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {body_id}},
          .type = dst_type};
    }
    if (same_shape) {
      return operand_expr;
    }
    return block.exprs.Get(body_id);
  }

  // Unpacked-array-of-byte -> string (LRM 21.3.4.3 $sscanf source lift).
  if (src_kind == mir::TypeKind::kUnpackedArray &&
      dst_kind == mir::TypeKind::kString) {
    return BuildStringFromFactory(
        unit, operand_id, support::BuiltinFn::kFromByteArray);
  }

  // Integral -> string (LRM 6.16 bit pattern -> string value).
  if (src_ty.IsIntegralPacked() && dst_kind == mir::TypeKind::kString) {
    return BuildStringFromFactory(
        unit, operand_id, support::BuiltinFn::kFromPackedArray);
  }

  // String -> integral (LRM 5.9): right-justified into the destination's shape,
  // which the prototype carries.
  if (src_kind == mir::TypeKind::kString && dst_ty.IsIntegralPacked()) {
    const mir::ExprId prototype =
        BuildPackedShapePrototype(block, dst_ty.AsIntegralPacked(), dst_type);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromString,
                        .qualification = mir::TypeQualifier{.type = dst_type}},
                .arguments = {operand_id, prototype}},
        .type = dst_type};
  }

  // String -> unpacked array of byte (LRM 5.9): left-justified from the array's
  // left bound. The element prototype carries the element representation and
  // doubles as the default an element past the text keeps. LRM 5.9 defines the
  // conversion only for a byte element, so an array of anything else is not a
  // destination this reshapes into.
  if (const auto* dst_arr = std::get_if<mir::UnpackedArrayType>(&dst_ty.data);
      dst_arr != nullptr && src_kind == mir::TypeKind::kString &&
      unit.types.Get(dst_arr->element_type).IsIntegralPacked()) {
    const auto& elem_ty = unit.types.Get(dst_arr->element_type);
    const mir::ExprId element_prototype = BuildPackedShapePrototype(
        block, elem_ty.AsIntegralPacked(), dst_arr->element_type);
    const mir::ExprId count = block.exprs.Add(
        mir::MakeInt32Literal(
            unit.builtins.int32,
            static_cast<std::int64_t>(dst_arr->dim.ElementCount())));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromString,
                        .qualification = mir::TypeQualifier{.type = dst_type}},
                .arguments = {operand_id, element_prototype, count}},
        .type = dst_type};
  }

  // Unpacked -> unpacked: assignment requires equivalent element types and the
  // same element count (LRM 7.6), so the element representation already matches
  // and a whole-array store is a plain ordinal-payload copy. The declared range
  // is a fact of the destination's static type consulted only at selection, not
  // payload that a store must conform -- so this falls through to the identity
  // path, no conversion node.

  // Queue -> queue: assignment requires equivalent element types (LRM 7.10), so
  // the element representation already matches and only the LRM 7.10.5 bound
  // can differ. Conform the source's contents to the destination's bound, which
  // a pure whole-value adopt would otherwise drop -- the bound is a declared
  // property of the destination variable.
  if (const auto* dst_q = std::get_if<mir::QueueType>(&dst_ty.data);
      dst_q != nullptr && std::holds_alternative<mir::QueueType>(src_ty.data)) {
    const std::int64_t bound =
        dst_q->max_bound.has_value()
            ? static_cast<std::int64_t>(*dst_q->max_bound)
            : -1;
    const mir::ExprId bound_id =
        block.exprs.Add(mir::MakeInt32Literal(unit.builtins.int32, bound));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{.target = support::BuiltinFn::kConformBound},
                .arguments = {operand_id, bound_id}},
        .type = dst_type};
  }

  // Identity fallback: the lowering inserted a conversion the type system
  // already satisfies (e.g. string -> string lift).
  return operand_expr;
}

auto ConvertToType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::ExprId {
  if (block.exprs.Get(operand_id).type == dst_type) {
    return operand_id;
  }
  return block.exprs.Add(
      BuildValueConversion(unit, block, operand_id, dst_type));
}

}  // namespace lyra::lowering::hir_to_mir
