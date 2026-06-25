#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"

#include <cstdint>
#include <utility>
#include <vector>

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

// Three trailing operands `PackedArray::FromInt` / `ConvertFrom` consume after
// the value: `(bit_width, is_signed, is_four_state)`. The runtime entry's
// signature takes host scalars (`uint64_t`, `bool`, `bool`), not SV-typed
// `PackedArray` values, so the lowering synthesizes `HostIntLiteral` for
// each -- they render as bare C++ integer literals that C++ implicitly
// narrows / converts to the parameter types.
void AppendPackedShapeArgs(
    const mir::CompilationUnit& unit, mir::Block& block,
    const mir::PackedArrayType& dst_pa, std::vector<mir::ExprId>& args) {
  const mir::TypeId int32_type = unit.builtins.int32;
  const auto host_lit = [&](std::int64_t v) {
    return block.exprs.Add(
        mir::Expr{.data = mir::HostIntLiteral{.value = v}, .type = int32_type});
  };
  args.push_back(host_lit(static_cast<std::int64_t>(dst_pa.BitWidth())));
  args.push_back(
      host_lit(dst_pa.signedness == mir::Signedness::kSigned ? 1 : 0));
  args.push_back(host_lit(dst_pa.IsFourState() ? 1 : 0));
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

// `PackedArray::FromInt(int_value, width, signed, four_state)` -- the static
// factory used by the real-to-integral and small-literal paths.
auto BuildPackedArrayFromInt(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId int_value,
    const mir::PackedArrayType& dst_pa, mir::TypeId dst_type) -> mir::Expr {
  std::vector<mir::ExprId> args{int_value};
  AppendPackedShapeArgs(unit, block, dst_pa, args);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFromInt,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = std::move(args)},
      .type = dst_type};
}

// `PackedArray::ConvertFrom(other, width, signed, four_state)` -- reshape a
// packed vector to the target shape.
auto BuildPackedArrayConvertFrom(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId src_id,
    const mir::PackedArrayType& dst_pa, mir::TypeId dst_type) -> mir::Expr {
  std::vector<mir::ExprId> args{src_id};
  AppendPackedShapeArgs(unit, block, dst_pa, args);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kConvertFrom,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = std::move(args)},
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
  const auto& src_ty = unit.GetType(src_type);
  const auto& dst_ty = unit.GetType(dst_type);
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
        unit, block, rounded_id, dst_ty.AsIntegralPacked(), dst_type);
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
    const bool same_shape = src_pa.BitWidth() == dst_pa.BitWidth() &&
                            src_pa.signedness == dst_pa.signedness &&
                            src_pa.atom == dst_pa.atom;
    const bool src_is_enum = src_ty.IsEnum();
    const bool dst_is_enum = dst_ty.IsEnum();
    mir::ExprId body_id = operand_id;
    if (!same_shape) {
      body_id = block.exprs.Add(BuildPackedArrayConvertFrom(
          unit, block, operand_id, dst_pa, dst_type));
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

  // Identity fallback: the lowering inserted a conversion the type system
  // already satisfies (e.g. string -> string lift).
  return operand_expr;
}

}  // namespace lyra::lowering::hir_to_mir
