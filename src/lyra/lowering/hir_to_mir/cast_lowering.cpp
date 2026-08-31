#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"

#include <cstdint>
#include <variant>

#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/packed_type_descriptor.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto IsRealFamilyKind(mir::TypeKind k) -> bool {
  return k == mir::TypeKind::kReal || k == mir::TypeKind::kShortReal ||
         k == mir::TypeKind::kRealTime;
}

// The destination real type's own factory, named for which conversion this is:
// landing a machine integer (LRM 6.12.1) and reshaping across precisions are
// two operations, and the operand's type is not what tells them apart.
auto MakeRealFactoryCall(
    support::BuiltinFn entry, mir::ExprId operand_id, mir::TypeId dst_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = entry,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = {operand_id}},
      .type = dst_type};
}

auto MakeToInt64Call(const mir::CompilationUnit& unit, mir::ExprId operand_id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kToInt64},
              .arguments = {operand_id}},
      .type = unit.builtins.machine_int64};
}

auto MakeRoundCall(const mir::CompilationUnit& unit, mir::ExprId operand_id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kRound},
              .arguments = {operand_id}},
      .type = unit.builtins.machine_int64};
}

// `PackedArray::FromInt(int_value, shape)` -- the static factory used by the
// real-to-integral path: lands `int_value` into the destination's declared
// representation.
auto BuildPackedArrayFromInt(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId int_value,
    mir::TypeId dst_type) -> mir::Expr {
  const mir::ExprId packed_type =
      mir::BuildPackedTypeRef(unit, block, dst_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFromInt,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = {int_value, packed_type}},
      .type = dst_type};
}

// `PackedArray::ConvertFrom(src, shape)` -- reshape `src` into the
// destination's declared representation (width / signedness / state domain /
// dimension stack).
auto BuildPackedArrayConvertFrom(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId src_id,
    mir::TypeId dst_type) -> mir::Expr {
  const mir::ExprId packed_type =
      mir::BuildPackedTypeRef(unit, block, dst_type);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kConvertFrom,
                      .qualification = mir::TypeQualifier{.type = dst_type}},
              .arguments = {src_id, packed_type}},
      .type = dst_type};
}

// `String::FromPackedArray(bits)` / `String::FromByteArray(bytes)` static
// factories.
auto MakeStringFromFactory(
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

  // Real-family reshape (LRM 6.12.1): crossing precisions is the destination
  // type's own conversion, and staying at one is identity.
  if (IsRealFamilyKind(src_kind) && IsRealFamilyKind(dst_kind)) {
    if (src_kind == dst_kind) {
      return operand_expr;
    }
    return MakeRealFactoryCall(
        support::BuiltinFn::kConvertFrom, operand_id, dst_type);
  }

  // Integral -> real: read out the host int64, build the real from it.
  if (src_ty.IsIntegralPacked() && IsRealFamilyKind(dst_kind)) {
    const mir::ExprId int_id =
        block.exprs.Add(MakeToInt64Call(unit, operand_id));
    return MakeRealFactoryCall(support::BuiltinFn::kFromInt, int_id, dst_type);
  }

  // Real -> integral: round to int64, then `PackedArray::FromInt(...)` lands
  // the rounded value into the destination shape.
  if (IsRealFamilyKind(src_kind) && dst_ty.IsIntegralPacked()) {
    const mir::ExprId rounded_id =
        block.exprs.Add(MakeRoundCall(unit, operand_id));
    return BuildPackedArrayFromInt(unit, block, rounded_id, dst_type);
  }

  // Integral -> integral: a reshape into the destination's declared
  // representation. Crossing the enumeration boundary (LRM 6.19.3) changes the
  // type a value is held to and not the bits it carries, so it is a cast over
  // the reshaped value -- or over the operand itself, where the two
  // representations already agree and nothing reshapes.
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
          BuildPackedArrayConvertFrom(unit, block, operand_id, dst_type));
    }
    if (dst_is_enum || src_is_enum) {
      return mir::Expr{
          .data = mir::ValueCastExpr{.operand = body_id}, .type = dst_type};
    }
    if (same_shape) {
      return operand_expr;
    }
    return block.exprs.Get(body_id);
  }

  // Unpacked-array-of-byte -> string (LRM 21.3.4.3 $sscanf source lift).
  if (src_kind == mir::TypeKind::kUnpackedArray &&
      dst_kind == mir::TypeKind::kString) {
    return MakeStringFromFactory(
        unit, operand_id, support::BuiltinFn::kFromByteArray);
  }

  // Integral -> string (LRM 6.16 bit pattern -> string value).
  if (src_ty.IsIntegralPacked() && dst_kind == mir::TypeKind::kString) {
    return MakeStringFromFactory(
        unit, operand_id, support::BuiltinFn::kFromPackedArray);
  }

  // String -> integral (LRM 5.9): right-justified into the destination's
  // declared shape, which the shape operand names.
  if (src_kind == mir::TypeKind::kString && dst_ty.IsIntegralPacked()) {
    const mir::ExprId packed_type =
        mir::BuildPackedTypeRef(unit, block, dst_type);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromString,
                        .qualification = mir::TypeQualifier{.type = dst_type}},
                .arguments = {operand_id, packed_type}},
        .type = dst_type};
  }

  // String -> unpacked array of byte (LRM 5.9): left-justified from the array's
  // left bound. The element shape names the representation each element takes,
  // which is also what an element past the end of the text is left holding. LRM
  // 5.9 defines the conversion only for a byte element, so an array of anything
  // else is not a destination this reshapes into.
  if (const auto* dst_arr = std::get_if<mir::UnpackedArrayType>(&dst_ty.data);
      dst_arr != nullptr && src_kind == mir::TypeKind::kString &&
      unit.types.Get(dst_arr->element_type).IsIntegralPacked()) {
    const mir::ExprId element_type =
        mir::BuildPackedTypeRef(unit, block, dst_arr->element_type);
    const mir::ExprId count = BuildIntLiteral(
        unit, block, static_cast<std::int64_t>(dst_arr->dim.ElementCount()));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromString,
                        .qualification = mir::TypeQualifier{.type = dst_type}},
                .arguments = {operand_id, element_type, count}},
        .type = dst_type};
  }

  // Integral -> unpacked array of byte (LRM 5.9): a string literal is a packed
  // bit-vector constant, so an assignment of one to a byte array arrives here
  // rather than through the string path. Its bytes left-justify the same way,
  // and they arrive whole: a NUL among them is a byte like any other, where
  // routing through a string value would have removed it (LRM 6.16).
  if (const auto* dst_arr = std::get_if<mir::UnpackedArrayType>(&dst_ty.data);
      dst_arr != nullptr && src_ty.IsIntegralPacked() &&
      unit.types.Get(dst_arr->element_type).IsIntegralPacked()) {
    const mir::ExprId element_type =
        mir::BuildPackedTypeRef(unit, block, dst_arr->element_type);
    const mir::ExprId count = BuildIntLiteral(
        unit, block, static_cast<std::int64_t>(dst_arr->dim.ElementCount()));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kFromPackedArray,
                        .qualification = mir::TypeQualifier{.type = dst_type}},
                .arguments = {operand_id, element_type, count}},
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
    const mir::ExprId bound_id = BuildIntLiteral(unit, block, bound);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{.target = support::BuiltinFn::kConformBound},
                .arguments = {operand_id, bound_id}},
        .type = dst_type};
  }

  // LRM 8.14: an object of a subclass is also an object of its base class, so a
  // handle to one is a legal value of a variable declared with the base class.
  // The object is unchanged; only the handle's declared class differs, which is
  // what re-typing the reference states.
  if (std::holds_alternative<mir::ManagedRefType>(src_ty.data) &&
      std::holds_alternative<mir::ManagedRefType>(dst_ty.data)) {
    return mir::Expr{
        .data = mir::PointerCastExpr{.operand = operand_id}, .type = dst_type};
  }

  // Identity fallback: the lowering inserted a conversion the type system
  // already satisfies (e.g. string -> string lift).
  return operand_expr;
}

auto BuildPropagatedConversion(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::Expr {
  const auto& src_ty = unit.types.Get(block.exprs.Get(operand_id).type);
  const auto& dst_ty = unit.types.Get(dst_type);
  if (src_ty.IsIntegralPacked() && dst_ty.IsIntegralPacked()) {
    const mir::Signedness propagated = dst_ty.AsIntegralPacked().signedness;
    if (src_ty.AsIntegralPacked().signedness != propagated) {
      // Restating the operand's own representation under the propagated
      // signedness is what leaves the ordinary widening behind it: the fill
      // then follows the signedness the value carries, as everywhere else.
      mir::PackedArrayType restated = src_ty.AsIntegralPacked();
      restated.signedness = propagated;
      operand_id =
          ConvertToType(unit, block, operand_id, unit.types.Intern(restated));
    }
  }
  return BuildValueConversion(unit, block, operand_id, dst_type);
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
