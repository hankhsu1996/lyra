#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 7.10.5: the maximum index a queue's type declares, as the operand a
// runtime entry takes for it. A bound below zero is the unbounded queue, so one
// operand covers both and a queue declaring no bound is not a second form.
auto BuildQueueBoundOperand(
    const mir::CompilationUnit& unit, mir::Block& block,
    const mir::QueueType& queue) -> mir::ExprId {
  return BuildMachineIntLiteral(
      unit, block,
      queue.max_bound.has_value() ? static_cast<std::int64_t>(*queue.max_bound)
                                  : -1);
}

// An array of one kind built out of another kind's elements (LRM 7.6). The
// element shape, the element count a fixed-size array declares, and a queue's
// bound are properties of the destination variable, so each is read from
// `dst_type`, and which of them there is to hand over is what tells the three
// destinations' entries apart.
auto BuildArrayFromArrayCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId src_id,
    mir::TypeId dst_type) -> mir::Expr {
  const mir::ExprId element_default = block.exprs.Add(BuildDefaultValueExpr(
      unit, block, RequiredContainerElementType(unit, dst_type)));
  const auto call = [&](support::BuiltinFn entry,
                        std::vector<mir::ExprId> arguments) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = entry},
                .arguments = std::move(arguments)},
        .type = dst_type};
  };
  const mir::Type& destination = unit.types.Get(dst_type);
  if (const auto* fixed_size = destination.As<mir::UnpackedArrayType>()) {
    return call(
        support::BuiltinFn::kUnpackedArrayFromArray,
        {src_id, element_default,
         BuildMachineIntLiteral(
             unit, block, static_cast<std::int64_t>(fixed_size->Size()))});
  }
  if (const auto* queue = destination.As<mir::QueueType>()) {
    return call(
        support::BuiltinFn::kQueueFromArray,
        {src_id, element_default, BuildQueueBoundOperand(unit, block, *queue)});
  }
  return call(
      support::BuiltinFn::kDynamicArrayFromArray, {src_id, element_default});
}

// A conversion into a real type, named for which conversion this is: landing a
// machine integer (LRM 6.12.1) and reshaping across precisions are two
// operations, and the operand's type is not what tells them apart.
auto MakeRealConversionCall(
    support::BuiltinFn entry, mir::ExprId operand_id, mir::TypeId dst_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry, .type_argument = dst_type},
              .arguments = {operand_id}},
      .type = dst_type};
}

auto MakeRoundCall(const mir::CompilationUnit& unit, mir::ExprId operand_id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kRound,
                      .receiver = operand_id},
              .arguments = {}},
      .type = unit.builtins.machine_int64};
}

// A string built from an integral value's bytes or from a byte array.
auto MakeStringFromFactory(
    const mir::CompilationUnit& unit, mir::ExprId src_id, support::BuiltinFn id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = id}, .arguments = {src_id}},
      .type = unit.builtins.string};
}

}  // namespace

auto BuildToInt64Call(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id)
    -> mir::ExprId {
  return block.exprs.Add(MakeBuiltinCall(
      unit, block, support::BuiltinFn::kToInt64, operand_id, {},
      unit.builtins.machine_int64));
}

auto BuildValueConversion(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::Expr {
  const mir::Expr& operand_expr = block.exprs.Get(operand_id);
  const mir::TypeId src_type = operand_expr.type;
  if (src_type == dst_type) {
    return operand_expr;
  }
  // LRM 8.4: `null` is a value of every class-handle type, so what a null
  // literal denotes is decided by where it is used and by nothing it carries.
  // It is the literal that is polymorphic rather than any pair of types, which
  // is why this reads the operand rather than the conversion's endpoints.
  if (std::holds_alternative<mir::NullLiteral>(operand_expr.data)) {
    return mir::Expr{.data = mir::NullLiteral{}, .type = dst_type};
  }

  const auto& src_ty = unit.types.Get(src_type);
  const auto& dst_ty = unit.types.Get(dst_type);

  // Real-family reshape (LRM 6.12.1): crossing precisions is the destination
  // type's own conversion. Staying at one precision cannot reach here, since
  // two real-family types that are the same type are the same id.
  if (src_ty.IsRealFamily() && dst_ty.IsRealFamily()) {
    return MakeRealConversionCall(
        support::BuiltinFn::kConvertFrom, operand_id, dst_type);
  }

  // Integral -> real: read out the host int64, build the real from it.
  if (src_ty.IsIntegral() && dst_ty.IsRealFamily()) {
    const mir::ExprId int_id = BuildToInt64Call(unit, block, operand_id);
    return MakeRealConversionCall(
        support::BuiltinFn::kFromInt, int_id, dst_type);
  }

  // Real -> integral: round to int64, then land the rounded value in the
  // destination type.
  if (src_ty.IsRealFamily() && dst_ty.IsIntegral()) {
    const mir::ExprId rounded_id =
        block.exprs.Add(MakeRoundCall(unit, operand_id));
    return MakeBuiltinCall(
        unit, block, support::BuiltinFn::kIntegralFromInt, std::nullopt,
        {rounded_id}, dst_type);
  }

  // Integral -> integral: a reshape into the destination's declared
  // representation. An enumeration (LRM 6.19.3) shares that representation
  // with its base while being a type of its own, so crossing into or out of
  // one changes the type a value is held to and not the bits it carries, which
  // is what the cast below says and all it says.
  if (src_ty.IsIntegral() && dst_ty.IsIntegral()) {
    // A value's representation is all its integral type says -- width,
    // signedness and state domain -- so two that agree on all three hold the
    // same bits. A reshape lands the bits at the destination type outright.
    // What is left is the same representation under another type, which is
    // the cast: the bits already fit, and only what the program holds the
    // value to be changes.
    if (src_ty.Integral() != dst_ty.Integral()) {
      return MakeBuiltinCall(
          unit, block, support::BuiltinFn::kIntegralConvert, std::nullopt,
          {operand_id}, dst_type);
    }
    return MakeSameRepresentationCast(unit, block, operand_id, dst_type);
  }

  // Unpacked-array-of-byte -> string (LRM 21.3.4.3 $sscanf source lift).
  if (src_ty.Is<mir::UnpackedArrayType>() && dst_ty.Is<mir::StringType>()) {
    return MakeStringFromFactory(
        unit, operand_id, support::BuiltinFn::kFromByteArray);
  }

  // Integral -> string (LRM 6.16 bit pattern -> string value). A constant's
  // bytes are fixed before the program runs, so its string is stated as the
  // string it is.
  if (src_ty.IsIntegral() && dst_ty.Is<mir::StringType>()) {
    if (std::optional<std::string> text =
            mir::FoldStringFromBits(unit, operand_expr)) {
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {block.exprs.Add(
                      mir::MakeStringLiteral(dst_type, *std::move(text)))}},
          .type = dst_type};
    }
    return MakeStringFromFactory(
        unit, operand_id, support::BuiltinFn::kStringFromBits);
  }

  // String -> integral (LRM 5.9): right-justified into the destination type,
  // which the call answers with.
  if (src_ty.Is<mir::StringType>() && dst_ty.IsIntegral()) {
    return MakeBuiltinCall(
        unit, block, support::BuiltinFn::kIntegralFromString, std::nullopt,
        {operand_id}, dst_type);
  }

  // String -> unpacked array of byte (LRM 5.9): left-justified from the array's
  // left bound. The array's element type is the representation each element
  // takes, which is also what an element past the end of the text is left
  // holding. LRM 5.9 defines the conversion only for a byte element, so an
  // array of anything else is not a destination this reshapes into.
  if (const auto* dst_arr = dst_ty.As<mir::UnpackedArrayType>();
      dst_arr != nullptr && src_ty.Is<mir::StringType>() &&
      unit.types.Get(dst_arr->element_type).IsIntegral()) {
    const mir::ExprId count = BuildMachineIntLiteral(
        unit, block, static_cast<std::int64_t>(dst_arr->dim.ElementCount()));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kByteArrayFromString,
                        .type_argument = dst_type},
                .arguments = {operand_id, count}},
        .type = dst_type};
  }

  // Integral -> unpacked array of byte (LRM 5.9): a string literal is a packed
  // bit-vector constant, so an assignment of one to a byte array arrives here
  // rather than through the string path. Its bytes left-justify the same way,
  // and they arrive whole: a NUL among them is a byte like any other, where
  // routing through a string value would have removed it (LRM 6.16).
  if (const auto* dst_arr = dst_ty.As<mir::UnpackedArrayType>();
      dst_arr != nullptr && src_ty.IsIntegral() &&
      unit.types.Get(dst_arr->element_type).IsIntegral()) {
    const mir::ExprId count = BuildMachineIntLiteral(
        unit, block, static_cast<std::int64_t>(dst_arr->dim.ElementCount()));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kByteArrayFromBits,
                        .type_argument = dst_type},
                .arguments = {operand_id, count}},
        .type = dst_type};
  }

  // Between two of the three array container kinds (LRM 7.6): the kinds hold
  // their elements differently, so the destination is built out of the source's
  // elements rather than taking its value whole. The clause admits the
  // assignment only where the element types are equivalent, so the elements
  // themselves cross as they stand.
  if (CrossesArrayContainerKinds(src_ty, dst_ty)) {
    return BuildArrayFromArrayCall(unit, block, operand_id, dst_type);
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
  if (const auto* dst_q = dst_ty.As<mir::QueueType>();
      dst_q != nullptr && src_ty.Is<mir::QueueType>()) {
    const mir::ExprId bound_id = BuildQueueBoundOperand(unit, block, *dst_q);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kConformBound,
                        .receiver = operand_id},
                .arguments = {bound_id}},
        .type = dst_type};
  }

  // LRM 8.14: an object of a subclass is also an object of its base class, so a
  // handle to one is a legal value of a variable declared with the base class.
  // The object is unchanged; only the handle's declared class differs, which is
  // what re-typing the reference states.
  if (src_ty.Is<mir::ManagedRefType>() && dst_ty.Is<mir::ManagedRefType>()) {
    return mir::Expr{
        .data = mir::CastExpr{.operand = operand_id}, .type = dst_type};
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
  if (src_ty.IsIntegral() && dst_ty.IsIntegral()) {
    const mir::Signedness propagated = dst_ty.Integral().signedness;
    if (src_ty.Integral().signedness != propagated) {
      // Restating the operand's own representation under the propagated
      // signedness is what leaves the ordinary widening behind it: the fill
      // then follows the signedness the value carries, as everywhere else.
      mir::IntegralType restated = src_ty.Integral();
      restated.signedness = propagated;
      operand_id = ConvertToType(
          unit, block, operand_id, unit.types.Intern(mir::Type{restated}));
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

auto ConvertToPropagatedType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::ExprId {
  if (block.exprs.Get(operand_id).type == dst_type) {
    return operand_id;
  }
  return block.exprs.Add(
      BuildPropagatedConversion(unit, block, operand_id, dst_type));
}

auto OperandAtHandleType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId compared_with) -> mir::ExprId {
  if (!unit.types.Get(compared_with).Is<mir::ManagedRefType>()) {
    return operand_id;
  }
  return ConvertToType(unit, block, operand_id, compared_with);
}

}  // namespace lyra::lowering::hir_to_mir
