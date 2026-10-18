#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant_folding.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// An array of one kind built out of another kind's elements (LRM 7.6), or a
// queue brought to the bound its own declaration states (LRM 7.10.5): the
// source is the one operand, and everything else the destination's type says.
auto BuildArrayFromArrayCall(
    support::BuiltinFn entry, mir::ExprId src_id, mir::TypeId dst_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry}, .arguments = {src_id}},
      .type = dst_type};
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

// Whether `array` is what a string literal may be assigned to (LRM 5.9): an
// array whose elements are `byte`, eight bits, signed and two-state (LRM 6.11).
auto IsArrayOfBytes(
    const mir::CompilationUnit& unit, const mir::UnpackedArrayType& array)
    -> bool {
  const mir::Type& element = unit.types.Get(array.element_type);
  return element.IsIntegral() &&
         element.Integral() ==
             mir::IntegralType{
                 .bit_width = 8,
                 .signedness = mir::Signedness::kSigned,
                 .state_kind = mir::IntegralStateKind::kTwoState};
}

// The language defines no conversion between the two types, so the front end
// refuses a program asking for one, and whatever asked here stated a
// conversion no program wrote.
[[noreturn]] void RefuseConversion(std::string_view into) {
  throw InternalError(
      std::format(
          "BuildValueConversion: the operand's type has no conversion into {}",
          into));
}

auto ToReal(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  const mir::Type& source = unit.types.Get(block.exprs.Get(operand).type);
  // LRM 6.12.1: crossing precisions is the destination type's own conversion.
  if (source.IsRealFamily()) {
    return MakeRealConversionCall(
        support::BuiltinFn::kConvertFrom, operand, destination);
  }
  if (source.IsIntegral()) {
    return MakeRealConversionCall(
        support::BuiltinFn::kFromInt, BuildToInt64Call(unit, block, operand),
        destination);
  }
  RefuseConversion("a real");
}

auto ToIntegral(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  const mir::Type& source = unit.types.Get(block.exprs.Get(operand).type);
  if (source.IsRealFamily()) {
    const mir::ExprId rounded = block.exprs.Add(MakeRoundCall(unit, operand));
    return MakeBuiltinCall(
        unit, block, support::BuiltinFn::kIntegralFromInt, std::nullopt,
        {rounded}, destination);
  }
  // An enumeration (LRM 6.19.3) shares its representation with its base while
  // being a type of its own, and a representation is all an integral type says
  // -- width, signedness and state domain. Two types that differ in it are a
  // reshape of the bits; two that agree hold the same bits, and only the type
  // the program holds the value to be changes.
  if (source.IsIntegral()) {
    if (source.Integral() != unit.types.Get(destination).Integral()) {
      return MakeBuiltinCall(
          unit, block, support::BuiltinFn::kIntegralConvert, std::nullopt,
          {operand}, destination);
    }
    return MakeSameRepresentationCast(unit, block, operand, destination);
  }
  // LRM 5.9: right-justified into the destination type.
  if (source.Is<mir::StringType>()) {
    return MakeBuiltinCall(
        unit, block, support::BuiltinFn::kIntegralFromString, std::nullopt,
        {operand}, destination);
  }
  RefuseConversion("an integral type");
}

auto ToString(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  const mir::Type& source = unit.types.Get(block.exprs.Get(operand).type);
  // LRM 21.3.4.3: an unpacked array of bytes read as a string.
  if (source.Is<mir::UnpackedArrayType>()) {
    return MakeStringFromFactory(
        unit, operand, support::BuiltinFn::kFromByteArray);
  }
  // LRM 6.16: a bit pattern read as a string. A constant's bytes are fixed
  // before the program runs, so its string is stated as the string it is.
  if (source.IsIntegral()) {
    if (std::optional<std::string> text =
            mir::FoldStringFromBits(unit, block.exprs.Get(operand))) {
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{},
                  .arguments = {block.exprs.Add(
                      mir::MakeStringLiteral(destination, *std::move(text)))}},
          .type = destination};
    }
    return MakeStringFromFactory(
        unit, operand, support::BuiltinFn::kStringFromBits);
  }
  RefuseConversion("a string");
}

// LRM 5.9: text left-justified from an array of bytes' left bound, an element
// past its end left at the element type's default. `entry` is the one taking
// the form the text arrives in.
auto MakeBytesFromTextCall(
    mir::ExprId operand, mir::TypeId destination, support::BuiltinFn entry)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = entry}, .arguments = {operand}},
      .type = destination};
}

auto ToFixedSizeArray(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  const mir::Type& source = unit.types.Get(block.exprs.Get(operand).type);
  const auto& array = unit.types.Get(destination).Get<mir::UnpackedArrayType>();
  // LRM 7.6: two arrays of one kind are assigned where their element types
  // are equivalent, and a fixed-size one where the counts agree too. What can
  // then differ between the two types is how a range is declared, which is
  // read where an element is selected and never held by the value.
  if (source.Is<mir::UnpackedArrayType>()) {
    return block.exprs.Get(operand);
  }
  if (source.Is<mir::DynamicArrayType>() || source.Is<mir::QueueType>()) {
    return BuildArrayFromArrayCall(
        support::BuiltinFn::kUnpackedArrayFromArray, operand, destination);
  }
  if (IsArrayOfBytes(unit, array)) {
    if (source.Is<mir::StringType>()) {
      return MakeBytesFromTextCall(
          operand, destination, support::BuiltinFn::kByteArrayFromString);
    }
    // A string literal is a packed constant, and its bytes arrive whole: a
    // NUL among them is a byte like any other, where a string value would
    // have dropped it (LRM 6.16).
    if (source.IsIntegral()) {
      return MakeBytesFromTextCall(
          operand, destination, support::BuiltinFn::kByteArrayFromBits);
    }
  }
  RefuseConversion("a fixed-size unpacked array");
}

auto ToDynamicArray(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  const mir::Type& source = unit.types.Get(block.exprs.Get(operand).type);
  if (source.Is<mir::DynamicArrayType>()) {
    return block.exprs.Get(operand);
  }
  if (source.Is<mir::UnpackedArrayType>() || source.Is<mir::QueueType>()) {
    return BuildArrayFromArrayCall(
        support::BuiltinFn::kDynamicArrayFromArray, operand, destination);
  }
  RefuseConversion("a dynamic array");
}

auto ToQueue(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  const mir::Type& source = unit.types.Get(block.exprs.Get(operand).type);
  if (source.Is<mir::UnpackedArrayType>() ||
      source.Is<mir::DynamicArrayType>()) {
    return BuildArrayFromArrayCall(
        support::BuiltinFn::kQueueFromArray, operand, destination);
  }
  // LRM 7.10.5: the bound is a declared property of the destination variable,
  // so a queue assigned another's contents keeps only what its own bound
  // admits.
  if (source.Is<mir::QueueType>()) {
    return BuildArrayFromArrayCall(
        support::BuiltinFn::kConformBound, operand, destination);
  }
  RefuseConversion("a queue");
}

// LRM 7.9.9: an associative array is assigned only another of a compatible
// type and the same index type, which leaves nothing of the value to change.
auto ToAssociativeArray(
    const mir::CompilationUnit& unit, const mir::Expr& operand) -> mir::Expr {
  if (unit.types.Get(operand.type).Is<mir::AssociativeArrayType>()) {
    return operand;
  }
  RefuseConversion("an associative array");
}

// LRM 8.14: an object of a subclass is also an object of its base class, so a
// handle to one is a value of a variable declared with the base class. The
// object is unchanged and only the handle's declared class differs.
auto ToClassHandle(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand,
    mir::TypeId destination) -> mir::Expr {
  if (unit.types.Get(block.exprs.Get(operand).type).Is<mir::ManagedRefType>()) {
    return mir::Expr{
        .data = mir::CastExpr{.operand = operand}, .type = destination};
  }
  RefuseConversion("a class handle");
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
  if (operand_expr.type == dst_type) {
    return operand_expr;
  }
  // LRM 8.4: `null` is a value of every class-handle type, so what a null
  // literal denotes is decided by where it is used and by nothing it carries.
  // It is the literal that is polymorphic rather than any pair of types, which
  // is why this reads the operand rather than the conversion's endpoints.
  if (std::holds_alternative<mir::NullLiteral>(operand_expr.data)) {
    return mir::Expr{.data = mir::NullLiteral{}, .type = dst_type};
  }

  // A type no conversion answers a value of: the source language converts
  // nothing into it, or no declaration of the source language names it.
  const auto none = [](const auto&) -> mir::Expr {
    RefuseConversion("the destination's type");
  };
  return unit.types.Get(dst_type).Visit(
      Overloaded{
          [&](const mir::RealType&) {
            return ToReal(unit, block, operand_id, dst_type);
          },
          [&](const mir::ShortRealType&) {
            return ToReal(unit, block, operand_id, dst_type);
          },
          [&](const mir::IntegralType&) {
            return ToIntegral(unit, block, operand_id, dst_type);
          },
          [&](const mir::EnumType&) {
            return ToIntegral(unit, block, operand_id, dst_type);
          },
          [&](const mir::StringType&) {
            return ToString(unit, block, operand_id, dst_type);
          },
          [&](const mir::UnpackedArrayType&) {
            return ToFixedSizeArray(unit, block, operand_id, dst_type);
          },
          [&](const mir::DynamicArrayType&) {
            return ToDynamicArray(unit, block, operand_id, dst_type);
          },
          [&](const mir::QueueType&) {
            return ToQueue(unit, block, operand_id, dst_type);
          },
          [&](const mir::ManagedRefType&) {
            return ToClassHandle(unit, block, operand_id, dst_type);
          },
          [&](const mir::AssociativeArrayType&) {
            return ToAssociativeArray(unit, operand_expr);
          },
          [&](const mir::WildcardIndexType& t) { return none(t); },
          [&](const mir::MachineCStringType& t) { return none(t); },
          [&](const mir::MachineBoolType& t) { return none(t); },
          [&](const mir::MachineIntType& t) { return none(t); },
          [&](const mir::MachineFloatType& t) { return none(t); },
          [&](const mir::MachineArrayType& t) { return none(t); },
          [&](const mir::MachineFunctionType& t) { return none(t); },
          [&](const mir::EventType& t) { return none(t); },
          [&](const mir::ChandleType& t) { return none(t); },
          [&](const mir::VoidType& t) { return none(t); },
          [&](const mir::ObjectType& t) { return none(t); },
          [&](const mir::RuntimeClassType& t) { return none(t); },
          [&](const mir::RuntimeEffectsType& t) { return none(t); },
          [&](const mir::FilesType& t) { return none(t); },
          [&](const mir::DiagnosticType& t) { return none(t); },
          [&](const mir::RuntimeLibraryType& t) { return none(t); },
          [&](const mir::CoroutineType& t) { return none(t); },
          [&](const mir::VariableRefType& t) { return none(t); },
          [&](const mir::ReferenceWrapperType& t) { return none(t); },
          [&](const mir::PointerType& t) { return none(t); },
          [&](const mir::ReferenceType& t) { return none(t); },
          [&](const mir::VectorType& t) { return none(t); },
          [&](const mir::TupleType& t) { return none(t); },
          [&](const mir::EmptyType& t) { return none(t); },
          [&](const mir::ObservableType& t) { return none(t); },
          [&](const mir::ResolvedType& t) { return none(t); },
          [&](const mir::DriverType& t) { return none(t); },
          [&](const mir::OpenWriteType& t) { return none(t); },
          [&](const mir::DesignationType& t) { return none(t); },
          [&](const mir::BitsReferenceType& t) { return none(t); },
          [&](const mir::ObjectWriteType& t) { return none(t); },
          [&](const mir::SampledHistoryType& t) { return none(t); },
          [&](const mir::EvaluationAttemptsType& t) { return none(t); },
          [&](const mir::LoopBlocksType& t) { return none(t); },
          [&](const mir::RecordType& t) { return none(t); },
          [&](const mir::ClosureType& t) { return none(t); },
          [&](const mir::ErasedCallableType& t) { return none(t); },
      });
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
  // A conversion acts on a value, so a place it is handed is read first.
  operand_id = block.AsValue(unit.types, operand_id);
  if (block.exprs.Get(operand_id).type == dst_type) {
    return operand_id;
  }
  return block.exprs.Add(
      BuildValueConversion(unit, block, operand_id, dst_type));
}

auto AtType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId type) -> mir::ExprId {
  if (block.exprs.Get(operand_id).type == type) {
    return operand_id;
  }
  return ConvertToType(unit, block, operand_id, type);
}

auto ConvertToPropagatedType(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId operand_id,
    mir::TypeId dst_type) -> mir::ExprId {
  operand_id = block.AsValue(unit.types, operand_id);
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
