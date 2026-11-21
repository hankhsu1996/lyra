#include "lyra/mir/expr.hpp"

#include <cstdint>
#include <optional>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_interface.hpp"

namespace lyra::mir {

auto MakeAssignExpr(
    const BuiltinMirTypes& builtins, ExprId target, ExprId value) -> Expr {
  return Expr{
      .data = AssignExpr{.target = target, .value = value},
      .type = builtins.void_type};
}

auto IsPlace(const ExprData& data) -> bool {
  return std::visit(
      Overloaded{
          [](const ReferenceExpr& e) {
            return std::visit(
                Overloaded{
                    [](const LocalRef&) { return true; },
                    [](const StaticPropertyRef&) { return true; },
                    [](const StaticVariableRef&) { return true; },
                    [](const DefinitionRef&) { return false; },
                    [](const ConstantRef&) { return false; },
                    [](const FunctionRef&) { return false; },
                    [](const DispatchEntryRef&) { return false; },
                    [](const DestructorRef&) { return false; },
                    [](const DispatchTableRef&) { return false; },
                    [](const InterfaceTableRef&) { return false; },
                    [](const ExternalObjectEntryRef&) { return false; }},
                e.target);
          },
          [](const DerefExpr&) { return true; },
          [](const FieldAccessExpr&) { return true; },
          [](const SliceExpr&) { return true; },
          [](const IndexExpr&) { return true; },
          [](const StringLiteral&) { return false; },
          [](const NullLiteral&) { return false; },
          [](const MachineBoolLiteral&) { return false; },
          [](const MachineIntLiteral&) { return false; },
          [](const FloatingLiteral&) { return false; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const CastExpr&) { return false; },
          [](const ConditionalExpr&) { return false; },
          [](const BlockExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const CallExpr&) { return false; },
          [](const NewExpr&) { return false; },
          [](const AddressOfExpr&) { return false; },
          [](const BorrowExpr&) { return false; },
          [](const MoveExpr&) { return false; },
          [](const CopyExpr&) { return false; },
          [](const DiscriminantExpr&) { return false; },
          [](const ClosureExpr&) { return false; },
          [](const CompositeExpr&) { return false; },
          [](const TaggedUnionExpr&) { return false; },
          [](const AwaitExpr&) { return false; },
          [](const WaitExpr&) { return false; },
          [](const VectorGetExpr&) { return false; }},
      data);
}

auto IsConstant(const ExprData& data) -> bool {
  return std::visit(
      Overloaded{
          [](const ReferenceExpr& e) {
            return std::visit(
                Overloaded{
                    [](const LocalRef&) { return false; },
                    [](const StaticPropertyRef&) { return false; },
                    [](const StaticVariableRef&) { return false; },
                    [](const DefinitionRef&) { return true; },
                    [](const ConstantRef&) { return true; },
                    [](const FunctionRef&) { return true; },
                    [](const DispatchEntryRef&) { return true; },
                    [](const DestructorRef&) { return true; },
                    [](const DispatchTableRef&) { return true; },
                    [](const InterfaceTableRef&) { return true; },
                    [](const ExternalObjectEntryRef&) { return true; }},
                e.target);
          },
          [](const StringLiteral&) { return true; },
          [](const NullLiteral&) { return true; },
          [](const MachineBoolLiteral&) { return true; },
          [](const MachineIntLiteral&) { return true; },
          [](const FloatingLiteral&) { return true; },
          [](const DerefExpr&) { return false; },
          [](const FieldAccessExpr&) { return false; },
          [](const SliceExpr&) { return false; },
          [](const IndexExpr&) { return false; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const CastExpr&) { return false; },
          [](const ConditionalExpr&) { return false; },
          [](const BlockExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const CallExpr&) { return false; },
          [](const NewExpr&) { return false; },
          [](const AddressOfExpr&) { return false; },
          [](const BorrowExpr&) { return false; },
          [](const MoveExpr&) { return false; },
          [](const CopyExpr&) { return false; },
          [](const DiscriminantExpr&) { return false; },
          [](const ClosureExpr&) { return false; },
          [](const CompositeExpr&) { return false; },
          [](const TaggedUnionExpr&) { return false; },
          [](const AwaitExpr&) { return false; },
          [](const WaitExpr&) { return false; },
          [](const VectorGetExpr&) { return false; }},
      data);
}

auto CalleeReceiver(const Callee& callee) -> std::optional<ExprId> {
  return std::visit(
      Overloaded{
          [](const Direct& d) { return d.receiver; },
          [](const Virtual& v) { return std::optional{v.receiver}; },
          [](const Indirect&) { return std::optional<ExprId>{}; },
          [](const Construct&) { return std::optional<ExprId>{}; }},
      callee);
}

auto DesignatedRunWrite(
    const CompilationUnit& unit, const Block& block, const CallExpr& call)
    -> TypeOperationTarget {
  const std::optional<ExprId> receiver = CalleeReceiver(call.callee);
  const auto* designated = receiver.has_value()
                               ? unit.types.Get(block.exprs.Get(*receiver).type)
                                     .As<DesignationType>()
                               : nullptr;
  const auto* direct = std::get_if<Direct>(&call.callee);
  if (designated == nullptr || direct == nullptr ||
      direct->type_arguments.size() != 1) {
    throw InternalError(
        "mir: a run written within a write is written on a designation, at "
        "the run's type -- please report this as a bug");
  }
  // The run write the designated array's type has: a queue's own, between two
  // positions it clamps, whose value is a queue its type fixes (LRM 7.10.1);
  // any other array's from a position, at the type of the value written.
  const std::optional<TypeInterface> interface =
      TypeInterfaceOf(unit, designated->value);
  if (interface.has_value() &&
      Declares(*interface, support::BuiltinFn::kAssignQueueSlice)) {
    return TypeOperationTarget{
        .type = designated->value,
        .answers = support::BuiltinFn::kAssignQueueSlice};
  }
  return TypeOperationTarget{
      .type = designated->value,
      .answers = support::BuiltinFn::kAssignElementSlice,
      .arguments = {direct->type_arguments.front()}};
}

auto RunWriteElementCount(
    const CompilationUnit& unit, const TypeOperationTarget& run)
    -> std::optional<std::uint64_t> {
  // A run of a fixed count is taken at that run's type; a queue's slice at
  // none.
  if (run.arguments.empty()) {
    return std::nullopt;
  }
  return unit.types.Get(run.arguments.front()).Get<UnpackedArrayType>().count;
}

}  // namespace lyra::mir
