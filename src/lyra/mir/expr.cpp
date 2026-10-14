#include "lyra/mir/expr.hpp"

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/compilation_unit.hpp"

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
                    [](const ExternalUnitVariableRef&) { return true; },
                    [](const ExternalStaticPropertyRef&) { return true; },
                    [](const DefinitionRef&) { return false; },
                    [](const EnumTableRef&) { return false; },
                    [](const IntegralConstantRef&) { return false; },
                    [](const ClassConstantRef&) { return false; },
                    [](const FunctionRef&) { return false; }},
                e.target);
          },
          [](const DerefExpr&) { return true; },
          [](const FieldAccessExpr&) { return true; },
          [](const StringLiteral&) { return false; },
          [](const NullLiteral&) { return false; },
          [](const MachineBoolLiteral&) { return false; },
          [](const MachineIntLiteral&) { return false; },
          [](const MachineFloatLiteral&) { return false; },
          [](const UnaryExpr&) { return false; },
          [](const BinaryExpr&) { return false; },
          [](const CastExpr&) { return false; },
          [](const DynamicCastExpr&) { return false; },
          [](const ConditionalExpr&) { return false; },
          [](const BlockExpr&) { return false; },
          [](const AssignExpr&) { return false; },
          [](const CallExpr&) { return false; },
          [](const NewExpr&) { return false; },
          [](const AddressOfExpr&) { return false; },
          [](const BorrowExpr&) { return false; },
          [](const MoveExpr&) { return false; },
          [](const CopyExpr&) { return false; },
          [](const ClosureExpr&) { return false; },
          [](const CompositeExpr&) { return false; },
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

}  // namespace lyra::mir
