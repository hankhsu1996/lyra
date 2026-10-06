#include "lyra/mir/expr.hpp"

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

auto MakeAssignExpr(
    const BuiltinMirTypes& builtins, ExprId target, ExprId value,
    std::optional<BinaryOp> compound_op) -> Expr {
  return Expr{
      .data =
          AssignExpr{
              .target = target, .compound_op = compound_op, .value = value},
      .type = builtins.void_type};
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
