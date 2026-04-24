#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <variant>

#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/internal_error.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerStmtData(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const BodyLoweringState& body_state,
    const hir::StmtData& data) -> mir::StmtData {
  return std::visit(
      support::Overloaded{
          [&](const hir::BlockingAssignment& s) -> mir::StmtData {
            return mir::Assignment{
                .target = TranslateLvalueTarget(
                    unit_facts, unit_state, stack, s.target),
                .value = body_state.TranslateExpr(s.value)};
          },
          [](const hir::BlockStmt&) -> mir::StmtData {
            throw support::InternalError(
                "LowerStmtData: BlockStmt must be handled by "
                "LowerStmtIntoBody");
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
