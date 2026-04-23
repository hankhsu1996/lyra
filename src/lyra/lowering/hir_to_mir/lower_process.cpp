#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstddef>
#include <cstdint>
#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerProcessData(
    const ProcessLoweringState& process_state, const hir::ProcessData& data)
    -> mir::ProcessData {
  return std::visit(
      support::Overloaded{
          [&](const hir::Initial& p) -> mir::ProcessData {
            return mir::Initial{process_state.TranslateStmt(p.body)};
          },
      },
      data);
}

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::Process& src) -> mir::Process {
  const ProcessLoweringFacts facts(unit_facts, src);
  ProcessLoweringState state;

  for (std::size_t i = 0; i < src.exprs.size(); ++i) {
    state.AppendExpr(
        hir::ExprId{static_cast<std::uint32_t>(i)},
        mir::Expr{.data = LowerExprData(unit_state, src.exprs[i].data)});
  }

  for (std::size_t i = 0; i < src.stmts.size(); ++i) {
    state.AppendStmt(
        hir::StmtId{static_cast<std::uint32_t>(i)},
        mir::Stmt{
            .label = src.stmts[i].label,
            .data = LowerStmtData(unit_state, state, src.stmts[i].data),
        });
  }

  return state.Finalize(LowerProcessData(state, src.data));
}

}  // namespace lyra::lowering::hir_to_mir
