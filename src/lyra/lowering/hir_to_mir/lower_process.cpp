#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstddef>
#include <cstdint>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/process.hpp"
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
            return mir::Initial{process_state.stmt_map[p.body.value]};
          },
      },
      data);
}

auto LowerProcess(const UnitLoweringState& unit_state, const hir::Process& src)
    -> mir::Process {
  ProcessLoweringState process_state;
  process_state.expr_map.resize(src.exprs.size());
  process_state.stmt_map.resize(src.stmts.size());

  std::vector<mir::Expr> exprs;
  exprs.reserve(src.exprs.size());
  for (std::size_t i = 0; i < src.exprs.size(); ++i) {
    const mir::ExprId mir_id{static_cast<std::uint32_t>(exprs.size())};
    exprs.push_back(
        mir::Expr{
            .data = LowerExprData(unit_state, src.exprs[i].data),
        });
    process_state.expr_map[i] = mir_id;
  }

  std::vector<mir::Stmt> stmts;
  stmts.reserve(src.stmts.size());
  for (std::size_t i = 0; i < src.stmts.size(); ++i) {
    const mir::StmtId mir_id{static_cast<std::uint32_t>(stmts.size())};
    stmts.push_back(
        mir::Stmt{
            .label = src.stmts[i].label,
            .data = LowerStmtData(unit_state, process_state, src.stmts[i].data),
        });
    process_state.stmt_map[i] = mir_id;
  }

  return mir::Process{
      .name = src.name,
      .data = LowerProcessData(process_state, src.data),
      .exprs = std::move(exprs),
      .stmts = std::move(stmts),
  };
}

}  // namespace lyra::lowering::hir_to_mir
