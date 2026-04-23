#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"

#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_decl_ref.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::hir_to_mir {

auto TranslateLvalueTargetToMember(
    const UnitLoweringState& unit_state, const hir::Lvalue& lvalue)
    -> mir::MemberId {
  return std::visit(
      support::Overloaded{
          [&](const hir::LocalValueRef& l) -> mir::MemberId {
            return TranslateValueDeclRefToMember(unit_state, l.target);
          },
      },
      lvalue);
}

auto LowerStmtData(
    const UnitLoweringState& unit_state,
    const ProcessLoweringState& process_state, const hir::StmtData& data)
    -> mir::StmtData {
  return std::visit(
      support::Overloaded{
          [&](const hir::BlockingAssignment& s) -> mir::StmtData {
            return mir::Assignment{
                .target = TranslateLvalueTargetToMember(unit_state, s.target),
                .value = process_state.expr_map[s.value.value]};
          },
          [&](const hir::BlockStmt& s) -> mir::StmtData {
            std::vector<mir::StmtId> translated;
            translated.reserve(s.statements.size());
            for (const auto& sid : s.statements) {
              translated.push_back(process_state.stmt_map[sid.value]);
            }
            return mir::BlockStmt{std::move(translated)};
          },
      },
      data);
}

}  // namespace lyra::lowering::hir_to_mir
