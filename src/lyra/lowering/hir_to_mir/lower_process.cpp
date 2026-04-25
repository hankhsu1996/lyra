#include "lyra/lowering/hir_to_mir/lower_process.hpp"

#include <cstddef>
#include <cstdint>
#include <variant>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerProcessKind(hir::ProcessKind kind) -> mir::ProcessKind {
  switch (kind) {
    case hir::ProcessKind::kInitial:
      return mir::ProcessKind::kInitial;
  }
  throw support::InternalError("LowerProcessKind: unknown HIR ProcessKind");
}

void LowerStmtIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::Process& hir_proc, hir::StmtId hir_id,
    BodyLoweringState& body_state, const ScopeStack& stack) {
  const hir::Stmt& s = hir_proc.stmts.at(hir_id.value);
  if (const auto* block = std::get_if<hir::BlockStmt>(&s.data)) {
    for (const auto child : block->statements) {
      LowerStmtIntoBody(
          unit_facts, unit_state, hir_proc, child, body_state, stack);
    }
    return;
  }
  const mir::StmtId id = body_state.AppendStmt(
      mir::Stmt{
          .label = s.label,
          .data =
              LowerStmtData(unit_facts, unit_state, stack, body_state, s.data),
          .child_bodies = {}});
  body_state.AppendRootStmt(id);
}

}  // namespace

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& process_scope, const hir::Process& src)
    -> mir::Process {
  ScopeStack stack;
  const ScopeStackGuard guard(stack, process_scope);

  BodyLoweringState body_state;
  for (std::size_t i = 0; i < src.exprs.size(); ++i) {
    const hir::ExprId hir_id{static_cast<std::uint32_t>(i)};
    body_state.AppendExpr(
        hir_id, mir::Expr{
                    .data = LowerExprData(
                        unit_facts, unit_state, stack, src.exprs[i].data)});
  }

  LowerStmtIntoBody(unit_facts, unit_state, src, src.body, body_state, stack);

  return mir::Process{
      .kind = LowerProcessKind(src.kind), .body = body_state.Finish()};
}

}  // namespace lyra::lowering::hir_to_mir
