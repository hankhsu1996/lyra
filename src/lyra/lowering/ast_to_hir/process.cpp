#include "lyra/lowering/ast_to_hir/process.hpp"

#include <expected>
#include <utility>

#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/statement/lower.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> diag::Result<hir::Process> {
  ProcessLoweringState proc_state;

  auto body = LowerStatement(
      unit_facts, proc_state, scope_state, stack, proc.getBody());
  if (!body) return std::unexpected(std::move(body.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body));

  return proc_state.Finalize(hir::ProcessKind::kInitial, body_id);
}

}  // namespace lyra::lowering::ast_to_hir
