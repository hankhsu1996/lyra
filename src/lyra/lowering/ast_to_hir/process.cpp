#include "lyra/lowering/ast_to_hir/process.hpp"

#include <expected>
#include <utility>

#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"
#include "lyra/lowering/ast_to_hir/statement/lower.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto FromSlangProceduralBlockKind(slang::ast::ProceduralBlockKind kind)
    -> hir::ProcessKind {
  switch (kind) {
    case slang::ast::ProceduralBlockKind::Initial:
      return hir::ProcessKind::kInitial;
    case slang::ast::ProceduralBlockKind::Final:
      return hir::ProcessKind::kFinal;
    case slang::ast::ProceduralBlockKind::Always:
      return hir::ProcessKind::kAlways;
    case slang::ast::ProceduralBlockKind::AlwaysComb:
      return hir::ProcessKind::kAlwaysComb;
    case slang::ast::ProceduralBlockKind::AlwaysLatch:
      return hir::ProcessKind::kAlwaysLatch;
    case slang::ast::ProceduralBlockKind::AlwaysFF:
      return hir::ProcessKind::kAlwaysFf;
  }
  throw InternalError(
      "ast_to_hir::FromSlangProceduralBlockKind: unknown ProceduralBlockKind");
}

}  // namespace

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> diag::Result<hir::Process> {
  ProcessLoweringState proc_state;

  auto body = LowerStatement(
      unit_facts, proc_state, scope_state, stack, proc.getBody());
  if (!body) return std::unexpected(std::move(body.error()));
  const hir::StmtId body_id = proc_state.AddStmt(*std::move(body));

  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.PointSpanOf(proc.location);
  return proc_state.Finalize(
      FromSlangProceduralBlockKind(proc.procedureKind), span, body_id);
}

}  // namespace lyra::lowering::ast_to_hir
