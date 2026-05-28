#include "lyra/lowering/ast_to_hir/process.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/ast_to_hir/facts.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
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

// Looks up sensitivity for `key_stmt` from the precomputed slang analysis
// and translates it into HIR identity entries. Empty result is legal (no
// reads, or unknown key) -- a SensitivityWaitStmt with an empty list means
// "wait forever", which is the correct degenerate behaviour.
auto LookupSensitivityEntries(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::Statement& key_stmt)
    -> std::vector<hir::SensitivityEntry> {
  const auto& reads_map = unit_facts.SensitivityReads();
  const auto it = reads_map.find(&key_stmt);
  if (it == reads_map.end()) return {};
  return TranslateSensitivityReads(it->second, unit_state, stack);
}

}  // namespace

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> diag::Result<hir::Process> {
  ProcessLoweringState proc_state;

  auto root_stmt = LowerStatement(
      unit_facts, proc_state, scope_state, stack, proc.getBody());
  if (!root_stmt) return std::unexpected(std::move(root_stmt.error()));
  const hir::StmtId root_stmt_id = proc_state.AddStmt(*std::move(root_stmt));

  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.PointSpanOf(proc.location);
  const auto kind = FromSlangProceduralBlockKind(proc.procedureKind);

  auto out = proc_state.Finalize(kind, span, root_stmt_id);
  if (kind == hir::ProcessKind::kAlwaysComb ||
      kind == hir::ProcessKind::kAlwaysLatch) {
    // LRM 9.2.2.2.1: implicit sensitivity is a property of the always_comb /
    // always_latch procedure itself (no `@*` token in source). Attach it to
    // the Process; HIR -> MIR materialises the trailing wait stmt.
    out.implicit_sensitivity_list = LookupSensitivityEntries(
        unit_facts, scope_state.UnitState(), stack, proc.getBody());
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
