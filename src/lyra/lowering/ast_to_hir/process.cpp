#include "lyra/lowering/ast_to_hir/process.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

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

// LRM 9.2.2.2.1 sensitivity is computed by slang::analysis::AnalysisManager;
// here we translate the slang ValueSymbol pointers + bit ranges it produced
// into the HIR StructuralVarRef + bit_range form used downstream.
auto TranslateSensitivityEntries(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> std::vector<hir::SensitivityEntry> {
  std::vector<hir::SensitivityEntry> out;
  const auto& reads_map = unit_facts.SensitivityReads();
  const auto it = reads_map.find(&proc);
  if (it == reads_map.end()) return out;
  for (const auto& read : it->second) {
    const auto* var = read.symbol->as_if<slang::ast::VariableSymbol>();
    if (var == nullptr) continue;
    const auto binding = unit_state.LookupStructuralVarBinding(*var);
    if (!binding.has_value()) continue;
    const auto hops = stack.HopsTo(binding->home_frame);
    if (!hops.has_value()) continue;
    out.push_back(
        hir::SensitivityEntry{
            .ref = hir::StructuralVarRef{.hops = *hops, .var = binding->var_id},
            .bit_range = read.bit_range});
  }
  return out;
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
    out.implicit_sensitivity_list = TranslateSensitivityEntries(
        unit_facts, scope_state.UnitState(), stack, proc);
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
