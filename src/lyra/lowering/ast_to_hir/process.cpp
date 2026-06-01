#include "lyra/lowering/ast_to_hir/process.hpp"

#include <algorithm>
#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

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

// LRM 9.2.2.2.1: a symbol is local to `proc` if it is an automatic-lifetime
// variable, or if it is declared inside one of `proc`'s statement blocks.
// `ProceduralBlockSymbol` itself is not a `Scope`, so its statement blocks
// hang off the surrounding scope -- we walk the symbol's parent-scope chain
// up to the outermost `StatementBlockSymbol` and check whether that block is
// one of `proc.getBlocks()`.
auto IsLocalTo(
    const slang::ast::ValueSymbol& sym,
    const slang::ast::ProceduralBlockSymbol& proc) -> bool {
  if (const auto* var = sym.as_if<slang::ast::VariableSymbol>();
      var != nullptr &&
      var->lifetime == slang::ast::VariableLifetime::Automatic) {
    return true;
  }
  const slang::ast::Scope* scope = sym.getParentScope();
  const slang::ast::StatementBlockSymbol* root_block = nullptr;
  while (scope != nullptr &&
         scope->asSymbol().kind == slang::ast::SymbolKind::StatementBlock) {
    root_block = &scope->asSymbol().as<slang::ast::StatementBlockSymbol>();
    scope = root_block->getParentScope();
  }
  if (root_block == nullptr) return false;
  const auto blocks = proc.getBlocks();
  return std::ranges::any_of(
      blocks, [&](const auto* block) { return block == root_block; });
}

// LRM 9.2.2.2.1 procedure-body sensitivity list: the reads of `body`,
// excluding any symbol declared inside `proc`. Empty result is legal -- a
// SensitivityWaitStmt with an empty reads list means "wait forever", the
// correct degenerate behaviour for a body with no external reads.
auto ProcedureBodySensitivity(
    SensitivityAnalyzer& analyzer, const UnitLoweringState& unit_state,
    const ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> std::vector<hir::SensitivityEntry> {
  const auto& raw = analyzer.AnalyzeReads(proc.getBody(), proc);
  std::vector<SensitivityRead> filtered;
  filtered.reserve(raw.size());
  for (const auto& read : raw) {
    if (!IsLocalTo(*read.symbol, proc)) {
      filtered.push_back(read);
    }
  }
  return TranslateSensitivityReads(filtered, unit_state, stack);
}

}  // namespace

auto LowerProcess(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ProceduralBlockSymbol& proc)
    -> diag::Result<hir::Process> {
  ProcessLoweringState proc_state(proc);

  auto root_stmt = LowerStatement(
      unit_facts, proc_state, scope_state, stack, proc.getBody());
  if (!root_stmt) return std::unexpected(std::move(root_stmt.error()));
  const hir::StmtId root_stmt_id = proc_state.AddStmt(*std::move(root_stmt));

  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.PointSpanOf(proc.location);
  const auto kind = FromSlangProceduralBlockKind(proc.procedureKind);

  hir::Process out{
      .kind = kind,
      .span = span,
      .body = proc_state.FinalizeBody(root_stmt_id),
      .implicit_sensitivity_list = {}};
  if (kind == hir::ProcessKind::kAlwaysComb ||
      kind == hir::ProcessKind::kAlwaysLatch) {
    // LRM 9.2.2.2.1: implicit sensitivity is a property of the always_comb /
    // always_latch procedure itself (no `@*` token in source). Attach it to
    // the Process; HIR -> MIR materialises the trailing wait stmt.
    out.implicit_sensitivity_list = ProcedureBodySensitivity(
        unit_facts.Sensitivity(), scope_state.UnitState(), stack, proc);
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
