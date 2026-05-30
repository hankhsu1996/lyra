#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

#include <mutex>
#include <utility>
#include <vector>

#include <slang/analysis/AbstractFlowAnalysis.h>
#include <slang/analysis/AnalysisManager.h>
#include <slang/analysis/AnalyzedProcedure.h>
#include <slang/analysis/DFAResults.h>
#include <slang/analysis/DataFlowAnalysis.h>
#include <slang/ast/Compilation.h>
#include <slang/ast/Statement.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/ast_to_hir/state.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Translates slang's `ReadRange`-shaped sensitivity entries (used by
// `getSensitivityList()` and `getImplicitEventReadSets()`) into lyra's
// `SensitivityRead` shape. Disjoint bit ranges for the same symbol are kept
// as-is so downstream can preserve precision when the runtime supports
// bit-level subscription.
template <typename SlangReads>
auto TranslateReadRanges(const SlangReads& reads)
    -> std::vector<SensitivityRead> {
  std::vector<SensitivityRead> entries;
  entries.reserve(reads.size());
  for (const auto& read : reads) {
    entries.push_back({.symbol = read.symbol, .bit_range = read.bitRange});
  }
  return entries;
}

// Flattens the post-DFA `ReadSet` (a map from symbol to bit-interval map)
// into a `SensitivityRead` vector. Mirrors slang's own attribution in
// AnalyzedProcedure.cpp:223-227.
auto FlattenReadSet(const slang::analysis::DFAResults::ReadSet& reads)
    -> std::vector<SensitivityRead> {
  std::vector<SensitivityRead> entries;
  for (const auto& [symbol, bitmap] : reads) {
    for (auto it = bitmap.begin(); it != bitmap.end(); ++it) {
      entries.push_back({.symbol = symbol, .bit_range = it.bounds()});
    }
  }
  return entries;
}

}  // namespace

auto BuildSensitivityReadStore(slang::ast::Compilation& compilation)
    -> SensitivityReadStore {
  SensitivityReadStore out;
  std::mutex out_mutex;

  slang::analysis::AnalysisManager manager;

  // Provider runs first per procedure-like symbol. We do not customise the
  // primary DFA -- we run slang's `DefaultDFA` exactly as the default path
  // would -- but we use the callback's `AnalysisContext` to drive a fresh
  // mini-DFA per wait cond. Calling `AbstractFlowAnalysis::run(expression)`
  // directly bypasses `visitStmt(WaitStatement)`, so the LRM 9.4.2.2 timing-
  // control suppression hook is not triggered and the cond's reads land in
  // the mini-DFA's `getRValues()` like an ordinary expression's reads.
  manager.setCustomDFAProvider(
      [&](slang::analysis::AnalysisContext& context,
          const slang::ast::Symbol& symbol,
          const slang::analysis::AnalyzedProcedure* parent_procedure)
          -> slang::analysis::AnalyzedProcedure {
        slang::analysis::DefaultDFA dfa(context, symbol, true);
        dfa.run();

        if (!dfa.bad) {
          for (const auto* stmt : dfa.getTimedStatements()) {
            if (stmt->kind != slang::ast::StatementKind::Wait) continue;
            const auto& wait_stmt = stmt->as<slang::ast::WaitStatement>();

            // Fresh per-wait DFA: result fields accumulate across run()
            // calls in slang's framework, so re-using `dfa` would conflate
            // procedure-wide and per-wait state.
            slang::analysis::DefaultDFA cond_dfa(context, symbol, false);
            cond_dfa.slang::analysis::AbstractFlowAnalysis<
                slang::analysis::DefaultDFA,
                slang::analysis::DataFlowState>::run(wait_stmt.cond);

            std::scoped_lock lock(out_mutex);
            out.Insert(wait_stmt.cond, FlattenReadSet(cond_dfa.getRValues()));
          }
        }

        if (dfa.bad) {
          return {symbol, parent_procedure};
        }
        return {context, symbol, parent_procedure, dfa};
      });

  // Listener fires after the AnalyzedProcedure is built. Pure post-analysis
  // harvest -- no context needed.
  manager.addListener([&](const slang::analysis::AnalyzedProcedure& ap) {
    const auto& symbol = *ap.analyzedSymbol;

    if (const auto* proc = symbol.as_if<slang::ast::ProceduralBlockSymbol>()) {
      // LRM 9.2.2.2.1: always_comb / always_latch sensitivity attaches to
      // the procedure body statement.
      if (proc->procedureKind == slang::ast::ProceduralBlockKind::AlwaysComb ||
          proc->procedureKind == slang::ast::ProceduralBlockKind::AlwaysLatch) {
        const auto& sens = ap.getSensitivityList();
        if (sens.kind == slang::analysis::SensitivityList::Kind::Implicit) {
          std::scoped_lock lock(out_mutex);
          out.Insert(proc->getBody(), TranslateReadRanges(sens.reads));
        }
      }
      // LRM 9.4.2.2: each `@*` TimedStatement carries its own region read set.
      for (const auto& region : ap.getImplicitEventReadSets()) {
        std::scoped_lock lock(out_mutex);
        out.Insert(*region.statement, TranslateReadRanges(region.reads));
      }
    } else if (
        const auto* ca = symbol.as_if<slang::ast::ContinuousAssignSymbol>()) {
      // LRM 10.3: continuous assignment sensitivity. slang treats `ca` as a
      // procedure for analysis purposes and produces an Implicit sensitivity
      // list under default flags (whole-var reads). We key it by the
      // assignment expression itself -- nothing in the store API names
      // continuous assignment specifically; future per-expression analyses
      // (property / assert / wait_order events) plug into the same bucket.
      const auto& sens = ap.getSensitivityList();
      if (sens.kind == slang::analysis::SensitivityList::Kind::Implicit) {
        std::scoped_lock lock(out_mutex);
        out.Insert(ca->getAssignment(), TranslateReadRanges(sens.reads));
      }
    }
  });

  manager.analyze(compilation);
  return out;
}

auto TranslateSensitivityReads(
    const std::vector<SensitivityRead>& reads,
    const UnitLoweringState& unit_state, const ScopeStack& stack)
    -> std::vector<hir::SensitivityEntry> {
  std::vector<hir::SensitivityEntry> out;
  out.reserve(reads.size());
  for (const auto& read : reads) {
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

}  // namespace lyra::lowering::ast_to_hir
