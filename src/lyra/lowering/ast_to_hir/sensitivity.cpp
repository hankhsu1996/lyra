#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

#include <utility>
#include <vector>

#include <slang/analysis/AbstractFlowAnalysis.h>
#include <slang/analysis/AnalysisManager.h>
#include <slang/analysis/DFAResults.h>
#include <slang/analysis/DataFlowAnalysis.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

namespace lyra::lowering::ast_to_hir {

namespace {

// Flattens slang's `(symbol, bitMap)` `ReadSet` into a vector of
// `(symbol, [lo, hi])` entries. Disjoint ranges for the same symbol stay
// disjoint so downstream can preserve precision.
auto FlattenReadSet(const slang::analysis::DFAResults::ReadSet& reads)
    -> std::vector<SensitivityRead> {
  std::vector<SensitivityRead> out;
  for (const auto& [symbol, bitmap] : reads) {
    for (auto it = bitmap.begin(); it != bitmap.end(); ++it) {
      out.push_back({.symbol = symbol, .footprint = it.bounds()});
    }
  }
  return out;
}

// Runs slang's `DefaultDFA` on a single AST node and harvests its read set.
template <typename Node>
auto RunDfa(
    slang::analysis::AnalysisContext& context,
    const slang::ast::Symbol& containing_symbol, const Node& node)
    -> std::vector<SensitivityRead> {
  slang::analysis::DefaultDFA dfa(context, containing_symbol, false);
  dfa.slang::analysis::AbstractFlowAnalysis<
      slang::analysis::DefaultDFA, slang::analysis::DataFlowState>::run(node);
  return FlattenReadSet(dfa.getRValues());
}

}  // namespace

SensitivityAnalyzer::SensitivityAnalyzer()
    : manager_(std::make_unique<slang::analysis::AnalysisManager>()),
      context_(std::make_unique<slang::analysis::AnalysisContext>(*manager_)) {
}

SensitivityAnalyzer::~SensitivityAnalyzer() = default;
SensitivityAnalyzer::SensitivityAnalyzer(SensitivityAnalyzer&&) noexcept =
    default;
auto SensitivityAnalyzer::operator=(SensitivityAnalyzer&&) noexcept
    -> SensitivityAnalyzer& = default;

auto SensitivityAnalyzer::AnalyzeReads(
    const slang::ast::Expression& expr,
    const slang::ast::Symbol& containing_symbol)
    -> const std::vector<SensitivityRead>& {
  if (const auto it = expression_cache_.find(&expr);
      it != expression_cache_.end()) {
    return it->second;
  }
  auto [inserted_it, _] = expression_cache_.emplace(
      &expr, RunDfa(*context_, containing_symbol, expr));
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeReads(
    const slang::ast::Statement& stmt,
    const slang::ast::Symbol& containing_symbol)
    -> const std::vector<SensitivityRead>& {
  if (const auto it = statement_cache_.find(&stmt);
      it != statement_cache_.end()) {
    return it->second;
  }
  auto [inserted_it, _] = statement_cache_.emplace(
      &stmt, RunDfa(*context_, containing_symbol, stmt));
  return inserted_it->second;
}

}  // namespace lyra::lowering::ast_to_hir
