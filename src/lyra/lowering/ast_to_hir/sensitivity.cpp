#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <slang/analysis/AbstractFlowAnalysis.h>
#include <slang/analysis/AnalysisManager.h>
#include <slang/analysis/AnalyzedProcedure.h>
#include <slang/analysis/DFAResults.h>
#include <slang/analysis/DataFlowAnalysis.h>
#include <slang/ast/ASTVisitor.h>
#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

namespace lyra::lowering::ast_to_hir {

namespace {

// Every value path the node writes, each with its longest static prefix as this
// elaboration settles it. A path is visited from the expression that heads it,
// which reaches the reads inside its selectors too, so an expression is taken
// whole and not descended into again.
class PathCollector : public slang::ast::ASTVisitor<
                          PathCollector, slang::ast::VisitFlags::AllGood> {
 public:
  PathCollector(
      slang::ast::EvalContext& eval_context,
      std::vector<slang::ast::ValuePath>& paths)
      : eval_context_(&eval_context), paths_(&paths) {
  }

  template <typename T>
    requires std::is_base_of_v<slang::ast::Expression, T>
  void handle(const T& expr) {
    slang::ast::ValuePath::visitPaths(
        expr, *eval_context_,
        [&](const slang::ast::ValuePath& path) { paths_->push_back(path); });
  }

 private:
  slang::ast::EvalContext* eval_context_;
  std::vector<slang::ast::ValuePath>* paths_;
};

// The bits `[lo, hi]` of `symbol` as the source names them: the prefixes among
// `paths` rooted at it whose bits lie inside the range, one per distinct run of
// bits, where together they leave none of it out -- and the run itself where
// they do not.
auto PartRead(
    const slang::ast::ValueSymbol& symbol,
    std::pair<std::uint64_t, std::uint64_t> range,
    std::span<const slang::ast::ValuePath> paths)
    -> std::variant<ReadOfWhole, ReadOfSelects, ReadOfBits> {
  const ReadOfBits unnamed{.first = range.first, .last = range.second};
  std::vector<const slang::ast::ValuePath*> inside;
  for (const slang::ast::ValuePath& path : paths) {
    if (path.lsp == nullptr || path.rootSymbol() != &symbol) continue;
    const auto [first, last] = path.lspBounds;
    if (first < range.first || last > range.second) continue;
    const bool repeated =
        std::ranges::any_of(inside, [&](const slang::ast::ValuePath* seen) {
          return seen->lspBounds == path.lspBounds;
        });
    if (!repeated) inside.push_back(&path);
  }
  std::vector<const slang::ast::ValuePath*> by_first = inside;
  std::ranges::sort(by_first, {}, [](const slang::ast::ValuePath* path) {
    return path->lspBounds.first;
  });
  std::uint64_t next = range.first;
  for (const slang::ast::ValuePath* path : by_first) {
    if (path->lspBounds.first > next) return unnamed;
    next = std::max(next, path->lspBounds.second + 1);
  }
  if (next <= range.second) return unnamed;
  ReadOfSelects named;
  named.prefixes.reserve(inside.size());
  for (const slang::ast::ValuePath* path : inside) {
    named.prefixes.push_back(path->lsp);
  }
  return named;
}

// Flattens slang's `(symbol, bitMap)` `ReadSet` into one read per run of a
// symbol's bits, each named by the prefixes in `paths` that make it up.
// Disjoint runs of the same symbol stay disjoint so downstream can preserve
// precision.
//
// What comes back stands for a set and carries no order of its own: it is
// handed over in whatever order the front end's own container iterates, which
// is where its symbols happened to be allocated. Whoever turns these into
// something the compiled artifact carries settles an order there, against the
// identities that step gives them.
auto FlattenReadSet(
    const slang::analysis::DFAResults::ReadSet& reads,
    std::span<const slang::ast::ValuePath> paths)
    -> std::vector<SensitivityRead> {
  std::vector<SensitivityRead> out;
  for (const auto& [symbol, bitmap] : reads) {
    for (auto it = bitmap.begin(); it != bitmap.end(); ++it) {
      out.push_back(
          {.symbol = symbol, .part = PartRead(*symbol, it.bounds(), paths)});
    }
  }
  return out;
}

// The value paths `node` writes, settled against the scope around
// `containing_symbol`.
template <typename Node>
auto PathsOf(const Node& node, const slang::ast::Symbol& containing_symbol)
    -> std::vector<slang::ast::ValuePath> {
  slang::ast::EvalContext eval_context(containing_symbol);
  std::vector<slang::ast::ValuePath> paths;
  PathCollector collector(eval_context, paths);
  node.visit(collector);
  return paths;
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
  return FlattenReadSet(dfa.getRValues(), PathsOf(node, containing_symbol));
}

// Flattens slang's procedure-level sensitivity list (LRM 9.2.2.2.1) into the
// same shape as a raw read set. slang has already narrowed each entry's bit
// range to the bits that wake the procedure and excluded the procedure's locals
// and self-driven bits.
auto FlattenSensitivityList(
    const slang::analysis::AnalyzedProcedure& analyzed,
    std::span<const slang::ast::ValuePath> paths)
    -> std::vector<SensitivityRead> {
  std::vector<SensitivityRead> out;
  for (const auto& read : analyzed.getSensitivityList().reads) {
    out.push_back(
        {.symbol = read.symbol,
         .part = PartRead(*read.symbol, read.bitRange, paths)});
  }
  return out;
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

auto SensitivityAnalyzer::AnalyzeProcedureSensitivity(
    const slang::ast::ProceduralBlockSymbol& proc)
    -> const std::vector<SensitivityRead>& {
  if (const auto it = procedure_cache_.find(&proc);
      it != procedure_cache_.end()) {
    return it->second;
  }
  slang::analysis::DefaultDFA dfa(*context_, proc, false);
  dfa.run();
  const slang::analysis::AnalyzedProcedure analyzed(
      *context_, proc, nullptr, dfa);
  auto [inserted_it, _] = procedure_cache_.emplace(
      &proc, FlattenSensitivityList(analyzed, PathsOf(proc, proc)));
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeProcedureClock(
    const slang::ast::ProceduralBlockSymbol& proc)
    -> const slang::ast::TimingControl* {
  if (const auto it = procedure_clock_cache_.find(&proc);
      it != procedure_clock_cache_.end()) {
    return it->second;
  }
  slang::analysis::DefaultDFA dfa(*context_, proc, false);
  dfa.run();
  const slang::analysis::AnalyzedProcedure analyzed(
      *context_, proc, nullptr, dfa);
  const auto* clock = analyzed.getInferredClock();
  procedure_clock_cache_.emplace(&proc, clock);
  return clock;
}

}  // namespace lyra::lowering::ast_to_hir
