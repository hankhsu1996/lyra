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
#include <slang/analysis/AnalysisOptions.h>
#include <slang/analysis/AnalyzedProcedure.h>
#include <slang/analysis/DFAResults.h>
#include <slang/analysis/DataFlowAnalysis.h>
#include <slang/ast/ASTVisitor.h>
#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Patterns.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
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

// What a node brings into being itself: the scopes it opens, whose
// declarations are its own, and the variables it introduces for its own
// evaluation -- an array method's iterator (LRM 7.12) and a pattern's binding
// (LRM 12.6) -- which belong to no scope it opens.
struct NodeDeclarations {
  std::vector<const slang::ast::Scope*> scopes;
  std::vector<const slang::ast::ValueSymbol*> temporaries;
};

class DeclarationsCollector
    : public slang::ast::ASTVisitor<
          DeclarationsCollector, slang::ast::VisitFlags::AllGood> {
 public:
  explicit DeclarationsCollector(NodeDeclarations& declarations)
      : declarations_(&declarations) {
  }

  void handle(const slang::ast::BlockStatement& block) {
    if (block.blockSymbol != nullptr) {
      declarations_->scopes.push_back(block.blockSymbol);
    }
    visitDefault(block);
  }

  void handle(const slang::ast::CallExpression& call) {
    if (const auto* system =
            std::get_if<slang::ast::CallExpression::SystemCallInfo>(
                &call.subroutine)) {
      if (const slang::ast::ValueSymbol* iterator =
              system->getIteratorInfo().second;
          iterator != nullptr) {
        declarations_->temporaries.push_back(iterator);
      }
    }
    visitDefault(call);
  }

  void handle(const slang::ast::VariablePattern& pattern) {
    declarations_->temporaries.push_back(&pattern.variable);
  }

 private:
  NodeDeclarations* declarations_;
};

template <typename Node>
auto DeclarationsOf(const Node& node) -> NodeDeclarations {
  NodeDeclarations declarations;
  DeclarationsCollector collector(declarations);
  node.visit(collector);
  return declarations;
}

// Whether a read of `symbol` inside a node is a read of state that is there
// before the node runs. What is declared outside is; so is a static variable
// the source declares inside, which lives from time zero (LRM 6.21). An
// automatic one, a formal and a function's result come into being as the node
// is entered, and a temporary holds only what the node puts in it.
auto IsStateOutside(
    const slang::ast::Symbol& symbol, const NodeDeclarations& declarations)
    -> bool {
  if (std::ranges::contains(declarations.temporaries, &symbol)) return false;
  bool declared_inside = false;
  for (const slang::ast::Scope* scope = symbol.getParentScope();
       scope != nullptr && !declared_inside;
       scope = scope->asSymbol().getParentScope()) {
    declared_inside = std::ranges::contains(declarations.scopes, scope);
  }
  if (!declared_inside) return true;
  if (symbol.kind != slang::ast::SymbolKind::Variable) return false;
  const auto& variable = symbol.as<slang::ast::VariableSymbol>();
  return variable.lifetime == slang::ast::VariableLifetime::Static &&
         !variable.flags.has(slang::ast::VariableFlags::CompilerGenerated);
}

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

// Runs slang's `DefaultDFA` on a single AST node and harvests the state it
// reads from outside itself, given what the node declares.
template <typename Node>
auto RunDfa(
    slang::analysis::AnalysisContext& context,
    const slang::ast::Symbol& containing_symbol, const Node& node,
    const NodeDeclarations& declarations) -> std::vector<SensitivityRead> {
  slang::analysis::DefaultDFA dfa(context, containing_symbol, false);
  dfa.slang::analysis::AbstractFlowAnalysis<
      slang::analysis::DefaultDFA, slang::analysis::DataFlowState>::run(node);
  std::vector<SensitivityRead> reads =
      FlattenReadSet(dfa.getRValues(), PathsOf(node, containing_symbol));
  std::erase_if(reads, [&](const SensitivityRead& read) {
    return !IsStateOutside(*read.symbol, declarations);
  });
  return reads;
}

// Flattens slang's procedure-level sensitivity list (LRM 9.2.2.2.1) into the
// same shape as a node's reads. slang has already narrowed each entry's bit
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
    : manager_(
          std::make_unique<slang::analysis::AnalysisManager>(
              slang::analysis::AnalysisOptions{
                  .flags = slang::analysis::AnalysisFlags::
                      IgnoreConstantConditions})),
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
      &expr, RunDfa(*context_, containing_symbol, expr, DeclarationsOf(expr)));
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
      &stmt, RunDfa(*context_, containing_symbol, stmt, DeclarationsOf(stmt)));
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeReads(
    const slang::ast::SubroutineSymbol& subroutine)
    -> const std::vector<SensitivityRead>& {
  if (const auto it = subroutine_cache_.find(&subroutine);
      it != subroutine_cache_.end()) {
    return it->second;
  }
  const slang::ast::Statement& body = subroutine.getBody();
  NodeDeclarations declarations = DeclarationsOf(body);
  declarations.scopes.push_back(&subroutine);
  auto [inserted_it, _] = subroutine_cache_.emplace(
      &subroutine, RunDfa(*context_, subroutine, body, declarations));
  return inserted_it->second;
}

auto HoldsStateBeforeItRuns(
    const slang::ast::Symbol& symbol,
    const slang::ast::SubroutineSymbol& subroutine) -> bool {
  return IsStateOutside(
      symbol, NodeDeclarations{.scopes = {&subroutine}, .temporaries = {}});
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
