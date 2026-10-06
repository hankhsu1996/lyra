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
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

namespace lyra::lowering::ast_to_hir {

namespace {

// Every value path in the node's text, each with its longest static prefix as
// this elaboration settles it. A path is visited from the expression that heads
// it, which reaches the reads inside its selectors too, so an expression is
// taken whole and not descended into again.
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

// Whether the node brings `symbol` into being: a temporary it introduces, or a
// declaration of a scope it opens.
auto IsDeclaredInside(
    const slang::ast::Symbol& symbol, const NodeDeclarations& declarations)
    -> bool {
  if (std::ranges::contains(declarations.temporaries, &symbol)) return true;
  for (const slang::ast::Scope* scope = symbol.getParentScope();
       scope != nullptr; scope = scope->asSymbol().getParentScope()) {
    if (std::ranges::contains(declarations.scopes, scope)) return true;
  }
  return false;
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
  if (!IsDeclaredInside(symbol, declarations)) return true;
  if (symbol.kind != slang::ast::SymbolKind::Variable) return false;
  const auto& variable = symbol.as<slang::ast::VariableSymbol>();
  return variable.lifetime == slang::ast::VariableLifetime::Static &&
         !variable.flags.has(slang::ast::VariableFlags::CompilerGenerated);
}

// The bits `[lo, hi]` of `symbol` as the source names them: the prefixes among
// `paths` rooted at it whose bits lie inside the range, one per distinct set of
// bits, where together they leave none of it out -- and the bits themselves
// where they do not.
auto PartReached(
    const slang::ast::ValueSymbol& symbol,
    std::pair<std::uint64_t, std::uint64_t> range,
    std::span<const slang::ast::ValuePath> paths)
    -> std::variant<WholePart, SelectedParts, UnselectedBits> {
  const UnselectedBits unnamed{.first = range.first, .last = range.second};
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
  SelectedParts named;
  named.prefixes.reserve(inside.size());
  for (const slang::ast::ValuePath* path : inside) {
    named.prefixes.push_back(path->lsp);
  }
  return named;
}

// The expression each name reaching `symbol` starts at, over `paths`, each
// stated once and in the order the text has them.
auto NamesReaching(
    const slang::ast::ValueSymbol& symbol,
    std::span<const slang::ast::ValuePath> paths)
    -> std::vector<const slang::ast::Expression*> {
  std::vector<const slang::ast::Expression*> names;
  for (const slang::ast::ValuePath& path : paths) {
    if (path.rootExpr == nullptr || path.rootSymbol() != &symbol) continue;
    if (!std::ranges::contains(names, path.rootExpr)) {
      names.push_back(path.rootExpr);
    }
  }
  return names;
}

// Flattens slang's `(symbol, bitMap)` `ReadSet` into one read per range of a
// symbol's bits, each named by the prefixes in `paths` that make it up.
// Disjoint ranges of the same symbol stay disjoint so downstream can preserve
// precision.
//
// What comes back stands for a set (LRM 9.4.2.1), and is in the order the
// analyzed text first reads each symbol, which is the order the front end keeps
// them in. Two copies of one text therefore hand their reads over alike, and so
// whatever a later step builds from them, one read at a time, is built alike.
auto FlattenReadSet(
    const slang::analysis::DFAResults::ReadSet& reads,
    std::span<const slang::ast::ValuePath> paths) -> std::vector<AccessedPart> {
  std::vector<AccessedPart> out;
  for (const auto& [symbol, bitmap] : reads) {
    const std::vector<const slang::ast::Expression*> names =
        NamesReaching(*symbol, paths);
    for (auto it = bitmap.begin(); it != bitmap.end(); ++it) {
      out.push_back(
          {.symbol = symbol,
           .part = PartReached(*symbol, it.bounds(), paths),
           .reached_by = names});
    }
  }
  return out;
}

// What `lvalues` write, one write per run of a symbol's bits assigned, each
// named by the prefixes in `paths` that make it up, as a read is.
auto FlattenWrites(
    std::span<const slang::analysis::DFAResults::LValueSymbol> lvalues,
    std::span<const slang::ast::ValuePath> paths) -> std::vector<AccessedPart> {
  std::vector<AccessedPart> out;
  for (const auto& lvalue : lvalues) {
    const slang::ast::ValueSymbol& symbol = *lvalue.symbol;
    const slang::ast::Type& type = symbol.getType();
    // A write is taken out of a read only where the two compare exactly. A
    // packed value's bits do, so the bits written are stated; anything else is
    // watched whole, so only a write of the whole of it is stated, and a write
    // of part of one is left out rather than taking the whole away.
    const bool bit_addressed = type.isIntegral() && !type.isEnum();
    const std::uint64_t width = type.getSelectableWidth();
    const std::vector<const slang::ast::Expression*> names =
        NamesReaching(symbol, paths);
    for (auto it = lvalue.assigned.begin(); it != lvalue.assigned.end(); ++it) {
      const auto [first, last] = it.bounds();
      if (!bit_addressed && (first != 0 || last + 1 < width)) continue;
      out.push_back(
          {.symbol = &symbol,
           .part = PartReached(symbol, it.bounds(), paths),
           .reached_by = names});
    }
  }
  return out;
}

// The value paths in `node`'s text, settled against the scope around
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
// reads and writes from outside itself, given what the node declares.
template <typename Node>
auto RunDfa(
    slang::analysis::AnalysisContext& context,
    const slang::ast::Symbol& containing_symbol, const Node& node,
    const NodeDeclarations& declarations) -> NodeAccesses {
  slang::analysis::DefaultDFA dfa(context, containing_symbol, false);
  dfa.slang::analysis::AbstractFlowAnalysis<
      slang::analysis::DefaultDFA, slang::analysis::DataFlowState>::run(node);
  const std::vector<slang::ast::ValuePath> paths =
      PathsOf(node, containing_symbol);
  NodeAccesses accesses{
      .reads = FlattenReadSet(dfa.getRValues(), paths),
      .writes = FlattenWrites(dfa.getLValues(), paths)};
  const auto inside = [&](const AccessedPart& access) {
    return !IsStateOutside(*access.symbol, declarations);
  };
  std::erase_if(accesses.reads, inside);
  std::erase_if(accesses.writes, inside);
  return accesses;
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
    -> const std::vector<AccessedPart>& {
  if (const auto it = expression_cache_.find(&expr);
      it != expression_cache_.end()) {
    return it->second;
  }
  auto [inserted_it, _] = expression_cache_.emplace(
      &expr,
      RunDfa(*context_, containing_symbol, expr, DeclarationsOf(expr)).reads);
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeReads(
    const slang::ast::Statement& stmt,
    const slang::ast::Symbol& containing_symbol)
    -> const std::vector<AccessedPart>& {
  if (const auto it = statement_cache_.find(&stmt);
      it != statement_cache_.end()) {
    return it->second;
  }
  auto [inserted_it, _] = statement_cache_.emplace(
      &stmt,
      RunDfa(*context_, containing_symbol, stmt, DeclarationsOf(stmt)).reads);
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeAccesses(
    const slang::ast::SubroutineSymbol& subroutine) -> const NodeAccesses& {
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

auto HoldsStateBeforeItRuns(
    const slang::ast::Symbol& symbol,
    const slang::ast::ProceduralBlockSymbol& proc) -> bool {
  return IsStateOutside(symbol, DeclarationsOf(proc.getBody()));
}

auto SensitivityAnalyzer::AnalyzeProcedureText(
    const slang::ast::ProceduralBlockSymbol& proc) -> const NodeAccesses& {
  if (const auto it = procedure_text_cache_.find(&proc);
      it != procedure_text_cache_.end()) {
    return it->second;
  }
  const slang::ast::Statement& body = proc.getBody();
  const NodeDeclarations declarations = DeclarationsOf(body);
  NodeAccesses accesses = RunDfa(*context_, proc, body, declarations);
  // The list leaves out every variable the procedure declares, static ones
  // too, where a wait inside it reads a static one as state (LRM 9.2.2.2.1 a).
  const auto declared = [&](const AccessedPart& access) {
    return IsDeclaredInside(*access.symbol, declarations);
  };
  std::erase_if(accesses.reads, declared);
  std::erase_if(accesses.writes, declared);
  auto [inserted_it, _] =
      procedure_text_cache_.emplace(&proc, std::move(accesses));
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
