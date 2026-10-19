#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

#include <algorithm>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <slang/analysis/AnalysisManager.h>
#include <slang/analysis/AnalysisOptions.h>
#include <slang/analysis/AnalyzedProcedure.h>
#include <slang/analysis/DataFlowAnalysis.h>
#include <slang/ast/ASTVisitor.h>
#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Patterns.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/lowering/ast_to_hir/expression/query_reads_its_operand.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// One name a text reaches a declaration by: the declaration, the longest
// static prefix the source wrote, the expression the name starts at, whether
// the name goes on past its prefix, and whether the text writes it.
struct NameInText {
  const slang::ast::ValueSymbol* symbol = nullptr;
  const slang::ast::Expression* prefix = nullptr;
  const slang::ast::Expression* start = nullptr;
  bool ends_at_prefix = false;
  bool written = false;
};

// A static class property is where a path through it starts (LRM 8.9).
auto IsStaticProperty(const slang::ast::MemberAccessExpression& access)
    -> bool {
  const auto* property = access.member.as_if<slang::ast::ClassPropertySymbol>();
  return property != nullptr &&
         property->lifetime == slang::ast::VariableLifetime::Static;
}

// A member reached through a class handle (LRM 8.4) is no part of the
// variable holding the handle, so writing it writes nothing that variable
// holds.
auto IsThroughHandle(const slang::ast::MemberAccessExpression& access) -> bool {
  const auto& value_type = access.value().type->getCanonicalType();
  return value_type.isObjectHandleType() || value_type.isVoid();
}

// Every name in a node's text, in the order the text has them, each marked
// read or written as LRM 9.2.2.2.1 and 9.4.2.2 count it: an assignment's
// target, an `++` / `--` operand, an output argument is written; a
// by-reference argument is both; what selects inside any name, a procedural
// `force` target, and everything else is read. A timing control's own
// expressions count for neither (LRM 9.2.2.2.1 c), and neither does what runs
// apart from the statement: a concurrent assertion, a deferred assertion's
// action, and the reads of an immediate assertion's action. Every name counts
// wherever the text has it, so a branch that a value the body is built with
// never takes still counts, and so does a select that value puts past its
// value's end.
class NameCollector : public slang::ast::ASTVisitor<
                          NameCollector, slang::ast::VisitFlags::AllGood> {
 public:
  NameCollector(
      slang::ast::EvalContext& eval_context, std::vector<NameInText>& names)
      : eval_context_(&eval_context), names_(&names) {
  }

  template <typename T>
    requires(
        std::is_same_v<T, slang::ast::NamedValueExpression> ||
        std::is_same_v<T, slang::ast::HierarchicalValueExpression> ||
        std::is_same_v<T, slang::ast::ElementSelectExpression> ||
        std::is_same_v<T, slang::ast::RangeSelectExpression> ||
        std::is_same_v<T, slang::ast::MemberAccessExpression>)
  void handle(const T& expr) {
    const slang::ast::ValuePath path(expr, *eval_context_);
    bool through_a_handle = false;
    for (const slang::ast::Expression& elem : path) {
      const auto* access = elem.as_if<slang::ast::MemberAccessExpression>();
      through_a_handle |= access != nullptr && IsThroughHandle(*access);
    }
    const bool written = writing_ && !through_a_handle;
    const slang::ast::Expression* prefix =
        PrefixAsWritten(path, *eval_context_);
    if (prefix != nullptr && (written || !in_action_)) {
      names_->push_back(
          NameInText{
              .symbol = path.rootSymbol(),
              .prefix = prefix,
              .start = path.rootExpr,
              .ends_at_prefix = path.fullExpr == prefix,
              .written = written});
    }
    const bool outer_writing = std::exchange(writing_, false);
    for (const slang::ast::Expression& elem : path) {
      if (const auto* select =
              elem.as_if<slang::ast::ElementSelectExpression>()) {
        select->selector().visit(*this);
      } else if (
          const auto* range = elem.as_if<slang::ast::RangeSelectExpression>()) {
        range->left().visit(*this);
        range->right().visit(*this);
      } else if (
          const auto* access =
              elem.as_if<slang::ast::MemberAccessExpression>()) {
        // What names a static property -- `objs[i].count` -- is still read
        // to reach it.
        if (IsStaticProperty(*access)) access->value().visit(*this);
      } else if (elem.as_if<slang::ast::ValueExpressionBase>() == nullptr) {
        elem.visit(*this);
      }
    }
    writing_ = outer_writing;
  }

  void handle(const slang::ast::AssignmentExpression& assign) {
    if (assign.isLValueArg()) {
      Write(assign.left());
      return;
    }
    Write(assign.left());
    assign.right().visit(*this);
  }

  void handle(const slang::ast::UnaryExpression& unary) {
    if (slang::ast::OpInfo::isLValue(unary.op)) {
      Write(unary.operand());
      return;
    }
    visitDefault(unary);
  }

  void handle(const slang::ast::CallExpression& call) {
    call.visitExprsNoArgs(*this);
    const auto args = call.arguments();
    if (const auto* system =
            std::get_if<slang::ast::CallExpression::SystemCallInfo>(
                &call.subroutine)) {
      const auto& subroutine = *system->subroutine;
      // A query's operand is bound unevaluated whatever its type, and where
      // the answer is the operand's current extent the run reads it all the
      // same (LRM 20.6.2, 20.7).
      const bool operand_read = QueryReadsItsOperand(call);
      for (std::size_t i = 0; i < args.size(); ++i) {
        const bool read_all_the_same = i == 0 && operand_read;
        if (subroutine.isArgUnevaluated(i) && !read_all_the_same) continue;
        if (subroutine.isArgByRef(i)) {
          ReadAndWrite(*args[i]);
        } else {
          args[i]->visit(*this);
        }
      }
      return;
    }
    const auto formals =
        std::get<const slang::ast::SubroutineSymbol*>(call.subroutine)
            ->getArguments();
    for (std::size_t i = 0; i < args.size(); ++i) {
      const auto& formal = *formals[i];
      if (formal.direction == slang::ast::ArgumentDirection::Ref &&
          !formal.flags.has(slang::ast::VariableFlags::Const)) {
        ReadAndWrite(*args[i]);
      } else {
        args[i]->visit(*this);
      }
    }
  }

  // A procedural force names the variable it overrides without driving it.
  void handle(const slang::ast::ProceduralAssignStatement& stmt) {
    if (stmt.isForce &&
        stmt.assignment.kind == slang::ast::ExpressionKind::Assignment) {
      const auto& assign =
          stmt.assignment.as<slang::ast::AssignmentExpression>();
      assign.left().visit(*this);
      assign.right().visit(*this);
      return;
    }
    visitDefault(stmt);
  }

  // An automatic's initializer runs each time its block is entered; a static
  // one's runs once before time zero (LRM 6.21), so it reads nothing here.
  void handle(const slang::ast::VariableDeclStatement& stmt) {
    if (stmt.symbol.lifetime == slang::ast::VariableLifetime::Static) return;
    if (const slang::ast::Expression* init = stmt.symbol.getInitializer();
        init != nullptr) {
      init->visit(*this);
    }
  }

  void handle(const slang::ast::TimedStatement& stmt) {
    stmt.stmt.visit(*this);
  }

  void handle(const slang::ast::WaitStatement& stmt) {
    stmt.stmt.visit(*this);
  }

  void handle(const slang::ast::EventTriggerStatement& stmt) {
    stmt.target.visit(*this);
  }

  void handle(const slang::ast::WaitOrderStatement& stmt) {
    if (stmt.ifTrue != nullptr) stmt.ifTrue->visit(*this);
    if (stmt.ifFalse != nullptr) stmt.ifFalse->visit(*this);
  }

  // A concurrent assertion runs apart from the statement that holds it (LRM
  // 16.14.6), so it is no part of what that statement reads or writes; any
  // other statement is read through.
  template <typename T>
    requires std::is_base_of_v<slang::ast::Statement, T>
  void handle(const T& stmt) {
    if constexpr (!std::is_same_v<
                      T, slang::ast::ConcurrentAssertionStatement>) {
      visitDefault(stmt);
    }
  }

  // A deferred assertion's action runs in a later region (LRM 16.4), apart
  // from the statement that holds it.
  void handle(const slang::ast::ImmediateAssertionStatement& stmt) {
    stmt.cond.visit(*this);
    if (stmt.isDeferred) return;
    const bool outer = std::exchange(in_action_, true);
    if (stmt.ifTrue != nullptr) stmt.ifTrue->visit(*this);
    if (stmt.ifFalse != nullptr) stmt.ifFalse->visit(*this);
    in_action_ = outer;
  }

 private:
  void Write(const slang::ast::Expression& target) {
    const bool outer = std::exchange(writing_, true);
    target.visit(*this);
    writing_ = outer;
  }

  // A by-reference argument names storage the callee reads and writes where it
  // lies (LRM 13.5.2).
  void ReadAndWrite(const slang::ast::Expression& target) {
    Write(target);
    target.visit(*this);
  }

  // The longest static prefix of `path` (LRM 11.5.3) as the source wrote it:
  // the outermost select whose own selector and every selector inside it are
  // constant expressions, and the name itself where there is none. A select
  // is static where its value has a fixed range and its selector evaluates as
  // a constant expression around the node, which a select past its value's
  // end still does. None where the path starts at something no name declares.
  static auto PrefixAsWritten(
      const slang::ast::ValuePath& path, slang::ast::EvalContext& eval_context)
      -> const slang::ast::Expression* {
    const slang::ast::Expression* prefix = nullptr;
    const auto note = [&](const slang::ast::Expression& select,
                          bool is_static) {
      if (!is_static) {
        prefix = nullptr;
      } else if (prefix == nullptr) {
        prefix = &select;
      }
    };
    for (const slang::ast::Expression& elem : path) {
      if (const auto* select =
              elem.as_if<slang::ast::ElementSelectExpression>()) {
        note(elem, select->isConstantSelect(eval_context));
      } else if (
          const auto* range = elem.as_if<slang::ast::RangeSelectExpression>()) {
        note(elem, range->isConstantSelect(eval_context));
      } else if (
          const auto* access =
              elem.as_if<slang::ast::MemberAccessExpression>()) {
        if (!IsStaticProperty(*access)) {
          note(elem, !IsThroughHandle(*access));
        } else if (prefix == nullptr) {
          prefix = &elem;
        }
      } else if (elem.as_if<slang::ast::ValueExpressionBase>() != nullptr) {
        if (prefix == nullptr) prefix = &elem;
      } else {
        return nullptr;
      }
    }
    return path.rootSymbol() == nullptr ? nullptr : prefix;
  }

  slang::ast::EvalContext* eval_context_;
  std::vector<NameInText>* names_;
  bool writing_ = false;
  bool in_action_ = false;
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

// What `name` states of its declaration: all of it where its prefix is the
// name itself, or where the declaration has no part a select states; the part
// its prefix names otherwise. A write that goes on past its prefix into a
// value whose parts are stated by selects -- an entry of an associative array,
// an element at an index the program computes -- writes only some of what the
// prefix names, so it states nothing, since what a write states is taken out
// of what is read.
auto PartNamed(const NameInText& name) -> std::optional<StatedPart> {
  const PartAddressing addressing = PartAddressingOf(name.symbol->getType());
  if (name.prefix == name.start || addressing == PartAddressing::kWholeOnly) {
    return WholePart{};
  }
  if (name.written && !name.ends_at_prefix &&
      addressing == PartAddressing::kSelects) {
    return std::nullopt;
  }
  return SelectedParts{.prefixes = {name.prefix}};
}

// One access per declaration `names` read, or wrote, in the order the text
// first names each, holding every prefix it names that declaration by once;
// all of the declaration where any name reaches all of it.
auto AccessesOf(std::span<const NameInText> names, bool written)
    -> std::vector<AccessedPart> {
  std::vector<AccessedPart> out;
  for (const NameInText& name : names) {
    if (name.written != written) continue;
    std::optional<StatedPart> part = PartNamed(name);
    if (!part.has_value()) continue;
    auto it = std::ranges::find(out, name.symbol, &AccessedPart::symbol);
    if (it == out.end()) {
      out.push_back(
          {.symbol = name.symbol, .part = *std::move(part), .reached_by = {}});
      continue;
    }
    auto* named = std::get_if<SelectedParts>(&it->part);
    if (named == nullptr) continue;
    if (std::holds_alternative<WholePart>(*part)) {
      it->part = WholePart{};
      continue;
    }
    const slang::ast::Expression* prefix =
        std::get<SelectedParts>(*part).prefixes.front();
    if (!std::ranges::contains(named->prefixes, prefix)) {
      named->prefixes.push_back(prefix);
    }
  }
  // Every name the text reaches a declaration by, read or written, is one it
  // is reached by from another instance too.
  for (AccessedPart& access : out) {
    for (const NameInText& name : names) {
      if (name.symbol == access.symbol &&
          !std::ranges::contains(access.reached_by, name.start)) {
        access.reached_by.push_back(name.start);
      }
    }
  }
  return out;
}

// What `node` reads and writes of the state there before it runs, given what
// it declares, settled against the scope around `containing_symbol`.
template <typename Node>
auto AccessesOfNode(
    const slang::ast::Symbol& containing_symbol, const Node& node,
    const NodeDeclarations& declarations) -> NodeAccesses {
  slang::ast::EvalContext eval_context(containing_symbol);
  std::vector<NameInText> names;
  NameCollector collector(eval_context, names);
  node.visit(collector);
  std::erase_if(names, [&](const NameInText& name) {
    return !IsStateOutside(*name.symbol, declarations);
  });
  return NodeAccesses{
      .reads = AccessesOf(names, false), .writes = AccessesOf(names, true)};
}

// The variables a statement uses only to control its `for` loops: each is
// assigned by the initialization of a loop (LRM 12.7.1) before that loop reads
// it, and the statement names it nowhere outside such a loop.
class LoopControlCollector
    : public slang::ast::ASTVisitor<
          LoopControlCollector, slang::ast::VisitFlags::AllGood> {
 public:
  void handle(const slang::ast::ForLoopStatement& loop) {
    // What an initialization reads, it reads before the variable it assigns is
    // under the loop's control, so `for (i = i + 1; ...)` still names `i`.
    std::vector<const slang::ast::Symbol*> controlled;
    for (const slang::ast::Expression* initializer : loop.initializers) {
      const slang::ast::Symbol* assigned = VariableAssignedWhole(*initializer);
      if (assigned == nullptr) {
        initializer->visit(*this);
        continue;
      }
      initializer->as<slang::ast::AssignmentExpression>().right().visit(*this);
      controlled.push_back(assigned);
    }
    for (const slang::ast::VariableSymbol* declared : loop.loopVars) {
      if (const slang::ast::Expression* value = declared->getInitializer();
          value != nullptr) {
        value->visit(*this);
      }
    }
    for (const slang::ast::Symbol* symbol : controlled) {
      if (!std::ranges::contains(controls_, symbol)) {
        controls_.push_back(symbol);
      }
    }
    under_control_.insert(
        under_control_.end(), controlled.begin(), controlled.end());
    if (loop.stopExpr != nullptr) loop.stopExpr->visit(*this);
    for (const slang::ast::Expression* step : loop.steps) {
      step->visit(*this);
    }
    loop.body.visit(*this);
    under_control_.resize(under_control_.size() - controlled.size());
  }

  void handle(const slang::ast::NamedValueExpression& name) {
    if (!std::ranges::contains(under_control_, &name.symbol) &&
        !std::ranges::contains(named_outside_, &name.symbol)) {
      named_outside_.push_back(&name.symbol);
    }
  }

  [[nodiscard]] auto OnlyLoopControls() const
      -> std::vector<const slang::ast::Symbol*> {
    std::vector<const slang::ast::Symbol*> only;
    for (const slang::ast::Symbol* symbol : controls_) {
      if (!std::ranges::contains(named_outside_, symbol)) {
        only.push_back(symbol);
      }
    }
    return only;
  }

 private:
  // The variable `expr` assigns by name with a plain `=`, and null where it is
  // any other expression.
  static auto VariableAssignedWhole(const slang::ast::Expression& expr)
      -> const slang::ast::Symbol* {
    if (expr.kind != slang::ast::ExpressionKind::Assignment) return nullptr;
    const auto& assignment = expr.as<slang::ast::AssignmentExpression>();
    if (assignment.isCompound()) return nullptr;
    const slang::ast::Expression& target = assignment.left();
    if (target.kind != slang::ast::ExpressionKind::NamedValue) return nullptr;
    return &target.as<slang::ast::NamedValueExpression>().symbol;
  }

  std::vector<const slang::ast::Symbol*> controls_;
  std::vector<const slang::ast::Symbol*> under_control_;
  std::vector<const slang::ast::Symbol*> named_outside_;
};

}  // namespace

auto PartAddressingOf(const slang::ast::Type& type) -> PartAddressing {
  const slang::ast::Type& canonical = type.getCanonicalType();
  if (canonical.isIntegral() && !canonical.isEnum()) {
    return PartAddressing::kBits;
  }
  if (canonical.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType ||
      canonical.isUnpackedStruct()) {
    return PartAddressing::kSelects;
  }
  return PartAddressing::kWholeOnly;
}

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
      AccessesOfNode(containing_symbol, expr, DeclarationsOf(expr)).reads);
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeImplicitEventList(
    const slang::ast::Statement& stmt,
    const slang::ast::Symbol& containing_symbol)
    -> const std::vector<AccessedPart>& {
  if (const auto it = statement_cache_.find(&stmt);
      it != statement_cache_.end()) {
    return it->second;
  }
  std::vector<AccessedPart> reads =
      AccessesOfNode(containing_symbol, stmt, DeclarationsOf(stmt)).reads;
  LoopControlCollector collector;
  stmt.visit(collector);
  const std::vector<const slang::ast::Symbol*> left_out =
      collector.OnlyLoopControls();
  std::erase_if(reads, [&](const AccessedPart& read) {
    return std::ranges::contains(left_out, read.symbol);
  });
  auto [inserted_it, _] = statement_cache_.emplace(&stmt, std::move(reads));
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
      &subroutine, AccessesOfNode(subroutine, body, declarations));
  return inserted_it->second;
}

auto SensitivityAnalyzer::AnalyzeProcedureText(
    const slang::ast::ProceduralBlockSymbol& proc) -> const NodeAccesses& {
  if (const auto it = procedure_text_cache_.find(&proc);
      it != procedure_text_cache_.end()) {
    return it->second;
  }
  const slang::ast::Statement& body = proc.getBody();
  const NodeDeclarations declarations = DeclarationsOf(body);
  NodeAccesses accesses = AccessesOfNode(proc, body, declarations);
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
