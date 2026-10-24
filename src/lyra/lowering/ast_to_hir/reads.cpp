#include "lyra/lowering/ast_to_hir/reads.hpp"

#include <concepts>
#include <expected>
#include <iterator>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/ASTVisitor.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// A method the runtime library carries out, which no unit declares a body for.
auto IsRuntimeLibraryMethod(const slang::ast::SubroutineSymbol& method)
    -> bool {
  const slang::ast::Scope* scope = method.getParentScope();
  if (scope == nullptr) return false;
  const auto* owner = scope->asSymbol().as_if<slang::ast::ClassType>();
  return owner != nullptr && ImportedRuntimeClassOf(*owner).has_value();
}

// Walks a function body's or a procedure's statements for the calls an
// implicit list follows into the function called (LRM 9.2.2.2.1): those the
// clause analyzes as normal functions -- a function of a design element or a
// package, one named hierarchically, a static method named through its class.
// A method called on an object, a static one included, adds nothing but its
// arguments, which are the caller's text; a task adds nothing at all; foreign
// code and a method of a class the runtime library defines (LRM 9.7 `process`)
// read nothing a list can hold; and a construction is a reference to a class
// object.
class ListedCalls : public slang::ast::ASTVisitor<
                        ListedCalls, slang::ast::VisitFlags::AllGood> {
 public:
  ListedCalls(ProcessLowerer& proc, WalkFrame frame)
      : proc_(&proc), frame_(std::move(frame)) {
  }

  void handle(const slang::ast::CallExpression& call) {
    if (failure_.has_value()) return;
    if (const auto* const* callee =
            std::get_if<const slang::ast::SubroutineSymbol*>(
                &call.subroutine)) {
      if (IsListed(call, **callee)) Add(call);
    }
    visitDefault(call);
  }

  auto Take() && -> diag::Result<std::vector<hir::ExprId>> {
    if (failure_.has_value()) return std::unexpected(*std::move(failure_));
    return std::move(calls_);
  }

 private:
  static auto IsListed(
      const slang::ast::CallExpression& call,
      const slang::ast::SubroutineSymbol& callee) -> bool {
    return callee.subroutineKind == slang::ast::SubroutineKind::Function &&
           !callee.flags.has(slang::ast::MethodFlags::DPIImport) &&
           !callee.flags.has(slang::ast::MethodFlags::Constructor) &&
           !IsRuntimeLibraryMethod(callee) && !IsMadeOnAnObject(call, callee);
  }

  void Add(const slang::ast::CallExpression& call) {
    auto lowered = proc_->LowerExpr(call, frame_);
    if (!lowered) {
      failure_ = std::move(lowered.error());
      return;
    }
    calls_.push_back(frame_.Exprs().Add(*std::move(lowered)));
  }

  ProcessLowerer* proc_;
  WalkFrame frame_;
  std::vector<hir::ExprId> calls_;
  std::optional<diag::Diagnostic> failure_;
};

auto CallsListedIn(
    ProcessLowerer& proc, const WalkFrame& frame,
    const slang::ast::Statement& body)
    -> diag::Result<std::vector<hir::ExprId>> {
  ListedCalls collector(proc, frame);
  body.visit(collector);
  return std::move(collector).Take();
}

// The cells a body's own text reads and writes.
auto AccessesOf(
    ProcessLowerer& proc, const NodeAccesses& accesses, const WalkFrame& frame)
    -> diag::Result<hir::Reads> {
  auto cells = proc.Owner().SensitivityEntriesOf(proc, accesses.reads, frame);
  if (!cells) return std::unexpected(std::move(cells.error()));
  auto writes = proc.Owner().SensitivityEntriesOf(proc, accesses.writes, frame);
  if (!writes) return std::unexpected(std::move(writes.error()));
  return hir::Reads{
      .cells = *std::move(cells),
      .writes = *std::move(writes),
      .calls = {},
      .unreportable = std::nullopt};
}

// A body is compiled whether or not a wait reaches it, so what it cannot state
// yet is the wait's to refuse when it asks, never the body's.
auto RefusedWhenAsked(diag::Diagnostic diagnostic) -> hir::Reads {
  return hir::Reads{
      .cells = {},
      .writes = {},
      .calls = {},
      .unreportable = std::move(diagnostic.primary.message)};
}

}  // namespace

template <typename Lowerer>
auto CellsOfWaitedExpression(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<std::vector<hir::SensitivityEntry>> {
  // A read is analyzed in the symbol whose code holds it: the procedure, or
  // outside any the scope itself.
  const slang::ast::Symbol& holder = [&]() -> const slang::ast::Symbol& {
    if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
      return lowerer.ContainingSymbol();
    } else {
      return frame.reader_scope->asSymbol();
    }
  }();
  return lowerer.Owner().SensitivityEntriesOf(
      lowerer, lowerer.Owner().Sensitivity().AnalyzeReads(expr, holder), frame);
}

template auto CellsOfWaitedExpression(
    ProcessLowerer&, WalkFrame, const slang::ast::Expression&)
    -> diag::Result<std::vector<hir::SensitivityEntry>>;
template auto CellsOfWaitedExpression(
    StructuralScopeLowerer&, WalkFrame, const slang::ast::Expression&)
    -> diag::Result<std::vector<hir::SensitivityEntry>>;

auto ReadsOfFunctionBody(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::SubroutineSymbol& function) -> hir::Reads {
  auto reads = AccessesOf(
      proc, proc.Owner().Sensitivity().AnalyzeAccesses(function), frame);
  if (!reads) return RefusedWhenAsked(std::move(reads.error()));
  auto calls = CallsListedIn(proc, frame, function.getBody());
  if (!calls) return RefusedWhenAsked(std::move(calls.error()));
  reads->calls = *std::move(calls);
  return *std::move(reads);
}

auto ReadsOfConstruction(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::ClassType& cls,
    const slang::ast::SubroutineSymbol* constructor,
    const slang::ast::Expression* base_clause) -> hir::Reads {
  hir::Reads reads{
      .cells = {}, .writes = {}, .calls = {}, .unreportable = std::nullopt};
  if (constructor != nullptr) {
    auto body = AccessesOf(
        proc, proc.Owner().Sensitivity().AnalyzeAccesses(*constructor), frame);
    if (!body) return RefusedWhenAsked(std::move(body.error()));
    reads.cells = std::move(body->cells);
  }
  // What the class itself evaluates as an object is built, in the class's own
  // scope: each property initializer, and the arguments an extends specifier
  // hands the base.
  std::vector<const slang::ast::Expression*> evaluated_by_the_class;
  for (const auto& property :
       cls.membersOfType<slang::ast::ClassPropertySymbol>()) {
    if (property.getParentScope() != &cls ||
        property.lifetime == slang::ast::VariableLifetime::Static) {
      continue;
    }
    if (const slang::ast::Expression* init = property.getInitializer();
        init != nullptr) {
      evaluated_by_the_class.push_back(init);
    }
  }
  if (base_clause != nullptr) evaluated_by_the_class.push_back(base_clause);
  for (const slang::ast::Expression* expr : evaluated_by_the_class) {
    auto cells = proc.Owner().SensitivityEntriesOf(
        proc, proc.Owner().Sensitivity().AnalyzeReads(*expr, cls), frame);
    if (!cells) return RefusedWhenAsked(std::move(cells.error()));
    reads.cells.insert(
        reads.cells.end(), std::make_move_iterator(cells->begin()),
        std::make_move_iterator(cells->end()));
  }
  return reads;
}

auto ReadsOfProcedure(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ProceduralBlockSymbol& procedure)
    -> diag::Result<hir::Reads> {
  // The procedure states what its own text reads and writes, and each function
  // it calls states its own once the list is collected, wherever the function
  // is declared.
  auto reads = AccessesOf(
      proc, proc.Owner().Sensitivity().AnalyzeProcedureText(procedure), frame);
  if (!reads) return std::unexpected(std::move(reads.error()));
  auto calls = CallsListedIn(proc, frame, procedure.getBody());
  if (!calls) return std::unexpected(std::move(calls.error()));
  reads->calls = *std::move(calls);
  return reads;
}

}  // namespace lyra::lowering::ast_to_hir
