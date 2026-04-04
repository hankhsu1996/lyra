#include "lyra/lowering/ast_to_hir/task_analysis.hpp"

#include <slang/ast/ASTVisitor.h>

namespace lyra::lowering::ast_to_hir {
namespace {

// Allowlist-oriented validator for non-suspending task bodies.
//
// Design: every statement kind must be explicitly classified as
// immediate-safe to be accepted. Any unclassified construct is rejected.
// This is the correctness gate for lowering task bodies onto the
// immediate callable path.
//
// Specific rejection reasons for known-suspending constructs:
// - TimedStatement: direct timing control (#delay, @event)
// - WaitStatement: wait(expr) statement
// - CallExpression with SubroutineKind::Task: may transitively suspend
//
// For all other unclassified constructs: "not yet proven immediate-safe".
//
// Uses statement-level dispatch (not the CRTP handle() mechanism for
// Statement subclasses) because slang's ASTVisitor::visitDefault requires
// leaf types. We switch on StatementKind and recurse via visit() on the
// concrete as<T>() cast, which satisfies the leaf-type requirement.
class NonSuspendingWalker
    : public slang::ast::ASTVisitor<NonSuspendingWalker, true, true> {
 public:
  explicit NonSuspendingWalker(const SourceMapper* mapper) : mapper_(mapper) {
  }

  void handle(const slang::ast::Statement& stmt) {
    if (found_) return;

    using K = slang::ast::StatementKind;
    switch (stmt.kind) {
      // Known-suspending: reject with specific reason.
      case K::Timed:
        Reject(stmt.sourceRange, "contains timing control");
        return;
      case K::Wait:
        Reject(stmt.sourceRange, "contains wait statement");
        return;

      // Proven immediate-safe: recurse via concrete type.
      case K::Empty:
        return;
      case K::List:
        stmt.as<slang::ast::StatementList>().visitStmts(*this);
        return;
      case K::Block:
        visit(stmt.as<slang::ast::BlockStatement>().body);
        return;
      case K::ExpressionStatement:
        visitDefault(stmt.as<slang::ast::ExpressionStatement>());
        return;
      case K::VariableDeclaration:
        visitDefault(stmt.as<slang::ast::VariableDeclStatement>());
        return;
      case K::Return:
        visitDefault(stmt.as<slang::ast::ReturnStatement>());
        return;
      case K::Continue:
        return;
      case K::Break:
        return;
      case K::Conditional:
        visitDefault(stmt.as<slang::ast::ConditionalStatement>());
        return;
      case K::Case:
        visitDefault(stmt.as<slang::ast::CaseStatement>());
        return;
      case K::ForLoop:
        visitDefault(stmt.as<slang::ast::ForLoopStatement>());
        return;
      case K::RepeatLoop:
        visitDefault(stmt.as<slang::ast::RepeatLoopStatement>());
        return;
      case K::ForeachLoop:
        visitDefault(stmt.as<slang::ast::ForeachLoopStatement>());
        return;
      case K::WhileLoop:
        visitDefault(stmt.as<slang::ast::WhileLoopStatement>());
        return;
      case K::DoWhileLoop:
        visitDefault(stmt.as<slang::ast::DoWhileLoopStatement>());
        return;
      case K::ForeverLoop:
        visitDefault(stmt.as<slang::ast::ForeverLoopStatement>());
        return;
      case K::ImmediateAssertion:
        visitDefault(stmt.as<slang::ast::ImmediateAssertionStatement>());
        return;

      // Everything else: not yet proven immediate-safe.
      default:
        Reject(
            stmt.sourceRange,
            "contains construct not yet proven immediate-safe");
        return;
    }
  }

  void handle(const slang::ast::CallExpression& expr) {
    if (found_) return;
    if (expr.getSubroutineKind() == slang::ast::SubroutineKind::Task) {
      Reject(expr.sourceRange, "calls a task (may transitively suspend)");
      return;
    }
    visitDefault(expr);
  }

  [[nodiscard]] auto Found() const -> bool {
    return found_;
  }
  [[nodiscard]] auto Span() const -> std::optional<SourceSpan> {
    return span_;
  }
  [[nodiscard]] auto Reason() const -> const std::string& {
    return reason_;
  }

 private:
  void Reject(slang::SourceRange range, std::string reason) {
    found_ = true;
    span_ = mapper_->SpanOf(range);
    reason_ = std::move(reason);
  }

  const SourceMapper* mapper_;
  bool found_ = false;
  std::optional<SourceSpan> span_;
  std::string reason_;
};

}  // namespace

auto CheckNonSuspendingTask(
    const slang::ast::SubroutineSymbol& sub, const SourceMapper& mapper)
    -> NonSuspendingTaskCheckResult {
  NonSuspendingWalker walker(&mapper);
  sub.getBody().visit(walker);

  if (walker.Found()) {
    return {
        .ok = false,
        .offending_span = walker.Span(),
        .reason = walker.Reason(),
    };
  }
  return {.ok = true, .offending_span = std::nullopt, .reason = {}};
}

}  // namespace lyra::lowering::ast_to_hir
