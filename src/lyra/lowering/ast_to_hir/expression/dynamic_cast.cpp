#include "lyra/lowering/ast_to_hir/expression/dynamic_cast.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The lvalue behind the destination position. The front end binds it the way it
// binds any `output` actual -- an assignment whose right side is a placeholder
// the callee fills -- and what this construct needs is the left side, because
// the write happens here rather than at a return. It is that shape at every
// call, the front end having bound the position as an lvalue before anything
// reached here, so an actual that is not one is that binding contradicting
// itself rather than a spelling to fall back for.
auto DestinationOf(const slang::ast::Expression* actual)
    -> const slang::ast::Expression* {
  if (actual->kind == slang::ast::ExpressionKind::Assignment) {
    const auto& assignment = actual->as<slang::ast::AssignmentExpression>();
    if (assignment.right().kind == slang::ast::ExpressionKind::EmptyArgument) {
      return &assignment.left();
    }
  }
  throw InternalError(
      "a dynamic cast's destination did not arrive bound as the storage it "
      "names -- please report this as a bug");
}

// What the two declared types settle, and where they settle nothing, which
// run-time check is left. The order is the standard's own: a class destination
// is governed by LRM 8.16, an enumeration by the exception LRM 6.24.2's example
// states, and everything else by plain compatibility (LRM 6.22.3, 6.22.4).
auto ClassifyValidity(
    const slang::ast::Type& destination, const slang::ast::Type& source)
    -> hir::AssignmentValidity {
  const hir::AssignmentValidity none{hir::NoAssignmentAllowed{}};
  const auto allowed = [](hir::RunTimeCheck check) {
    return hir::AssignmentValidity{hir::AssignmentAllowed{.check = check}};
  };

  if (destination.isClass()) {
    // LRM 6.22.5: a class handle is type incompatible with every type that is
    // not one, so nothing but a handle or `null` can reach such a destination.
    if (!source.isClass() && !source.isNull()) {
      return none;
    }
    // LRM 8.16 case 1 and case 3: the destination is the source's own class or
    // one it extends, or the source is the null literal.
    if (destination.isAssignmentCompatible(source)) {
      return allowed(hir::RunTimeCheck::kNone);
    }
    // LRM 8.16 case 2: the destination extends the source's class, or the
    // source's class is an interface the destination's may conform to. Which
    // object is in hand is what is left.
    if (source.isAssignmentCompatible(destination)) {
      return allowed(hir::RunTimeCheck::kObjectIsOfTheDestinationClass);
    }
    return none;
  }
  if (source.isClass() || source.isNull()) {
    return none;
  }
  if (destination.isEnum()) {
    // Another value of the enumeration itself is valid outright; anything that
    // needs an explicit cast to become one has to be a member of it.
    if (destination.isAssignmentCompatible(source)) {
      return allowed(hir::RunTimeCheck::kNone);
    }
    return destination.isCastCompatible(source)
               ? allowed(hir::RunTimeCheck::kValueIsAMemberOfTheEnumeration)
               : none;
  }
  const bool valid = destination.isSingular() && source.isSingular()
                         ? destination.isCastCompatible(source)
                         : destination.isAssignmentCompatible(source);
  return valid ? allowed(hir::RunTimeCheck::kNone) : none;
}

}  // namespace

auto IsDynamicCast(const slang::ast::Expression& expr) -> bool {
  if (expr.kind != slang::ast::ExpressionKind::Call) {
    return false;
  }
  const auto& call = expr.as<slang::ast::CallExpression>();
  if (!call.isSystemCall()) {
    return false;
  }
  const auto& info =
      std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
  return info.subroutine != nullptr &&
         info.subroutine->knownNameId == slang::parsing::KnownSystemName::Cast;
}

template <ExprLowerer Lowerer>
auto LowerDynamicCastExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (call.arguments().size() != 2) {
    throw InternalError(
        "a dynamic cast reached lowering with other than a destination and a "
        "source -- please report this as a bug");
  }
  const slang::ast::Expression& destination_actual =
      *DestinationOf(call.arguments()[0]);
  auto destination_or = lowerer.LowerExpr(destination_actual, frame);
  if (!destination_or) {
    return std::unexpected(std::move(destination_or.error()));
  }
  const hir::ExprId destination = frame.Exprs().Add(*std::move(destination_or));

  auto source_or = lowerer.LowerExpr(*call.arguments()[1], frame);
  if (!source_or) {
    return std::unexpected(std::move(source_or.error()));
  }
  const hir::ExprId source = frame.Exprs().Add(*std::move(source_or));

  auto type_id = lowerer.Owner().InternType(*call.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  return hir::Expr{
      .type = *type_id,
      .data =
          hir::DynamicCastExpr{
              .destination = destination,
              .source = source,
              .validity = ClassifyValidity(
                  destination_actual.type->getCanonicalType(),
                  call.arguments()[1]->type->getCanonicalType()),
              .on_invalid = on_invalid},
      .span = span,
  };
}

template <ExprLowerer Lowerer>
auto LowerExprWithDiscardedAnswer(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& expr,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (IsDynamicCast(expr)) {
    return LowerDynamicCastExpr(
        lowerer, frame, expr.as<slang::ast::CallExpression>(), on_invalid,
        span);
  }
  return lowerer.LowerExpr(expr, frame);
}

template auto LowerDynamicCastExpr(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerDynamicCastExpr(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;
template auto LowerExprWithDiscardedAnswer(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::Expression& expr,
    hir::InvalidAssignmentHandling on_invalid, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

}  // namespace lyra::lowering::ast_to_hir
