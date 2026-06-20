#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/method.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// LRM 6.16.2: writing `s[i]` mutates one byte through putc, the symmetric
// counterpart of the getc index read, because a string exposes no element
// lvalue (include/lyra/value/concepts.hpp). A compound `s[i] op= rhs` reads the
// current byte with getc, applies the operator, and writes the result back.
auto LowerStringElementWrite(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as,
    const slang::ast::ElementSelectExpression& sel, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (as.isNonBlocking()) {
    // LRM 10.4.2 permits a nonblocking string-element write (a string is not a
    // dynamically sized array), but scheduling a deferred putc into the NBA
    // region is not yet modeled.
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "nonblocking assignment to a string element is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  auto& module = proc.Module();
  auto& body = *frame.current_procedural_body;

  auto base_or = proc.LowerExpr(sel.value(), frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const hir::ExprId base_id = body.AddExpr(*std::move(base_or));
  auto idx_or = proc.LowerExpr(sel.selector(), frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const hir::ExprId idx_id = body.AddExpr(*std::move(idx_or));

  auto elem_type = module.InternType(*sel.type, span);
  if (!elem_type) return std::unexpected(std::move(elem_type.error()));

  hir::ExprId value_id{};
  if (as.op.has_value()) {
    const hir::ExprId cur_id = body.AddExpr(
        hir::Expr{
            .type = *elem_type,
            .data =
                hir::CallExpr{
                    .callee =
                        hir::BuiltinMethodRef{
                            .method = hir::StringMethodKind::kGetc},
                    .arguments = {base_id, idx_id}},
            .span = span});
    auto rhs_or = proc.LowerExpr(BareCompoundUserRhs(as.right()), frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    hir::Expr rhs_expr = *std::move(rhs_or);
    if (rhs_expr.type.value != elem_type->value) {
      const hir::ExprId inner_id = body.AddExpr(std::move(rhs_expr));
      rhs_expr = hir::Expr{
          .type = *elem_type,
          .data =
              hir::ConversionExpr{
                  .operand = inner_id, .kind = hir::ConversionKind::kImplicit},
          .span = span};
    }
    const hir::ExprId rhs_id = body.AddExpr(std::move(rhs_expr));
    value_id = body.AddExpr(
        hir::Expr{
            .type = *elem_type,
            .data =
                hir::BinaryExpr{
                    .op = LowerBinaryOp(*as.op), .lhs = cur_id, .rhs = rhs_id},
            .span = span});
  } else {
    auto rhs_or = proc.LowerExpr(as.right(), frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    value_id = body.AddExpr(*std::move(rhs_or));
  }

  return hir::Expr{
      .type = module.Unit().builtins.void_type,
      .data =
          hir::CallExpr{
              .callee =
                  hir::BuiltinMethodRef{.method = hir::StringMethodKind::kPutc},
              .arguments = {base_id, idx_id, value_id}},
      .span = span,
  };
}

}  // namespace

auto LowerAssignmentExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  auto validate = ValidateAssignableImpl(module, true, as.left());
  if (!validate) return std::unexpected(std::move(validate.error()));

  if (as.left().kind == slang::ast::ExpressionKind::ElementSelect) {
    const auto& sel = as.left().as<slang::ast::ElementSelectExpression>();
    if (sel.value().type->getCanonicalType().isString()) {
      return LowerStringElementWrite(proc, frame, as, sel, span);
    }
  }

  auto lhs_or = proc.LowerExpr(as.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id =
      frame.current_procedural_body->AddExpr(*std::move(lhs_or));

  auto type_id = module.InternType(*as.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  const auto kind = as.isNonBlocking() ? hir::AssignKind::kNonBlocking
                                       : hir::AssignKind::kBlocking;

  if (!as.op.has_value()) {
    auto rhs_or = proc.LowerExpr(as.right(), frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const hir::ExprId rhs_id =
        frame.current_procedural_body->AddExpr(*std::move(rhs_or));
    return hir::Expr{
        .type = *type_id,
        .data =
            hir::AssignExpr{
                .kind = kind,
                .lhs = lhs_id,
                .compound_op = std::nullopt,
                .rhs = rhs_id},
        .span = span,
    };
  }

  if (as.isNonBlocking()) {
    throw InternalError(
        "LowerAssignmentExprProc: compound assignment with non-blocking "
        "operator is not a legal SV form (LRM A.6.2 grammar)");
  }

  const auto& bare_user_rhs = BareCompoundUserRhs(as.right());
  auto rhs_or = proc.LowerExpr(bare_user_rhs, frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  hir::Expr rhs_expr = *std::move(rhs_or);
  if (rhs_expr.type.value != type_id->value) {
    const hir::ExprId inner_id =
        frame.current_procedural_body->AddExpr(std::move(rhs_expr));
    rhs_expr = hir::Expr{
        .type = *type_id,
        .data =
            hir::ConversionExpr{
                .operand = inner_id, .kind = hir::ConversionKind::kImplicit},
        .span = span,
    };
  }
  const hir::ExprId rhs_id =
      frame.current_procedural_body->AddExpr(std::move(rhs_expr));
  return hir::Expr{
      .type = *type_id,
      .data =
          hir::AssignExpr{
              .kind = kind,
              .lhs = lhs_id,
              .compound_op = LowerBinaryOp(*as.op),
              .rhs = rhs_id},
      .span = span,
  };
}

auto LowerIncDecExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  auto validate = ValidateAssignableImpl(module, true, un.operand());
  if (!validate) return std::unexpected(std::move(validate.error()));

  auto target_or = proc.LowerExpr(un.operand(), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const hir::ExprId target_id =
      frame.current_procedural_body->AddExpr(*std::move(target_or));

  auto type_id = module.InternType(*un.type, span);
  if (!type_id) return std::unexpected(std::move(type_id.error()));

  return hir::Expr{
      .type = *type_id,
      .data =
          hir::IncDecExpr{.op = LowerSlangIncDecOp(un.op), .target = target_id},
      .span = span,
  };
}

auto ValidateAssignableImpl(
    ModuleLowerer& module, bool procedural_context,
    const slang::ast::Expression& expr) -> diag::Result<void> {
  using EK = slang::ast::ExpressionKind;
  const auto& mapper = module.SourceMapper();
  const auto span = mapper.SpanOf(expr.sourceRange);
  auto reject = [&](std::string_view why) -> diag::Result<void> {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget, std::string{why},
        diag::UnsupportedCategory::kFeature);
  };

  switch (expr.kind) {
    case EK::NamedValue: {
      const auto& nv = expr.as<slang::ast::NamedValueExpression>();
      const auto& sym = nv.symbol;
      if (sym.kind != slang::ast::SymbolKind::Variable &&
          sym.kind != slang::ast::SymbolKind::FormalArgument) {
        return reject("assignment target must be a variable reference");
      }
      const auto& var = sym.as<slang::ast::VariableSymbol>();
      if (module.LookupLoopVarBinding(var).has_value()) {
        return reject(
            "generate loop variable is not a legal assignment target");
      }
      if (!procedural_context) {
        // LRM 10.3: continuous-assign target must resolve to a structural var.
        if (!module.LookupStructuralVarBinding(var).has_value()) {
          return reject(
              "continuous-assignment target must be a structural variable");
        }
      }
      return {};
    }
    case EK::ElementSelect:
      return ValidateAssignableImpl(
          module, procedural_context,
          expr.as<slang::ast::ElementSelectExpression>().value());
    case EK::RangeSelect:
      return ValidateAssignableImpl(
          module, procedural_context,
          expr.as<slang::ast::RangeSelectExpression>().value());
    case EK::MemberAccess: {
      const auto& ma = expr.as<slang::ast::MemberAccessExpression>();
      if (ma.member.kind != slang::ast::SymbolKind::Field) {
        return reject("member access target is not a struct field");
      }
      return ValidateAssignableImpl(module, procedural_context, ma.value());
    }
    case EK::HierarchicalValue: {
      const auto& hv = expr.as<slang::ast::HierarchicalValueExpression>();
      if (hv.symbol.kind != slang::ast::SymbolKind::Variable) {
        return reject("assignment target must be a variable reference");
      }
      if (!procedural_context) {
        return reject(
            "continuous-assignment target must be a structural variable");
      }
      return {};
    }
    case EK::Concatenation: {
      const auto& cc = expr.as<slang::ast::ConcatenationExpression>();
      for (const auto* op : cc.operands()) {
        if (op->kind == EK::Replication) {
          const auto op_span = mapper.SpanOf(op->sourceRange);
          return diag::Unsupported(
              op_span, diag::DiagCode::kUnsupportedAssignmentTarget,
              "replication is not allowed inside a destructuring "
              "assignment target (LRM 11.4.12.1)",
              diag::UnsupportedCategory::kFeature);
        }
        auto sub = ValidateAssignableImpl(module, procedural_context, *op);
        if (!sub) return sub;
      }
      return {};
    }
    default:
      return reject("assignment target is not supported yet");
  }
}

}  // namespace lyra::lowering::ast_to_hir
