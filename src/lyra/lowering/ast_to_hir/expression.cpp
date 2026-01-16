#include "lyra/lowering/ast_to_hir/expression.hpp"

#include <format>
#include <optional>

#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ConvertUnaryOp(slang::ast::UnaryOperator op)
    -> std::optional<hir::UnaryOp> {
  using SlangOp = slang::ast::UnaryOperator;
  using HirOp = hir::UnaryOp;

  switch (op) {
    case SlangOp::Plus:
      return HirOp::kPlus;
    case SlangOp::Minus:
      return HirOp::kMinus;
    case SlangOp::BitwiseNot:
      return HirOp::kBitwiseNot;
    case SlangOp::BitwiseAnd:
      return HirOp::kReductionAnd;
    case SlangOp::BitwiseOr:
      return HirOp::kReductionOr;
    case SlangOp::BitwiseXor:
      return HirOp::kReductionXor;
    case SlangOp::BitwiseNand:
      return HirOp::kReductionNand;
    case SlangOp::BitwiseNor:
      return HirOp::kReductionNor;
    case SlangOp::BitwiseXnor:
      return HirOp::kReductionXnor;
    case SlangOp::LogicalNot:
      return HirOp::kLogicalNot;
    case SlangOp::Preincrement:
      return HirOp::kPreincrement;
    case SlangOp::Predecrement:
      return HirOp::kPredecrement;
    case SlangOp::Postincrement:
      return HirOp::kPostincrement;
    case SlangOp::Postdecrement:
      return HirOp::kPostdecrement;
  }
  return std::nullopt;
}

auto ConvertBinaryOp(slang::ast::BinaryOperator op)
    -> std::optional<hir::BinaryOp> {
  using SlangOp = slang::ast::BinaryOperator;
  using HirOp = hir::BinaryOp;

  switch (op) {
    case SlangOp::Add:
      return HirOp::kAdd;
    case SlangOp::Subtract:
      return HirOp::kSubtract;
    case SlangOp::Multiply:
      return HirOp::kMultiply;
    case SlangOp::Divide:
      return HirOp::kDivide;
    case SlangOp::Mod:
      return HirOp::kMod;
    case SlangOp::Power:
      return HirOp::kPower;
    case SlangOp::BinaryAnd:
      return HirOp::kBitwiseAnd;
    case SlangOp::BinaryOr:
      return HirOp::kBitwiseOr;
    case SlangOp::BinaryXor:
      return HirOp::kBitwiseXor;
    case SlangOp::BinaryXnor:
      return HirOp::kBitwiseXnor;
    case SlangOp::LogicalAnd:
      return HirOp::kLogicalAnd;
    case SlangOp::LogicalOr:
      return HirOp::kLogicalOr;
    case SlangOp::LogicalImplication:
      return HirOp::kLogicalImplication;
    case SlangOp::LogicalEquivalence:
      return HirOp::kLogicalEquivalence;
    case SlangOp::Equality:
      return HirOp::kEqual;
    case SlangOp::Inequality:
      return HirOp::kNotEqual;
    case SlangOp::CaseEquality:
      return HirOp::kCaseEqual;
    case SlangOp::CaseInequality:
      return HirOp::kCaseNotEqual;
    case SlangOp::WildcardEquality:
      return HirOp::kWildcardEqual;
    case SlangOp::WildcardInequality:
      return HirOp::kWildcardNotEqual;
    case SlangOp::LessThan:
      return HirOp::kLessThan;
    case SlangOp::LessThanEqual:
      return HirOp::kLessThanEqual;
    case SlangOp::GreaterThan:
      return HirOp::kGreaterThan;
    case SlangOp::GreaterThanEqual:
      return HirOp::kGreaterThanEqual;
    case SlangOp::LogicalShiftLeft:
      return HirOp::kLogicalShiftLeft;
    case SlangOp::LogicalShiftRight:
      return HirOp::kLogicalShiftRight;
    case SlangOp::ArithmeticShiftLeft:
      return HirOp::kArithmeticShiftLeft;
    case SlangOp::ArithmeticShiftRight:
      return HirOp::kArithmeticShiftRight;
  }
  return std::nullopt;
}

}  // namespace

auto LowerExpression(
    const slang::ast::Expression& expr, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  using slang::ast::ExpressionKind;

  switch (expr.kind) {
    case ExpressionKind::IntegerLiteral: {
      const auto& literal = expr.as<slang::ast::IntegerLiteral>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }
      ConstId constant = LowerIntegralConstant(literal.getValue(), type, ctx);
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConstant,
              .type = type,
              .span = span,
              .data = hir::ConstantExpressionData{.constant = constant}});
    }

    case ExpressionKind::StringLiteral: {
      const auto& literal = expr.as<slang::ast::StringLiteral>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }
      ConstId constant = ctx->constant_arena->Intern(
          type, StringConstant{.value = std::string(literal.getValue())});
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConstant,
              .type = type,
              .span = span,
              .data = hir::ConstantExpressionData{.constant = constant}});
    }

    case ExpressionKind::NamedValue: {
      const auto& named = expr.as<slang::ast::NamedValueExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      SymbolId sym = registrar.Lookup(named.symbol);
      if (!sym) {
        ctx->sink->Error(
            span, std::format("undefined symbol '{}'", named.symbol.name));
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(named.symbol.getType(), span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kNameRef,
              .type = type,
              .span = span,
              .data = hir::NameRefExpressionData{.symbol = sym}});
    }

    case ExpressionKind::UnaryOp: {
      const auto& unary = expr.as<slang::ast::UnaryExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      auto hir_op = ConvertUnaryOp(unary.op);
      if (!hir_op) {
        ctx->sink->Error(
            span,
            std::format("unsupported unary operator '{}'", toString(unary.op)));
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId operand =
          LowerExpression(unary.operand(), registrar, ctx);
      if (!operand) {
        return hir::kInvalidExpressionId;
      }
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kUnaryOp,
              .type = type,
              .span = span,
              .data =
                  hir::UnaryExpressionData{.op = *hir_op, .operand = operand}});
    }

    case ExpressionKind::BinaryOp: {
      const auto& binary = expr.as<slang::ast::BinaryExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      auto hir_op = ConvertBinaryOp(binary.op);
      if (!hir_op) {
        ctx->sink->Error(
            span, std::format(
                      "unsupported binary operator '{}'", toString(binary.op)));
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId lhs = LowerExpression(binary.left(), registrar, ctx);
      if (!lhs) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId rhs = LowerExpression(binary.right(), registrar, ctx);
      if (!rhs) {
        return hir::kInvalidExpressionId;
      }
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kBinaryOp,
              .type = type,
              .span = span,
              .data = hir::BinaryExpressionData{
                  .op = *hir_op, .lhs = lhs, .rhs = rhs}});
    }

    case ExpressionKind::Call: {
      const auto& call = expr.as<slang::ast::CallExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      if (!call.isSystemCall()) {
        ctx->sink->Error(span, "only system calls are supported");
        return hir::kInvalidExpressionId;
      }

      return LowerSystemCall(call, registrar, ctx);
    }

    default:
      ctx->sink->Error(
          ctx->SpanOf(expr.sourceRange),
          std::format("unsupported expression kind '{}'", toString(expr.kind)));
      return hir::kInvalidExpressionId;
  }
}

}  // namespace lyra::lowering::ast_to_hir
