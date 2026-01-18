#include "lyra/lowering/ast_to_hir/expression.hpp"

#include <format>
#include <optional>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>

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

    case ExpressionKind::RealLiteral: {
      const auto& literal = expr.as<slang::ast::RealLiteral>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }
      ConstId constant = ctx->constant_arena->Intern(
          type, RealConstant{.value = literal.getValue()});
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

    case ExpressionKind::Conversion: {
      const auto& conv = expr.as<slang::ast::ConversionExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      using CK = slang::ast::ConversionKind;
      switch (conv.conversionKind) {
        case CK::Implicit:
        case CK::Propagated:
          break;
        case CK::Explicit:
          ctx->sink->Error(span, "explicit casts not yet supported");
          return hir::kInvalidExpressionId;
        case CK::StreamingConcat:
          ctx->sink->Error(span, "streaming concatenation not supported");
          return hir::kInvalidExpressionId;
        case CK::BitstreamCast:
          ctx->sink->Error(span, "bitstream casts not supported");
          return hir::kInvalidExpressionId;
      }

      // Handle StringLiteral → string conversion specially.
      // Slang represents string literals as bit[N:0] internally, then wraps
      // with Conversion to string. We extract the byte content directly.
      const slang::ast::Type& tgt_type = expr.type->getCanonicalType();
      if (conv.operand().kind == ExpressionKind::StringLiteral &&
          tgt_type.isString()) {
        const auto& literal = conv.operand().as<slang::ast::StringLiteral>();
        TypeId type = LowerType(tgt_type, span, ctx);
        if (!type) {
          return hir::kInvalidExpressionId;
        }
        // getValue() returns std::string_view with explicit length,
        // preserving embedded NUL bytes.
        ConstId constant = ctx->constant_arena->Intern(
            type, StringConstant{.value = std::string(literal.getValue())});
        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kConstant,
                .type = type,
                .span = span,
                .data = hir::ConstantExpressionData{.constant = constant}});
      }

      // Validate source and target types for integral conversions
      const slang::ast::Type& src_type =
          conv.operand().type->getCanonicalType();

      if (!src_type.isIntegral()) {
        ctx->sink->Error(
            span, "conversion from non-integral type not supported");
        return hir::kInvalidExpressionId;
      }
      if (!tgt_type.isIntegral()) {
        ctx->sink->Error(span, "conversion to non-integral type not supported");
        return hir::kInvalidExpressionId;
      }
      if (src_type.isFourState()) {
        ctx->sink->Error(
            span, "conversion from 4-state type not yet supported");
        return hir::kInvalidExpressionId;
      }
      if (tgt_type.isFourState()) {
        ctx->sink->Error(span, "conversion to 4-state type not yet supported");
        return hir::kInvalidExpressionId;
      }

      hir::ExpressionId operand =
          LowerExpression(conv.operand(), registrar, ctx);
      if (!operand) {
        return hir::kInvalidExpressionId;
      }

      TypeId type = LowerType(tgt_type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kCast,
              .type = type,
              .span = span,
              .data = hir::CastExpressionData{.operand = operand}});
    }

    case ExpressionKind::ConditionalOp: {
      const auto& cond = expr.as<slang::ast::ConditionalExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      // Reject multiple conditions (SV pattern matching not yet supported)
      if (cond.conditions.size() != 1) {
        ctx->sink->Error(span, "chained ternary conditions not yet supported");
        return hir::kInvalidExpressionId;
      }

      // Reject pattern matching (SV 2023 feature)
      const auto& condition = cond.conditions[0];
      if (condition.pattern != nullptr) {
        ctx->sink->Error(span, "pattern matching in ternary not yet supported");
        return hir::kInvalidExpressionId;
      }

      // Lower all three sub-expressions
      hir::ExpressionId cond_expr =
          LowerExpression(*condition.expr, registrar, ctx);
      if (!cond_expr) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId then_expr =
          LowerExpression(cond.left(), registrar, ctx);
      if (!then_expr) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId else_expr =
          LowerExpression(cond.right(), registrar, ctx);
      if (!else_expr) {
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
              .kind = hir::ExpressionKind::kConditional,
              .type = type,
              .span = span,
              .data = hir::ConditionalExpressionData{
                  .condition = cond_expr,
                  .then_expr = then_expr,
                  .else_expr = else_expr}});
    }

    case ExpressionKind::Assignment: {
      const auto& assign = expr.as<slang::ast::AssignmentExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      hir::ExpressionId target = LowerExpression(assign.left(), registrar, ctx);
      if (!target) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId value = LowerExpression(assign.right(), registrar, ctx);
      if (!value) {
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
              .kind = hir::ExpressionKind::kAssignment,
              .type = type,
              .span = span,
              .data =
                  hir::AssignmentExpressionData{
                      .target = target, .value = value},
          });
    }

    case ExpressionKind::ElementSelect: {
      const auto& select = expr.as<slang::ast::ElementSelectExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      // Gate: only handle unpacked arrays. Packed bit-select is not supported.
      const slang::ast::Type& value_type =
          select.value().type->getCanonicalType();
      if (!value_type.isUnpackedArray()) {
        ctx->sink->Error(
            span, std::format(
                      "packed array bit-select not yet supported (type: {})",
                      value_type.toString()));
        return hir::kInvalidExpressionId;
      }

      hir::ExpressionId base = LowerExpression(select.value(), registrar, ctx);
      if (!base) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId index =
          LowerExpression(select.selector(), registrar, ctx);
      if (!index) {
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
              .kind = hir::ExpressionKind::kElementAccess,
              .type = type,
              .span = span,
              .data =
                  hir::ElementAccessExpressionData{
                      .base = base, .index = index},
          });
    }

    case ExpressionKind::MemberAccess: {
      const auto& access = expr.as<slang::ast::MemberAccessExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      const slang::ast::Type& value_type =
          access.value().type->getCanonicalType();
      if (!value_type.isUnpackedStruct()) {
        if (value_type.isStruct()) {
          ctx->sink->Error(span, "only unpacked structs supported");
        } else {
          ctx->sink->Error(
              span, "member access only supported on struct types");
        }
        return hir::kInvalidExpressionId;
      }

      const auto* field = access.member.as_if<slang::ast::FieldSymbol>();
      if (field == nullptr) {
        ctx->sink->Error(span, "only struct field access is supported");
        return hir::kInvalidExpressionId;
      }

      hir::ExpressionId base = LowerExpression(access.value(), registrar, ctx);
      if (!base) {
        return hir::kInvalidExpressionId;
      }

      int field_index = static_cast<int>(field->fieldIndex);

      if (expr.type == nullptr) {
        ctx->sink->Error(span, "internal: member access has no resolved type");
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kMemberAccess,
              .type = type,
              .span = span,
              .data =
                  hir::MemberAccessExpressionData{
                      .base = base, .field_index = field_index},
          });
    }

    default:
      ctx->sink->Error(
          ctx->SpanOf(expr.sourceRange),
          std::format("unsupported expression kind '{}'", toString(expr.kind)));
      return hir::kInvalidExpressionId;
  }
}

}  // namespace lyra::lowering::ast_to_hir
