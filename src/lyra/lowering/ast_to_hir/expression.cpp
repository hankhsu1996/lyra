#include "lyra/lowering/ast_to_hir/expression.hpp"

#include <format>
#include <optional>
#include <span>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

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

      // Handle type parameters - explicit unsupported error
      if (named.symbol.kind == slang::ast::SymbolKind::TypeParameter) {
        ctx->sink->Error(
            span,
            std::format(
                "type parameters not supported ('{}')", named.symbol.name));
        return hir::kInvalidExpressionId;
      }

      // Handle value parameters - inline as constants
      if (named.symbol.kind == slang::ast::SymbolKind::Parameter) {
        const auto& param = named.symbol.as<slang::ast::ParameterSymbol>();
        // Pass sourceRange for better slang diagnostics on evaluation errors
        const slang::ConstantValue& cv = param.getValue(expr.sourceRange);

        // Check for bad/invalid/unbounded values
        if (cv.bad()) {
          ctx->sink->Error(
              span, std::format(
                        "parameter '{}' has invalid value", named.symbol.name));
          return hir::kInvalidExpressionId;
        }
        if (cv.isUnbounded()) {
          ctx->sink->Error(
              span,
              std::format(
                  "unbounded parameter '{}' not supported", named.symbol.name));
          return hir::kInvalidExpressionId;
        }

        if (cv.isInteger()) {
          // Verify the expression type is actually integral (not a packed
          // struct that happens to have integer representation)
          if (expr.type == nullptr) {
            return hir::kInvalidExpressionId;
          }
          if (!expr.type->isIntegral()) {
            ctx->sink->Error(
                span, std::format(
                          "unsupported parameter type for '{}' (only integral "
                          "parameters supported)",
                          named.symbol.name));
            return hir::kInvalidExpressionId;
          }
          TypeId type = LowerType(*expr.type, span, ctx);
          if (!type) {
            return hir::kInvalidExpressionId;
          }
          ConstId constant = LowerIntegralConstant(cv.integer(), type, ctx);
          return ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kConstant,
                  .type = type,
                  .span = span,
                  .data = hir::ConstantExpressionData{.constant = constant}});
        }

        // Unsupported parameter value type - clear error
        ctx->sink->Error(
            span, std::format(
                      "unsupported parameter type for '{}' (only integral "
                      "parameters supported)",
                      named.symbol.name));
        return hir::kInvalidExpressionId;
      }

      // Existing code for variables/other symbols
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

    case ExpressionKind::NewArray: {
      const auto& new_arr = expr.as<slang::ast::NewArrayExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      hir::ExpressionId size_expr =
          LowerExpression(new_arr.sizeExpr(), registrar, ctx);
      if (!size_expr) {
        return hir::kInvalidExpressionId;
      }

      std::optional<hir::ExpressionId> init_expr;
      if (new_arr.initExpr() != nullptr) {
        hir::ExpressionId init_id =
            LowerExpression(*new_arr.initExpr(), registrar, ctx);
        if (!init_id) {
          return hir::kInvalidExpressionId;
        }
        init_expr = init_id;
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
              .kind = hir::ExpressionKind::kNewArray,
              .type = type,
              .span = span,
              .data = hir::NewArrayExpressionData{
                  .size_expr = size_expr, .init_expr = init_expr}});
    }

    case ExpressionKind::Call: {
      const auto& call = expr.as<slang::ast::CallExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      // Check if this is a builtin method call on a dynamic array.
      // IMPORTANT: Must check BEFORE isSystemCall() - slang's isSystemCall()
      // returns true for built-in array methods like size().
      // Also must verify it's NOT a user function (SubroutineSymbol) to avoid
      // confusing user-defined size(arr) with the builtin arr.size().
      // NOTE: System calls have names starting with '$' (e.g., $size), so the
      // exact name match "size"/"delete" won't capture future system calls.
      const auto* user_sub =
          std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
      std::string_view name = call.getSubroutineName();
      if (user_sub == nullptr && !call.arguments().empty()) {
        const auto* first_arg = call.arguments()[0];
        if (first_arg->type != nullptr &&
            first_arg->type->kind == slang::ast::SymbolKind::DynamicArrayType) {
          if (name == "size") {
            hir::ExpressionId receiver =
                LowerExpression(*first_arg, registrar, ctx);
            if (!receiver) {
              return hir::kInvalidExpressionId;
            }

            // size() returns int (32-bit signed)
            TypeId int_type = ctx->type_arena->Intern(
                TypeKind::kIntegral, IntegralInfo{
                                         .bit_width = 32,
                                         .is_signed = true,
                                         .is_four_state = false});

            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kBuiltinMethodCall,
                    .type = int_type,
                    .span = span,
                    .data = hir::BuiltinMethodCallExpressionData{
                        .receiver = receiver,
                        .method = hir::BuiltinMethod::kSize}});
          }
          if (name == "delete") {
            ctx->sink->Error(
                span, "delete() cannot be used in expression context");
            return hir::kInvalidExpressionId;
          }
        }
      }

      // Handle system calls ($display, etc.)
      if (call.isSystemCall()) {
        return LowerSystemCall(call, registrar, ctx);
      }

      // User function call (user_sub was computed above for builtin detection)
      if (user_sub == nullptr) {
        ctx->sink->Error(span, "indirect function calls not supported");
        return hir::kInvalidExpressionId;
      }

      // Reject task calls from functions
      if ((*user_sub)->subroutineKind == slang::ast::SubroutineKind::Task) {
        ctx->sink->Error(span, "task calls not supported");
        return hir::kInvalidExpressionId;
      }

      SymbolId callee = registrar.Lookup(**user_sub);
      if (!callee) {
        ctx->sink->Error(
            span, std::format("undefined function '{}'", (*user_sub)->name));
        return hir::kInvalidExpressionId;
      }

      // Lower arguments
      std::vector<hir::ExpressionId> args;
      args.reserve(call.arguments().size());
      for (const auto* arg_expr : call.arguments()) {
        hir::ExpressionId arg = LowerExpression(*arg_expr, registrar, ctx);
        if (!arg) {
          return hir::kInvalidExpressionId;
        }
        args.push_back(arg);
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
              .kind = hir::ExpressionKind::kCall,
              .type = type,
              .span = span,
              .data = hir::CallExpressionData{
                  .callee = callee, .arguments = std::move(args)}});
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
      // Note: 2-state → 4-state is allowed (lossless, no X/Z bits introduced)

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

    case ExpressionKind::ReplicatedAssignmentPattern:
    case ExpressionKind::SimpleAssignmentPattern:
    case ExpressionKind::StructuredAssignmentPattern: {
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      // Validate type exists
      if (expr.type == nullptr) {
        ctx->sink->Error(span, "assignment pattern has no resolved type");
        return hir::kInvalidExpressionId;
      }
      const slang::ast::Type& ct = expr.type->getCanonicalType();

      // Gate: only unpacked structs
      if (!ct.isUnpackedStruct()) {
        if (ct.isArray()) {
          ctx->sink->Error(span, "array literals not yet supported");
        } else if (ct.isStruct()) {
          ctx->sink->Error(span, "only unpacked struct literals supported");
        } else {
          ctx->sink->Error(span, "assignment pattern requires struct type");
        }
        return hir::kInvalidExpressionId;
      }

      // Gate: reject replication patterns
      if (expr.kind == ExpressionKind::ReplicatedAssignmentPattern) {
        ctx->sink->Error(span, "replication not supported in struct literals");
        return hir::kInvalidExpressionId;
      }

      // Gate: reject default/type setters (check before size to give clearer
      // error)
      if (expr.kind == ExpressionKind::StructuredAssignmentPattern) {
        const auto& structured =
            expr.as<slang::ast::StructuredAssignmentPatternExpression>();
        if (structured.defaultSetter != nullptr ||
            !structured.typeSetters.empty()) {
          ctx->sink->Error(
              span, "default/type setters not supported in struct literals");
          return hir::kInvalidExpressionId;
        }
      }

      // Get elements - both pattern types use elements() with declaration order
      auto get_elements = [&]() {
        if (expr.kind == ExpressionKind::SimpleAssignmentPattern) {
          return expr.as<slang::ast::SimpleAssignmentPatternExpression>()
              .elements();
        }
        return expr.as<slang::ast::StructuredAssignmentPatternExpression>()
            .elements();
      };
      auto elements = get_elements();

      // Gate: full literal only (all fields must be specified)
      const auto& struct_type = ct.as<slang::ast::UnpackedStructType>();
      if (elements.size() != struct_type.fields.size()) {
        ctx->sink->Error(
            span, "all struct fields must be explicitly specified");
        return hir::kInvalidExpressionId;
      }

      // Lower field values
      std::vector<hir::ExpressionId> field_values;
      field_values.reserve(elements.size());
      for (const auto* elem : elements) {
        hir::ExpressionId field_id = LowerExpression(*elem, registrar, ctx);
        if (!field_id) {
          return hir::kInvalidExpressionId;
        }
        field_values.push_back(field_id);
      }

      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kStructLiteral,
              .type = type,
              .span = span,
              .data = hir::StructLiteralExpressionData{
                  .field_values = std::move(field_values)}});
    }

    default:
      ctx->sink->Error(
          ctx->SpanOf(expr.sourceRange),
          std::format("unsupported expression kind '{}'", toString(expr.kind)));
      return hir::kInvalidExpressionId;
  }
}

}  // namespace lyra::lowering::ast_to_hir
