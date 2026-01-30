#include "lyra/lowering/ast_to_hir/expression.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/operator.hpp"
#include "lyra/lowering/ast_to_hir/assign_target.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/expression_access.hpp"
#include "lyra/lowering/ast_to_hir/expression_aggregate.hpp"
#include "lyra/lowering/ast_to_hir/expression_call.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
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

auto LowerConstantValueExpression(
    const slang::ConstantValue& cv, const slang::ast::Type& type,
    SourceSpan span, Context* ctx) -> hir::ExpressionId {
  if (cv.isInteger()) {
    if (!type.isIntegral()) {
      ctx->sink->Error(span, "unsupported non-integral constant value");
      return hir::kInvalidExpressionId;
    }
    TypeId type_id = LowerType(type, span, ctx);
    if (!type_id) {
      return hir::kInvalidExpressionId;
    }
    ConstId constant = LowerIntegralConstant(cv.integer(), type_id, ctx);
    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kConstant,
            .type = type_id,
            .span = span,
            .data = hir::ConstantExpressionData{.constant = constant}});
  }

  if (cv.isString()) {
    TypeId type_id = LowerType(type, span, ctx);
    if (!type_id) {
      return hir::kInvalidExpressionId;
    }
    ConstId constant = ctx->constant_arena->Intern(
        type_id, StringConstant{.value = std::string(cv.str())});
    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kConstant,
            .type = type_id,
            .span = span,
            .data = hir::ConstantExpressionData{.constant = constant}});
  }

  if (cv.isReal() || cv.isShortReal()) {
    TypeId type_id = LowerType(type, span, ctx);
    if (!type_id) {
      return hir::kInvalidExpressionId;
    }
    double value = cv.isReal() ? static_cast<double>(cv.real())
                               : static_cast<double>(cv.shortReal());
    ConstId constant =
        ctx->constant_arena->Intern(type_id, RealConstant{.value = value});
    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kConstant,
            .type = type_id,
            .span = span,
            .data = hir::ConstantExpressionData{.constant = constant}});
  }

  if (cv.isUnpacked()) {
    const auto& canonical = type.getCanonicalType();
    auto elements = cv.elements();

    if (canonical.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      const auto& arr = canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      auto expected_size = arr.range.width();
      if (elements.size() != expected_size) {
        ctx->sink->Error(span, "array constant size mismatch");
        return hir::kInvalidExpressionId;
      }

      std::vector<hir::ExpressionId> element_ids;
      element_ids.reserve(elements.size());
      for (const auto& elem : elements) {
        hir::ExpressionId elem_id =
            LowerConstantValueExpression(elem, arr.elementType, span, ctx);
        if (!elem_id) {
          return hir::kInvalidExpressionId;
        }
        element_ids.push_back(elem_id);
      }

      TypeId type_id = LowerType(type, span, ctx);
      if (!type_id) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kArrayLiteral,
              .type = type_id,
              .span = span,
              .data = hir::ArrayLiteralExpressionData{
                  .elements = std::move(element_ids)}});
    }

    if (canonical.kind == slang::ast::SymbolKind::DynamicArrayType ||
        canonical.kind == slang::ast::SymbolKind::QueueType) {
      const slang::ast::Type* element_type =
          canonical.kind == slang::ast::SymbolKind::DynamicArrayType
              ? &canonical.as<slang::ast::DynamicArrayType>().elementType
              : &canonical.as<slang::ast::QueueType>().elementType;

      std::vector<hir::ExpressionId> element_ids;
      element_ids.reserve(elements.size());
      for (const auto& elem : elements) {
        hir::ExpressionId elem_id =
            LowerConstantValueExpression(elem, *element_type, span, ctx);
        if (!elem_id) {
          return hir::kInvalidExpressionId;
        }
        element_ids.push_back(elem_id);
      }

      TypeId type_id = LowerType(type, span, ctx);
      if (!type_id) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kArrayLiteral,
              .type = type_id,
              .span = span,
              .data = hir::ArrayLiteralExpressionData{
                  .elements = std::move(element_ids)}});
    }

    if (canonical.kind == slang::ast::SymbolKind::UnpackedStructType) {
      const auto& struct_type = canonical.as<slang::ast::UnpackedStructType>();
      auto fields = struct_type.fields;
      if (elements.size() != fields.size()) {
        ctx->sink->Error(span, "struct constant size mismatch");
        return hir::kInvalidExpressionId;
      }

      std::vector<hir::ExpressionId> field_values;
      field_values.reserve(elements.size());
      for (size_t i = 0; i < elements.size(); ++i) {
        hir::ExpressionId field_id = LowerConstantValueExpression(
            elements[i], fields[i]->getType(), span, ctx);
        if (!field_id) {
          return hir::kInvalidExpressionId;
        }
        field_values.push_back(field_id);
      }

      TypeId type_id = LowerType(type, span, ctx);
      if (!type_id) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kStructLiteral,
              .type = type_id,
              .span = span,
              .data = hir::StructLiteralExpressionData{
                  .field_values = std::move(field_values)}});
    }

    if (canonical.kind == slang::ast::SymbolKind::PackedStructType) {
      const auto& struct_type = canonical.as<slang::ast::PackedStructType>();

      // Collect fields via Scope interface (membersOfType)
      std::vector<const slang::ast::FieldSymbol*> fields;
      for (const auto& field :
           struct_type.membersOfType<slang::ast::FieldSymbol>()) {
        fields.push_back(&field);
      }

      if (elements.size() != fields.size()) {
        ctx->sink->Error(span, "struct constant size mismatch");
        return hir::kInvalidExpressionId;
      }

      std::vector<hir::ExpressionId> field_values;
      field_values.reserve(elements.size());
      for (size_t i = 0; i < elements.size(); ++i) {
        hir::ExpressionId field_id = LowerConstantValueExpression(
            elements[i], fields[i]->getType(), span, ctx);
        if (!field_id) {
          return hir::kInvalidExpressionId;
        }
        field_values.push_back(field_id);
      }

      TypeId type_id = LowerType(type, span, ctx);
      if (!type_id) {
        return hir::kInvalidExpressionId;
      }
      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kStructLiteral,
              .type = type_id,
              .span = span,
              .data = hir::StructLiteralExpressionData{
                  .field_values = std::move(field_values)}});
    }
  }

  ctx->sink->Error(span, "unsupported constant value type");
  return hir::kInvalidExpressionId;
}

// Skip Conversion nodes to get to the underlying expression.
auto UnwrapConversions(const slang::ast::Expression& expr)
    -> const slang::ast::Expression& {
  const slang::ast::Expression* current = &expr;
  while (current->kind == slang::ast::ExpressionKind::Conversion) {
    current = &current->as<slang::ast::ConversionExpression>().operand();
  }
  return *current;
}

// Check if expression is an LValueReference (used in compound assignments).
auto IsLValueReference(const slang::ast::Expression& expr) -> bool {
  return UnwrapConversions(expr).kind ==
         slang::ast::ExpressionKind::LValueReference;
}

// Extract the user's RHS from slang's expanded compound assignment.
// For `a += b`, slang expands to `a = a + b` where inner `a` is
// LValueReference. Returns the non-LValueReference operand from the binary
// expression.
//
// Invariant: slang always expands compound assignments to a binary op with
// exactly one LValueReference operand. Violations are compiler bugs.
auto ExtractCompoundAssignmentRhs(const slang::ast::Expression& expanded_rhs)
    -> const slang::ast::Expression& {
  const auto& unwrapped = UnwrapConversions(expanded_rhs);
  if (unwrapped.kind != slang::ast::ExpressionKind::BinaryOp) {
    throw common::InternalError(
        "ExtractCompoundAssignmentRhs",
        "compound assignment RHS must be a binary expression");
  }
  const auto& binary = unwrapped.as<slang::ast::BinaryExpression>();

  bool left_is_lvalue_ref = IsLValueReference(binary.left());
  bool right_is_lvalue_ref = IsLValueReference(binary.right());

  // Exactly one operand should be LValueReference
  if (left_is_lvalue_ref && !right_is_lvalue_ref) {
    return binary.right();
  }
  if (!left_is_lvalue_ref && right_is_lvalue_ref) {
    return binary.left();
  }

  // Neither or both - invariant violation
  throw common::InternalError(
      "ExtractCompoundAssignmentRhs",
      "expected exactly one LValueReference operand in compound assignment");
}

// Try to get constant value from a symbol (params, enum values, etc.).
// Returns the constant value if the symbol is a compile-time constant,
// or nullptr if it's a runtime variable that needs lookup.
// Note: source_range is only used for Parameter (better slang diagnostics).
auto TryGetConstantValue(
    const slang::ast::Symbol& symbol, slang::SourceRange source_range)
    -> const slang::ConstantValue* {
  using slang::ast::SymbolKind;

  switch (symbol.kind) {
    case SymbolKind::Parameter: {
      const auto& param = symbol.as<slang::ast::ParameterSymbol>();
      return &param.getValue(source_range);
    }
    case SymbolKind::EnumValue: {
      const auto& ev = symbol.as<slang::ast::EnumValueSymbol>();
      return &ev.getValue();  // No source_range needed for enum values
    }
    default:
      return nullptr;
  }
}

}  // namespace

// Determine if inside set item should use ==? (wildcard equality).
// SEMANTIC DEVIATION: Slang converts ? to z at parse time, so we treat
// ANY constant with X/Z bits as a wildcard pattern.
// NOTE: Only checks literal kinds directly. Parameters and constant expressions
// with X/Z are not detected (they use ==). This is a limitation.
auto InsideItemUsesWildcardEq(const slang::ast::Expression& item) -> bool {
  // Check integer literals for unknown bits
  if (item.kind == slang::ast::ExpressionKind::IntegerLiteral) {
    const auto& lit = item.as<slang::ast::IntegerLiteral>();
    return lit.getValue().hasUnknown();
  }
  // Check unbased unsized literals (e.g., 'x, 'z)
  if (item.kind == slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral) {
    const auto& lit = item.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
    return lit.getValue().hasUnknown();
  }
  // Non-literal -> use == (correct SV semantics for variables)
  return false;
}

auto LowerExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  // Aliases for backward compatibility with existing code
  auto& registrar = *view.registrar;
  auto* ctx = view.context;
  [[maybe_unused]] const auto* frame = view.frame;

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

    case ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      const auto& literal = expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
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
      // String literals always use kString type, regardless of slang's
      // reported type (which may be a packed bit array for format strings).
      TypeId type =
          ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
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
      std::string_view name = named.symbol.name;

      // Handle type parameters - explicit unsupported error
      if (named.symbol.kind == slang::ast::SymbolKind::TypeParameter) {
        ctx->ErrorFmt(span, "type parameters not supported ('{}')", name);
        return hir::kInvalidExpressionId;
      }

      // Try constant symbol resolution first (params, enum values, etc.).
      // Slang already has the constant value - we just ask for it.
      if (const auto* cv =
              TryGetConstantValue(named.symbol, expr.sourceRange)) {
        // Check for bad/invalid/unbounded values
        if (cv->bad()) {
          ctx->ErrorFmt(span, "symbol '{}' has invalid value", name);
          return hir::kInvalidExpressionId;
        }
        if (cv->isUnbounded()) {
          ctx->ErrorFmt(span, "unbounded value '{}' not supported", name);
          return hir::kInvalidExpressionId;
        }

        if (cv->isInteger()) {
          if (expr.type == nullptr || !expr.type->isIntegral()) {
            ctx->ErrorFmt(
                span, "unsupported constant type for '{}' (expected integral)",
                name);
            return hir::kInvalidExpressionId;
          }
          TypeId type = LowerType(*expr.type, span, ctx);
          if (!type) {
            return hir::kInvalidExpressionId;
          }
          ConstId constant = LowerIntegralConstant(cv->integer(), type, ctx);
          return ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kConstant,
                  .type = type,
                  .span = span,
                  .data = hir::ConstantExpressionData{.constant = constant}});
        }

        if (cv->isUnpacked()) {
          if (expr.type == nullptr) {
            return hir::kInvalidExpressionId;
          }
          return LowerConstantValueExpression(*cv, *expr.type, span, ctx);
        }

        if (cv->isReal() || cv->isShortReal()) {
          return LowerConstantValueExpression(*cv, *expr.type, span, ctx);
        }

        if (cv->isString()) {
          if (expr.type == nullptr) {
            return hir::kInvalidExpressionId;
          }
          return LowerConstantValueExpression(*cv, *expr.type, span, ctx);
        }

        // Unsupported constant value type
        ctx->ErrorFmt(
            span, "unsupported constant type '{}' for '{}'",
            expr.type != nullptr ? expr.type->toString() : "unknown", name);
        return hir::kInvalidExpressionId;
      }

      // Fall back to variable lookup (runtime symbols)
      SymbolId sym = registrar.Lookup(named.symbol);
      if (!sym) {
        ctx->ErrorFmt(span, "undefined symbol '{}'", name);
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
        ctx->ErrorFmt(
            span, "unsupported unary operator '{}'", toString(unary.op));
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId operand = LowerExpression(unary.operand(), view);
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
        ctx->ErrorFmt(
            span, "unsupported binary operator '{}'", toString(binary.op));
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId lhs = LowerExpression(binary.left(), view);
      if (!lhs) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId rhs = LowerExpression(binary.right(), view);
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

      hir::ExpressionId size_expr = LowerExpression(new_arr.sizeExpr(), view);
      if (!size_expr) {
        return hir::kInvalidExpressionId;
      }

      std::optional<hir::ExpressionId> init_expr;
      if (new_arr.initExpr() != nullptr) {
        hir::ExpressionId init_id = LowerExpression(*new_arr.initExpr(), view);
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

    case ExpressionKind::Call:
      return LowerCallExpression(expr, view);

    case ExpressionKind::Conversion:
      return LowerConversionExpression(expr, view);

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
      hir::ExpressionId cond_expr = LowerExpression(*condition.expr, view);
      if (!cond_expr) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId then_expr = LowerExpression(cond.left(), view);
      if (!then_expr) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId else_expr = LowerExpression(cond.right(), view);
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

      // Lower assign target to get root symbol for semantic gates
      auto target_opt = LowerAssignTarget(assign.left(), view);
      if (!target_opt) {
        // Error already emitted by LowerAssignTarget
        return hir::kInvalidExpressionId;
      }

      // GATE: reject procedural writes to nets
      const auto& sym_info = (*ctx->symbol_table)[target_opt->root_symbol];
      if (sym_info.kind == SymbolKind::kNet) {
        ctx->ErrorFmt(
            target_opt->span, "procedural assignment to net '{}' not supported",
            sym_info.name);
        return hir::kInvalidExpressionId;
      }

      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      // Check if this is a compound assignment (+=, -=, etc.)
      if (assign.op.has_value()) {
        // Compound assignment: extract the operator and user's RHS
        auto hir_op = ConvertBinaryOp(*assign.op);
        if (!hir_op) {
          ctx->ErrorFmt(
              span, "unsupported compound assignment operator '{}'",
              toString(*assign.op));
          return hir::kInvalidExpressionId;
        }

        // Lower target (the lvalue being modified)
        hir::ExpressionId target = LowerExpression(assign.left(), view);
        if (!target) {
          return hir::kInvalidExpressionId;
        }

        // Extract and lower the user's RHS from slang's expanded binary
        const auto& rhs = ExtractCompoundAssignmentRhs(assign.right());
        hir::ExpressionId operand = LowerExpression(rhs, view);
        if (!operand) {
          return hir::kInvalidExpressionId;
        }

        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kCompoundAssignment,
                .type = type,
                .span = span,
                .data =
                    hir::CompoundAssignmentExpressionData{
                        .op = *hir_op, .target = target, .operand = operand},
            });
      }

      // Regular assignment
      hir::ExpressionId target = LowerExpression(assign.left(), view);
      if (!target) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId value = LowerExpression(assign.right(), view);
      if (!value) {
        return hir::kInvalidExpressionId;
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kAssignment,
              .type = type,
              .span = span,
              .data =
                  hir::AssignmentExpressionData{
                      .target = target,
                      .value = value,
                      .is_non_blocking = assign.isNonBlocking()},
          });
    }

    case ExpressionKind::ElementSelect:
      return LowerElementSelectExpression(expr, view);

    case ExpressionKind::RangeSelect:
      return LowerRangeSelectExpression(expr, view);

    case ExpressionKind::MemberAccess:
      return LowerMemberAccessExpression(expr, view);

    case ExpressionKind::StructuredAssignmentPattern:
    case ExpressionKind::ReplicatedAssignmentPattern:
    case ExpressionKind::SimpleAssignmentPattern:
      return LowerAssignmentPatternExpression(expr, view);

    case ExpressionKind::Replication:
      return LowerReplicationExpression(expr, view);

    case ExpressionKind::Concatenation:
      return LowerConcatenationExpression(expr, view);

    case ExpressionKind::HierarchicalValue: {
      const auto& hier = expr.as<slang::ast::HierarchicalValueExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      // Try constant folding first (parameters, enum values).
      // Parameters are kConstOnly - no runtime storage, so we must fold them.
      if (const auto* cv = TryGetConstantValue(hier.symbol, expr.sourceRange)) {
        if (cv->bad()) {
          ctx->ErrorFmt(
              span, "hierarchical symbol '{}' has invalid value",
              hier.symbol.name);
          return hir::kInvalidExpressionId;
        }
        if (cv->isInteger()) {
          ConstId constant = LowerIntegralConstant(cv->integer(), type, ctx);
          return ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kConstant,
                  .type = type,
                  .span = span,
                  .data = hir::ConstantExpressionData{.constant = constant}});
        }
        // Extend for other constant types as needed (real, string, etc.)
      }

      // Runtime symbol - needs hierarchical reference
      SymbolId target = registrar.Lookup(hier.symbol);
      if (!target) {
        throw common::InternalError(
            "LowerExpression",
            std::format(
                "hierarchical ref target '{}' (path: {}) not "
                "pre-registered — either unsupported symbol kind or "
                "symbol registration gap",
                hier.symbol.name, hier.symbol.getHierarchicalPath()));
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kHierarchicalRef,
              .type = type,
              .span = span,
              .data = hir::HierarchicalRefExpressionData{.target = target},
          });
    }

    case ExpressionKind::Inside: {
      const auto& inside = expr.as<slang::ast::InsideExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      // Lower the left operand (value being tested)
      hir::ExpressionId left = LowerExpression(inside.left(), view);
      if (!left) {
        return hir::kInvalidExpressionId;
      }

      // Get result type (should be 1-bit logic)
      if (expr.type == nullptr) {
        return hir::kInvalidExpressionId;
      }
      TypeId result_type = LowerType(*expr.type, span, ctx);
      if (!result_type) {
        return hir::kInvalidExpressionId;
      }

      // Build OR chain of predicates
      std::optional<hir::ExpressionId> result;

      for (const auto* item : inside.rangeList()) {
        hir::ExpressionId predicate;

        if (item->kind == ExpressionKind::ValueRange) {
          const auto& range = item->as<slang::ast::ValueRangeExpression>();

          if (range.rangeKind != slang::ast::ValueRangeKind::Simple) {
            ctx->sink->Error(
                span, "tolerance ranges not yet supported in inside");
            return hir::kInvalidExpressionId;
          }

          // Range: (left >= lo) & (left <= hi)
          hir::ExpressionId lo = LowerExpression(range.left(), view);
          if (!lo) {
            return hir::kInvalidExpressionId;
          }
          hir::ExpressionId hi = LowerExpression(range.right(), view);
          if (!hi) {
            return hir::kInvalidExpressionId;
          }

          hir::ExpressionId ge = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = result_type,
                  .span = span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kGreaterThanEqual,
                      .lhs = left,
                      .rhs = lo}});
          hir::ExpressionId le = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = result_type,
                  .span = span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kLessThanEqual,
                      .lhs = left,
                      .rhs = hi}});
          // Use bitwise AND to preserve X propagation
          predicate = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = result_type,
                  .span = span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kBitwiseAnd, .lhs = ge, .rhs = le}});
        } else {
          // Discrete item: choose == or ==? based on whether constant has X/Z
          hir::ExpressionId item_val = LowerExpression(*item, view);
          if (!item_val) {
            return hir::kInvalidExpressionId;
          }

          // SEMANTIC DEVIATION: Slang converts ? to z at parse time, so we
          // treat ANY constant with X/Z bits as a wildcard pattern.
          bool use_wildcard = InsideItemUsesWildcardEq(*item);
          hir::BinaryOp op = use_wildcard ? hir::BinaryOp::kWildcardEqual
                                          : hir::BinaryOp::kEqual;

          predicate = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = result_type,
                  .span = span,
                  .data = hir::BinaryExpressionData{
                      .op = op, .lhs = left, .rhs = item_val}});
        }

        // Combine with bitwise OR (preserves X)
        if (result) {
          result = ctx->hir_arena->AddExpression(
              hir::Expression{
                  .kind = hir::ExpressionKind::kBinaryOp,
                  .type = result_type,
                  .span = span,
                  .data = hir::BinaryExpressionData{
                      .op = hir::BinaryOp::kBitwiseOr,
                      .lhs = *result,
                      .rhs = predicate}});
        } else {
          result = predicate;
        }
      }

      // Handle empty set: inside {} -> always 0
      if (!result) {
        ConstId zero_const =
            LowerIntegralConstant(slang::SVInt(1, 0, false), result_type, ctx);
        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kConstant,
                .type = result_type,
                .span = span,
                .data = hir::ConstantExpressionData{.constant = zero_const}});
      }

      return *result;
    }

    default:
      ctx->ErrorFmt(
          ctx->SpanOf(expr.sourceRange), "unsupported expression kind '{}'",
          toString(expr.kind));
      return hir::kInvalidExpressionId;
  }
}

auto LowerDesignLevelExpression(
    const slang::ast::Expression& expr, Context& ctx,
    SymbolRegistrar& registrar) -> hir::ExpressionId {
  ExpressionLoweringView view{
      .context = &ctx, .registrar = &registrar, .frame = nullptr};
  return LowerExpression(expr, view);
}

auto LowerScopedExpression(
    const slang::ast::Expression& expr, Context& ctx,
    SymbolRegistrar& registrar, const LoweringFrame& frame)
    -> hir::ExpressionId {
  ExpressionLoweringView view{
      .context = &ctx, .registrar = &registrar, .frame = &frame};
  return LowerExpression(expr, view);
}

auto LowerAndCoerce(
    const slang::ast::Expression& expr, TypeId target_type,
    ExpressionLoweringView view) -> hir::ExpressionId {
  auto* ctx = view.context;
  SourceSpan span = ctx->SpanOf(expr.sourceRange);

  // Handle unbased-unsized tick literals with fill-to-width semantics
  if (expr.kind == slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral) {
    const auto& lit = expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>();
    ConstId filled = CreateFilledConstant(lit.getValue(), target_type, ctx);
    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kConstant,
            .type = target_type,
            .span = span,
            .data = hir::ConstantExpressionData{.constant = filled}});
  }

  // Normal expression: lower as-is (type checking/casting happens at HIR->MIR)
  return LowerExpression(expr, view);
}

}  // namespace lyra::lowering::ast_to_hir
