#include "lyra/lowering/ast_to_hir/expression.hpp"

#include <optional>
#include <span>
#include <string_view>

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
#include "lyra/common/internal_error.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/builtin_method.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/system_call.hpp"
#include "lyra/lowering/ast_to_hir/system_function_desugar.hpp"
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

        // Unsupported constant value type
        ctx->ErrorFmt(span, "unsupported constant type for '{}'", name);
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
        ctx->ErrorFmt(
            span, "unsupported binary operator '{}'", toString(binary.op));
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

      // Check if this is a builtin method call on a dynamic array or queue.
      // IMPORTANT: Must check BEFORE isSystemCall() - slang's isSystemCall()
      // returns true for built-in array methods like size().
      if (auto info = ClassifyBuiltinMethod(call)) {
        // Lower the receiver (first argument).
        const auto* first_arg = call.arguments()[0];
        hir::ExpressionId receiver =
            LowerExpression(*first_arg, registrar, ctx);
        if (!receiver) {
          return hir::kInvalidExpressionId;
        }

        // Determine HIR method and result type based on classification.
        hir::BuiltinMethod hir_method{};
        TypeId result_type{};
        std::vector<hir::ExpressionId> args;

        switch (info->method) {
          case BuiltinMethodKind::kSize: {
            hir_method = hir::BuiltinMethod::kSize;
            result_type = ctx->type_arena->Intern(
                TypeKind::kIntegral, IntegralInfo{
                                         .bit_width = 32,
                                         .is_signed = true,
                                         .is_four_state = false});
            break;
          }
          case BuiltinMethodKind::kPopBack:
          case BuiltinMethodKind::kPopFront: {
            hir_method = (info->method == BuiltinMethodKind::kPopBack)
                             ? hir::BuiltinMethod::kPopBack
                             : hir::BuiltinMethod::kPopFront;
            // Get element type from queue.
            const auto& queue_type =
                first_arg->type->getCanonicalType().as<slang::ast::QueueType>();
            result_type = LowerType(queue_type.elementType, span, ctx);
            if (!result_type) {
              return hir::kInvalidExpressionId;
            }
            break;
          }
          case BuiltinMethodKind::kDelete: {
            hir_method = hir::BuiltinMethod::kDelete;
            result_type =
                ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
            // delete() or delete(idx): 0 or 1 arg after receiver
            if (call.arguments().size() > 2) {
              ctx->sink->Error(span, "delete() takes at most one argument");
              return hir::kInvalidExpressionId;
            }
            if (call.arguments().size() == 2) {
              hir::ExpressionId idx =
                  LowerExpression(*call.arguments()[1], registrar, ctx);
              if (!idx) {
                return hir::kInvalidExpressionId;
              }
              args.push_back(idx);
            }
            break;
          }
          case BuiltinMethodKind::kPushBack:
          case BuiltinMethodKind::kPushFront: {
            hir_method = (info->method == BuiltinMethodKind::kPushBack)
                             ? hir::BuiltinMethod::kPushBack
                             : hir::BuiltinMethod::kPushFront;
            result_type =
                ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
            // push_back(val) / push_front(val): exactly 1 arg after receiver
            if (call.arguments().size() != 2) {
              ctx->sink->Error(
                  span, std::format(
                            "{}() requires exactly one argument",
                            call.getSubroutineName()));
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId value =
                LowerExpression(*call.arguments()[1], registrar, ctx);
            if (!value) {
              return hir::kInvalidExpressionId;
            }
            args.push_back(value);
            break;
          }
          case BuiltinMethodKind::kInsert: {
            hir_method = hir::BuiltinMethod::kInsert;
            result_type =
                ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
            // insert(idx, val): exactly 2 args after receiver
            if (call.arguments().size() != 3) {
              ctx->sink->Error(
                  span,
                  "insert() requires exactly two arguments (index and value)");
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId idx =
                LowerExpression(*call.arguments()[1], registrar, ctx);
            if (!idx) {
              return hir::kInvalidExpressionId;
            }
            hir::ExpressionId value =
                LowerExpression(*call.arguments()[2], registrar, ctx);
            if (!value) {
              return hir::kInvalidExpressionId;
            }
            args.push_back(idx);
            args.push_back(value);
            break;
          }
        }

        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kBuiltinMethodCall,
                .type = result_type,
                .span = span,
                .data = hir::BuiltinMethodCallExpressionData{
                    .receiver = receiver,
                    .method = hir_method,
                    .args = std::move(args)}});
      }

      // Pure system functions ($signed, $unsigned, $itor, etc.) -> desugar
      if (auto pure_kind = ClassifyPureSystemFunction(call)) {
        return LowerPureSystemFunction(call, *pure_kind, registrar, ctx);
      }

      // Effectful system calls ($display, etc.)
      if (call.isSystemCall()) {
        return LowerSystemCall(call, registrar, ctx);
      }

      // User function call
      const auto* user_sub =
          std::get_if<const slang::ast::SubroutineSymbol*>(&call.subroutine);
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
        ctx->ErrorFmt(span, "undefined function '{}'", (*user_sub)->name);
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
        case CK::Explicit:
          break;
        case CK::StreamingConcat:
          ctx->sink->Error(span, "streaming concatenation not supported");
          return hir::kInvalidExpressionId;
        case CK::BitstreamCast:
          ctx->sink->Error(span, "bitstream casts not supported");
          return hir::kInvalidExpressionId;
      }

      // Handle StringLiteral -> string conversion specially.
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

      const slang::ast::Type& src_type =
          conv.operand().type->getCanonicalType();

      // Handle integral -> string conversion (packed bits to byte string)
      if (tgt_type.isString() && src_type.isIntegral()) {
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

      // Handle string -> integral conversion (byte string to packed bits)
      if (tgt_type.isIntegral() && src_type.isString()) {
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

      // Validate source and target types for conversions
      // Support: integral<->integral, integral<->float, float<->float
      // (float = real, shortreal, realtime)
      bool src_ok = src_type.isIntegral() || src_type.isFloating();
      bool tgt_ok = tgt_type.isIntegral() || tgt_type.isFloating();
      if (!src_ok || !tgt_ok) {
        ctx->sink->Error(
            span, "conversion requires integral or floating types");
        return hir::kInvalidExpressionId;
      }
      // Note: 4-state -> 2-state converts X/Z to 0 (lossy but well-defined)
      // Note: 2-state -> 4-state is lossless (no X/Z bits introduced)
      // Note: real -> integral truncates toward zero

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
        hir::ExpressionId target =
            LowerExpression(assign.left(), registrar, ctx);
        if (!target) {
          return hir::kInvalidExpressionId;
        }

        // Extract and lower the user's RHS from slang's expanded binary
        const auto& rhs = ExtractCompoundAssignmentRhs(assign.right());
        hir::ExpressionId operand = LowerExpression(rhs, registrar, ctx);
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
      hir::ExpressionId target = LowerExpression(assign.left(), registrar, ctx);
      if (!target) {
        return hir::kInvalidExpressionId;
      }
      hir::ExpressionId value = LowerExpression(assign.right(), registrar, ctx);
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
                      .target = target, .value = value},
          });
    }

    case ExpressionKind::ElementSelect: {
      const auto& select = expr.as<slang::ast::ElementSelectExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      const slang::ast::Type& value_type =
          select.value().type->getCanonicalType();

      // Check if this is a packed array select
      if (value_type.isPackedArray()) {
        hir::ExpressionId base =
            LowerExpression(select.value(), registrar, ctx);
        if (!base) {
          return hir::kInvalidExpressionId;
        }
        hir::ExpressionId index =
            LowerExpression(select.selector(), registrar, ctx);
        if (!index) {
          return hir::kInvalidExpressionId;
        }

        TypeId type = LowerType(*expr.type, span, ctx);
        if (!type) {
          return hir::kInvalidExpressionId;
        }

        // Bounds/direction are now in the base expression's type (kPackedArray)
        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kPackedElementSelect,
                .type = type,
                .span = span,
                .data =
                    hir::PackedElementSelectExpressionData{
                        .base = base, .index = index},
            });
      }

      // Integral (bit select)
      if (value_type.isIntegral()) {
        hir::ExpressionId base =
            LowerExpression(select.value(), registrar, ctx);
        if (!base) {
          return hir::kInvalidExpressionId;
        }
        hir::ExpressionId index =
            LowerExpression(select.selector(), registrar, ctx);
        if (!index) {
          return hir::kInvalidExpressionId;
        }

        TypeId type = LowerType(*expr.type, span, ctx);
        if (!type) {
          return hir::kInvalidExpressionId;
        }

        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kBitSelect,
                .type = type,
                .span = span,
                .data =
                    hir::BitSelectExpressionData{.base = base, .index = index},
            });
      }

      // Unpacked array case
      if (!value_type.isUnpackedArray()) {
        ctx->ErrorFmt(
            span, "element select not supported on type: {}",
            value_type.toString());
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

    case ExpressionKind::RangeSelect: {
      const auto& select = expr.as<slang::ast::RangeSelectExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      auto selection_kind = select.getSelectionKind();

      if (selection_kind == slang::ast::RangeSelectionKind::Simple) {
        // Constant range select: x[a:b]
        const auto* left_const = select.left().getConstant();
        const auto* right_const = select.right().getConstant();
        if (left_const == nullptr || right_const == nullptr) {
          ctx->sink->Error(span, "range select bounds must be constant");
          return hir::kInvalidExpressionId;
        }
        auto left = left_const->integer().as<int32_t>();
        auto right = right_const->integer().as<int32_t>();
        if (!left || !right) {
          ctx->sink->Error(span, "range select bounds must fit in int32");
          return hir::kInvalidExpressionId;
        }

        hir::ExpressionId base =
            LowerExpression(select.value(), registrar, ctx);
        if (!base) {
          return hir::kInvalidExpressionId;
        }

        TypeId type = LowerType(*expr.type, span, ctx);
        if (!type) {
          return hir::kInvalidExpressionId;
        }

        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kRangeSelect,
                .type = type,
                .span = span,
                .data =
                    hir::RangeSelectExpressionData{
                        .base = base, .left = *left, .right = *right},
            });
      }

      // Indexed part-select: x[i +: w] or x[i -: w]
      // Explicitly check for known indexed part-select kinds
      if (selection_kind != slang::ast::RangeSelectionKind::IndexedUp &&
          selection_kind != slang::ast::RangeSelectionKind::IndexedDown) {
        ctx->sink->Error(span, "unsupported range selection kind");
        return hir::kInvalidExpressionId;
      }

      hir::ExpressionId base = LowerExpression(select.value(), registrar, ctx);
      if (!base) {
        return hir::kInvalidExpressionId;
      }

      // Index expression (dynamic)
      hir::ExpressionId index = LowerExpression(select.left(), registrar, ctx);
      if (!index) {
        return hir::kInvalidExpressionId;
      }

      // Width (constant)
      const auto* width_const = select.right().getConstant();
      if (width_const == nullptr) {
        ctx->sink->Error(span, "indexed part-select width must be constant");
        return hir::kInvalidExpressionId;
      }
      auto width = width_const->integer().as<uint32_t>();
      if (!width || *width == 0) {
        ctx->sink->Error(span, "indexed part-select width must be positive");
        return hir::kInvalidExpressionId;
      }

      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      bool ascending =
          (selection_kind == slang::ast::RangeSelectionKind::IndexedUp);

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kIndexedPartSelect,
              .type = type,
              .span = span,
              .data =
                  hir::IndexedPartSelectExpressionData{
                      .base = base,
                      .index = index,
                      .width = *width,
                      .ascending = ascending},
          });
    }

    case ExpressionKind::MemberAccess: {
      const auto& access = expr.as<slang::ast::MemberAccessExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      const slang::ast::Type& value_type =
          access.value().type->getCanonicalType();

      const auto* field = access.member.as_if<slang::ast::FieldSymbol>();
      if (field == nullptr) {
        ctx->sink->Error(span, "only struct field access is supported");
        return hir::kInvalidExpressionId;
      }

      hir::ExpressionId base = LowerExpression(access.value(), registrar, ctx);
      if (!base) {
        return hir::kInvalidExpressionId;
      }

      if (expr.type == nullptr) {
        ctx->sink->Error(span, "internal: member access has no resolved type");
        return hir::kInvalidExpressionId;
      }
      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      int field_index = static_cast<int>(field->fieldIndex);

      // Handle packed struct/union field access
      const auto& canonical_base = value_type.getCanonicalType();
      if (canonical_base.kind == slang::ast::SymbolKind::PackedStructType ||
          canonical_base.kind == slang::ast::SymbolKind::PackedUnionType) {
        // Get packed struct type info to retrieve field offset and width
        TypeId base_type = LowerType(value_type, span, ctx);
        if (!base_type) {
          return hir::kInvalidExpressionId;
        }
        const Type& base_type_info = (*ctx->type_arena)[base_type];
        if (base_type_info.Kind() != TypeKind::kPackedStruct) {
          ctx->sink->Error(span, "internal: expected packed struct type");
          return hir::kInvalidExpressionId;
        }
        const auto& psi = base_type_info.AsPackedStruct();
        if (field_index < 0 ||
            static_cast<size_t>(field_index) >= psi.fields.size()) {
          ctx->sink->Error(span, "internal: field index out of range");
          return hir::kInvalidExpressionId;
        }
        const auto& field_info = psi.fields[static_cast<size_t>(field_index)];

        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kPackedFieldAccess,
                .type = type,
                .span = span,
                .data =
                    hir::PackedFieldAccessExpressionData{
                        .base = base,
                        .field_index = field_index,
                        .bit_offset = field_info.bit_offset,
                        .bit_width = field_info.bit_width},
            });
      }

      // Handle unpacked struct field access
      if (!value_type.isUnpackedStruct()) {
        ctx->sink->Error(span, "member access only supported on struct types");
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

      // Gate: reject replication patterns for both structs and arrays
      if (expr.kind == ExpressionKind::ReplicatedAssignmentPattern) {
        ctx->sink->Error(span, "replication not supported in literals");
        return hir::kInvalidExpressionId;
      }

      // Helper to get elements from both pattern types
      auto get_elements = [&]() {
        if (expr.kind == ExpressionKind::SimpleAssignmentPattern) {
          return expr.as<slang::ast::SimpleAssignmentPatternExpression>()
              .elements();
        }
        return expr.as<slang::ast::StructuredAssignmentPatternExpression>()
            .elements();
      };

      // Handle array/queue literals
      bool is_dynamic = ct.kind == slang::ast::SymbolKind::DynamicArrayType;
      bool is_queue = ct.isQueue();
      if (ct.isUnpackedArray() || is_dynamic || is_queue) {
        auto elements = get_elements();

        if (expr.kind == ExpressionKind::StructuredAssignmentPattern &&
            ct.isUnpackedArray() && !is_dynamic && !is_queue) {
          const auto& structured =
              expr.as<slang::ast::StructuredAssignmentPatternExpression>();
          if (structured.defaultSetter != nullptr &&
              structured.typeSetters.empty()) {
            const auto& arr = ct.as<slang::ast::FixedSizeUnpackedArrayType>();
            auto expected_size = arr.range.width();
            hir::ExpressionId default_id =
                LowerExpression(*structured.defaultSetter, registrar, ctx);
            if (!default_id) {
              return hir::kInvalidExpressionId;
            }

            std::vector<hir::ExpressionId> element_ids(
                expected_size, default_id);
            for (const auto& setter : structured.indexSetters) {
              SourceSpan index_span = ctx->SpanOf(setter.index->sourceRange);
              const auto* index_cv = setter.index->getConstant();
              if (index_cv == nullptr || !index_cv->isInteger()) {
                ctx->sink->Error(
                    index_span,
                    "array index in assignment pattern must be constant");
                return hir::kInvalidExpressionId;
              }
              const auto& sv_int = index_cv->integer();
              if (sv_int.hasUnknown()) {
                ctx->sink->Error(
                    index_span,
                    "array index in assignment pattern has unknown bits");
                return hir::kInvalidExpressionId;
              }
              auto maybe_index = sv_int.as<int32_t>();
              if (!maybe_index) {
                ctx->sink->Error(
                    index_span,
                    "array index in assignment pattern out of range");
                return hir::kInvalidExpressionId;
              }
              int32_t index_value = *maybe_index;
              if (!arr.range.containsPoint(index_value)) {
                ctx->sink->Error(
                    index_span,
                    "array index in assignment pattern out of range");
                return hir::kInvalidExpressionId;
              }
              int32_t translated = arr.range.translateIndex(index_value);
              if (translated < 0 ||
                  static_cast<size_t>(translated) >= element_ids.size()) {
                ctx->sink->Error(
                    index_span,
                    "array index in assignment pattern out of range");
                return hir::kInvalidExpressionId;
              }

              hir::ExpressionId value_id =
                  LowerExpression(*setter.expr, registrar, ctx);
              if (!value_id) {
                return hir::kInvalidExpressionId;
              }
              element_ids[static_cast<size_t>(translated)] = value_id;
            }

            TypeId type = LowerType(*expr.type, span, ctx);
            if (!type) {
              return hir::kInvalidExpressionId;
            }
            return ctx->hir_arena->AddExpression(
                hir::Expression{
                    .kind = hir::ExpressionKind::kArrayLiteral,
                    .type = type,
                    .span = span,
                    .data = hir::ArrayLiteralExpressionData{
                        .elements = std::move(element_ids)}});
          }
        }

        // For fixed unpacked arrays: validate size matches
        if (ct.isUnpackedArray() && !is_dynamic && !is_queue) {
          const auto& arr = ct.as<slang::ast::FixedSizeUnpackedArrayType>();
          auto expected_size = arr.range.width();
          if (elements.size() != expected_size) {
            ctx->sink->Error(span, "array literal size mismatch");
            return hir::kInvalidExpressionId;
          }
        }

        // Lower elements
        std::vector<hir::ExpressionId> element_ids;
        element_ids.reserve(elements.size());
        for (const auto* elem : elements) {
          hir::ExpressionId id = LowerExpression(*elem, registrar, ctx);
          if (!id) {
            return hir::kInvalidExpressionId;
          }
          element_ids.push_back(id);
        }

        TypeId type = LowerType(*expr.type, span, ctx);
        if (!type) {
          return hir::kInvalidExpressionId;
        }

        return ctx->hir_arena->AddExpression(
            hir::Expression{
                .kind = hir::ExpressionKind::kArrayLiteral,
                .type = type,
                .span = span,
                .data = hir::ArrayLiteralExpressionData{
                    .elements = std::move(element_ids)}});
      }

      // Handle struct literals (packed and unpacked)
      bool is_unpacked_struct = ct.isUnpackedStruct();
      bool is_packed_struct =
          ct.kind == slang::ast::SymbolKind::PackedStructType;
      if (!is_unpacked_struct && !is_packed_struct) {
        ctx->sink->Error(span, "assignment pattern requires struct type");
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

      auto elements = get_elements();

      // Gate: full literal only (all fields must be specified)
      // slang validates this and elements() returns fields in declaration order
      size_t field_count = 0;
      if (is_unpacked_struct) {
        field_count = ct.as<slang::ast::UnpackedStructType>().fields.size();
      } else {
        // Packed struct - count fields via Scope interface
        for ([[maybe_unused]] const auto& _ :
             ct.as<slang::ast::PackedStructType>()
                 .membersOfType<slang::ast::FieldSymbol>()) {
          ++field_count;
        }
      }
      if (elements.size() != field_count) {
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

    case ExpressionKind::Replication: {
      const auto& repl = expr.as<slang::ast::ReplicationExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      const auto* count_cv = repl.count().getConstant();
      if (count_cv == nullptr || !count_cv->isInteger()) {
        ctx->sink->Error(span, "variable-count replication not supported");
        return hir::kInvalidExpressionId;
      }

      auto count = count_cv->integer().as<int64_t>();
      if (!count || *count < 0) {
        ctx->sink->Error(span, "replication count must be non-negative");
        return hir::kInvalidExpressionId;
      }

      if (*count == 0 || expr.type->isVoid()) {
        ctx->sink->Error(span, "zero replication as standalone expression");
        return hir::kInvalidExpressionId;
      }

      hir::ExpressionId inner_id =
          LowerExpression(repl.concat(), registrar, ctx);
      if (!inner_id) {
        return hir::kInvalidExpressionId;
      }

      std::vector<hir::ExpressionId> operands(
          static_cast<size_t>(*count), inner_id);

      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConcat,
              .type = type,
              .span = span,
              .data =
                  hir::ConcatExpressionData{.operands = std::move(operands)}});
    }

    case ExpressionKind::Concatenation: {
      const auto& concat = expr.as<slang::ast::ConcatenationExpression>();
      SourceSpan span = ctx->SpanOf(expr.sourceRange);

      // Packed (integral) or string concatenation supported.
      // Slang uses ConcatenationExpression for unpacked array literals too.
      // Streaming concatenation ({>> ...}) is a separate ExpressionKind.
      if (!expr.type->isIntegral() && !expr.type->isString()) {
        ctx->sink->Error(span, "unpacked array concatenation not supported");
        return hir::kInvalidExpressionId;
      }

      bool is_integral_concat = expr.type->isIntegral();

      // LRM 11.4.12: Integral concatenation result is always unsigned
      if (is_integral_concat && expr.type->isSigned()) {
        throw common::InternalError(
            "LowerExpression",
            "concatenation result type should be unsigned per LRM 11.4.12");
      }

      // For string concat, validate operand types at compile time
      if (!is_integral_concat) {
        for (const auto* op : concat.operands()) {
          if (op->type->isVoid()) {
            continue;
          }
          if (!op->type->isString() && !op->type->isIntegral()) {
            ctx->sink->Error(
                span,
                "string concatenation operand must be string or integral");
            return hir::kInvalidExpressionId;
          }
        }
      }

      // Lower operands, skipping void-type (zero replication like {0{x}})
      std::vector<hir::ExpressionId> operands;
      operands.reserve(concat.operands().size());
      uint32_t total_width = 0;
      for (const auto* op : concat.operands()) {
        if (op->type->isVoid()) {
          continue;
        }
        if (is_integral_concat) {
          total_width += op->type->getBitWidth();
        }
        hir::ExpressionId op_id = LowerExpression(*op, registrar, ctx);
        if (!op_id) {
          return hir::kInvalidExpressionId;
        }
        operands.push_back(op_id);
      }

      if (operands.empty()) {
        ctx->sink->Error(span, "concatenation has no non-zero-width operands");
        return hir::kInvalidExpressionId;
      }

      // LRM 11.4.12: Result width equals sum of operand widths (integral only)
      if (is_integral_concat && expr.type->getBitWidth() != total_width) {
        throw common::InternalError(
            "LowerExpression",
            std::format(
                "concatenation result width {} != sum of operand widths {}",
                expr.type->getBitWidth(), total_width));
      }

      TypeId type = LowerType(*expr.type, span, ctx);
      if (!type) {
        return hir::kInvalidExpressionId;
      }

      return ctx->hir_arena->AddExpression(
          hir::Expression{
              .kind = hir::ExpressionKind::kConcat,
              .type = type,
              .span = span,
              .data =
                  hir::ConcatExpressionData{.operands = std::move(operands)}});
    }

    default:
      ctx->ErrorFmt(
          ctx->SpanOf(expr.sourceRange), "unsupported expression kind '{}'",
          toString(expr.kind));
      return hir::kInvalidExpressionId;
  }
}

}  // namespace lyra::lowering::ast_to_hir
