#include <expected>
#include <string>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/numeric/Time.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/ast_to_hir/expression/aggregates.hpp"
#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"
#include "lyra/lowering/ast_to_hir/expression/calls.hpp"
#include "lyra/lowering/ast_to_hir/expression/inside.hpp"
#include "lyra/lowering/ast_to_hir/expression/operators.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Trivial literal makers used only by the dispatcher. Kept here because the
// dispatcher is their only caller and each is small.

auto MakeIntegerLiteralExpr(
    const slang::ast::IntegerLiteral& lit, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value = LowerSVIntToIntegralConstant(lit.getValue()),
                      .base = LowerSlangLiteralBase(lit.syntax),
                      .declared_unsized = lit.isDeclaredUnsized,
                  }},
      .span = span,
  };
}

auto MakeUnbasedUnsizedLiteralExpr(
    const slang::ast::UnbasedUnsizedIntegerLiteral& lit, hir::TypeId type,
    diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data =
                  hir::IntegerLiteral{
                      .value = LowerSVIntToIntegralConstant(lit.getValue()),
                      .base = hir::IntegerLiteralBase::kUnbased,
                      .declared_unsized = true,
                  }},
      .span = span,
  };
}

auto MakeStringLiteralExpr(
    std::string text, hir::TypeId type, diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data = hir::StringLiteral{.value = std::move(text)}},
      .span = span,
  };
}

auto MakeTimeLiteralExpr(
    double value, hir::TimeScale scale, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data =
          hir::PrimaryExpr{
              .data = hir::TimeLiteral{.value = value, .scale = scale}},
      .span = span,
  };
}

auto MakeRealLiteralExpr(double value, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::RealLiteral{.value = value}},
      .span = span,
  };
}

}  // namespace

auto LowerProcExpr(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr> {
  auto& module = proc.Module();
  const auto span = module.SourceMapper().SpanOf(expr.sourceRange);

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeIntegerLiteralExpr(
          expr.as<slang::ast::IntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeUnbasedUnsizedLiteralExpr(
          expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& sl = expr.as<slang::ast::StringLiteral>();
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeStringLiteralExpr(std::string{sl.getValue()}, *type_id, span);
    }

    case slang::ast::ExpressionKind::TimeLiteral: {
      const auto& tl = expr.as<slang::ast::TimeLiteral>();
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeTimeLiteralExpr(
          tl.getValue(), LowerTimeUnit(tl.getScale().base.unit), *type_id,
          span);
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& rl = expr.as<slang::ast::RealLiteral>();
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeRealLiteralExpr(rl.getValue(), *type_id, span);
    }

    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValueProc(
          proc, frame, expr.as<slang::ast::NamedValueExpression>());

    case slang::ast::ExpressionKind::HierarchicalValue:
      return LowerHierarchicalValueProc(
          module, frame, expr.as<slang::ast::HierarchicalValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      throw InternalError(
          "LowerProcExpr: slang LValueReference must not reach HIR; "
          "compound assignment is lowered as a single AssignExpr with "
          "compound_op, and the LValueReference-bearing BinaryOp tree slang "
          "constructed is discarded at AST -> HIR");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExpr(
          proc, frame, expr.as<slang::ast::ConversionExpression>(), span);

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& un = expr.as<slang::ast::UnaryExpression>();
      if (slang::ast::OpInfo::isLValue(un.op)) {
        return LowerIncDecExprProc(proc, frame, un, span);
      }
      return LowerUnaryExpr(proc, frame, un, span);
    }

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExpr(
          proc, frame, expr.as<slang::ast::BinaryExpression>(), span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExpr(
          proc, frame, expr.as<slang::ast::ConditionalExpression>(), span);

    case slang::ast::ExpressionKind::Call:
      return LowerCallExprProc(
          proc, frame, expr.as<slang::ast::CallExpression>(), span);

    case slang::ast::ExpressionKind::Assignment:
      return LowerAssignmentExprProc(
          proc, frame, expr.as<slang::ast::AssignmentExpression>(), span);

    case slang::ast::ExpressionKind::Inside:
      return LowerInsideExprProc(
          proc, frame, expr.as<slang::ast::InsideExpression>(), span);

    case slang::ast::ExpressionKind::ElementSelect:
      return LowerElementSelectExpr(
          proc, frame, expr.as<slang::ast::ElementSelectExpression>(), span);

    case slang::ast::ExpressionKind::RangeSelect:
      return LowerRangeSelectExpr(
          proc, frame, expr.as<slang::ast::RangeSelectExpression>(), span);

    case slang::ast::ExpressionKind::MemberAccess:
      return LowerMemberAccessExpr(
          proc, frame, expr.as<slang::ast::MemberAccessExpression>(), span);

    case slang::ast::ExpressionKind::UnboundedLiteral:
      return LowerUnboundedLiteralProc(proc, frame, span);

    case slang::ast::ExpressionKind::Concatenation:
      return LowerConcatExpr(
          proc, frame, expr.as<slang::ast::ConcatenationExpression>(), span);

    case slang::ast::ExpressionKind::Replication:
      return LowerReplicationExprProc(
          proc, frame, expr.as<slang::ast::ReplicationExpression>(), span);

    case slang::ast::ExpressionKind::SimpleAssignmentPattern: {
      const auto& sap =
          expr.as<slang::ast::SimpleAssignmentPatternExpression>();
      if (sap.isLValue) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "assignment pattern as LHS destructuring is not yet supported");
      }
      return LowerAssignmentPatternFromElements(proc, frame, sap, span);
    }

    case slang::ast::ExpressionKind::StructuredAssignmentPattern: {
      const auto& sap =
          expr.as<slang::ast::StructuredAssignmentPatternExpression>();
      const auto target_kind = expr.type->getCanonicalType().kind;
      // LRM 7.9.11: an associative literal keeps its `index: value` pairs and
      // optional default; lower the keyed form, not the positional one that
      // would drop both.
      if (target_kind == slang::ast::SymbolKind::AssociativeArrayType) {
        return LowerAssociativeAssignmentPattern(proc, frame, sap, span);
      }
      // Slang accepts `'{idx:val, ...}` for a dynamic-array target as long
      // as indices cover a dense `0..N-1`. The legal subset is equivalent to
      // positional and adds no expressive power; rejecting it here keeps the
      // dispatch narrow until a consumer needs the structured form.
      if (target_kind == slang::ast::SymbolKind::DynamicArrayType) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "index-keyed assignment pattern on a dynamic array is not yet "
            "supported; use positional form");
      }
      return LowerAssignmentPatternFromElements(proc, frame, sap, span);
    }

    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
      return LowerReplicatedAssignmentPatternExpr(
          proc, frame,
          expr.as<slang::ast::ReplicatedAssignmentPatternExpression>(), span);

    case slang::ast::ExpressionKind::NewArray:
      return LowerNewArrayExprProc(
          proc, frame, expr.as<slang::ast::NewArrayExpression>(), span);

    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "this expression form is not supported yet");
  }
}

auto LowerStructuralExpr(
    StructuralScopeLowerer& scope, WalkFrame frame,
    const slang::ast::Expression& expr) -> diag::Result<hir::Expr> {
  auto& module = scope.Module();
  const auto span = module.SourceMapper().SpanOf(expr.sourceRange);

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeIntegerLiteralExpr(
          expr.as<slang::ast::IntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeUnbasedUnsizedLiteralExpr(
          expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& rl = expr.as<slang::ast::RealLiteral>();
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeRealLiteralExpr(rl.getValue(), *type_id, span);
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& sl = expr.as<slang::ast::StringLiteral>();
      auto type_id = module.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeStringLiteralExpr(std::string{sl.getValue()}, *type_id, span);
    }

    case slang::ast::ExpressionKind::NamedValue:
      return LowerNamedValueStructural(
          module, frame, expr.as<slang::ast::NamedValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      throw InternalError(
          "LowerStructuralExpr: slang LValueReference must not reach HIR; "
          "generate-iter compound is rewritten as the next-value BinaryExpr "
          "directly");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExpr(
          scope, frame, expr.as<slang::ast::ConversionExpression>(), span);

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& un = expr.as<slang::ast::UnaryExpression>();
      if (slang::ast::OpInfo::isLValue(un.op)) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
            "increment / decrement is not legal outside procedural code "
            "(LRM 11.3.6, 11.4.2)");
      }
      return LowerUnaryExpr(scope, frame, un, span);
    }

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExpr(
          scope, frame, expr.as<slang::ast::BinaryExpression>(), span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExpr(
          scope, frame, expr.as<slang::ast::ConditionalExpression>(), span);

    case slang::ast::ExpressionKind::ElementSelect:
      return LowerElementSelectExpr(
          scope, frame, expr.as<slang::ast::ElementSelectExpression>(), span);

    case slang::ast::ExpressionKind::RangeSelect:
      return LowerRangeSelectExpr(
          scope, frame, expr.as<slang::ast::RangeSelectExpression>(), span);

    case slang::ast::ExpressionKind::MemberAccess:
      return LowerMemberAccessExpr(
          scope, frame, expr.as<slang::ast::MemberAccessExpression>(), span);

    case slang::ast::ExpressionKind::Concatenation:
      return LowerConcatExpr(
          scope, frame, expr.as<slang::ast::ConcatenationExpression>(), span);

    case slang::ast::ExpressionKind::SimpleAssignmentPattern: {
      const auto& sap =
          expr.as<slang::ast::SimpleAssignmentPatternExpression>();
      if (sap.isLValue) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "assignment pattern as LHS destructuring is not yet supported");
      }
      return LowerAssignmentPatternFromElements(scope, frame, sap, span);
    }

    case slang::ast::ExpressionKind::StructuredAssignmentPattern: {
      const auto& sap =
          expr.as<slang::ast::StructuredAssignmentPatternExpression>();
      const auto target_kind = expr.type->getCanonicalType().kind;
      if (target_kind == slang::ast::SymbolKind::AssociativeArrayType) {
        return LowerAssociativeAssignmentPattern(scope, frame, sap, span);
      }
      if (target_kind == slang::ast::SymbolKind::DynamicArrayType) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedAssignmentPatternKind,
            "index-keyed assignment pattern on a dynamic array is not yet "
            "supported; use positional form");
      }
      return LowerAssignmentPatternFromElements(scope, frame, sap, span);
    }

    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
      return LowerReplicatedAssignmentPatternExpr(
          scope, frame,
          expr.as<slang::ast::ReplicatedAssignmentPatternExpression>(), span);

    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
          "this structural expression form is not supported yet");
  }
}

// Class method wrappers. The pass-class entries delegate to the free-function
// dispatchers above; this keeps per-kind handlers free from class-method
// declaration growth.

auto ProcessLowerer::LowerExpr(
    const slang::ast::Expression& expr, WalkFrame frame)
    -> diag::Result<hir::Expr> {
  return LowerProcExpr(*this, frame, expr);
}

auto ProcessLowerer::LowerInsideItem(
    const slang::ast::Expression& item_expr, WalkFrame frame)
    -> diag::Result<hir::InsideItem> {
  return LowerInsideItemImpl(*this, frame, item_expr);
}

auto ProcessLowerer::ValidateAssignableProcedural(
    const slang::ast::Expression& expr) -> diag::Result<void> {
  return ValidateAssignableImpl(*module_, true, expr);
}

auto StructuralScopeLowerer::LowerExpr(
    const slang::ast::Expression& expr, WalkFrame frame)
    -> diag::Result<hir::Expr> {
  return LowerStructuralExpr(*this, frame, expr);
}

auto StructuralScopeLowerer::ValidateAssignableStructural(
    const slang::ast::Expression& expr) -> diag::Result<void> {
  return ValidateAssignableImpl(*module_, false, expr);
}

}  // namespace lyra::lowering::ast_to_hir
