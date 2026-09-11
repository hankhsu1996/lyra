#include <concepts>
#include <cstdint>
#include <expected>
#include <optional>
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
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/lowering/ast_to_hir/expression/aggregates.hpp"
#include "lyra/lowering/ast_to_hir/expression/assignment.hpp"
#include "lyra/lowering/ast_to_hir/expression/calls.hpp"
#include "lyra/lowering/ast_to_hir/expression/expr_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/expression/inside.hpp"
#include "lyra/lowering/ast_to_hir/expression/operators.hpp"
#include "lyra/lowering/ast_to_hir/expression/references.hpp"
#include "lyra/lowering/ast_to_hir/expression/selects.hpp"
#include "lyra/lowering/ast_to_hir/integral_constant.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

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

auto MakeRealLiteralExpr(double value, hir::TypeId type, diag::SourceSpan span)
    -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::RealLiteral{.value = value}},
      .span = span,
  };
}

auto MakeNullLiteralExpr(hir::TypeId type, diag::SourceSpan span) -> hir::Expr {
  return hir::Expr{
      .type = type,
      .data = hir::PrimaryExpr{.data = hir::NullLiteral{}},
      .span = span,
  };
}

// The one expression dispatcher, shared by both pass classes. An expression's
// meaning does not depend on whether a process body or a structural scope
// encloses it, so every context-free kind routes to one template handler listed
// exactly once -- a kind cannot be wired in one context and forgotten in the
// other. The only two real differences are parameterized inline: name
// resolution (a bare name maps to different storage per scope) and the kinds
// LRM allows only in procedural code (increment / decrement, the assignment
// expression, the dynamic-array constructor, the queue `$`), which a structural
// expression rejects. Constructor-time constness is enforced upstream by slang,
// so a structural expression never carries a simulation-time call /
// `$isunknown` even though this dispatcher would lower one.
template <ExprLowerer Lowerer>
auto LowerExprImpl(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::Expression& expr)
    -> diag::Result<hir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  const auto span = unit_lowerer.SourceMapper().SpanOf(expr.sourceRange);
  constexpr bool kProcedural = std::same_as<Lowerer, ProcessLowerer>;

  switch (expr.kind) {
    case slang::ast::ExpressionKind::IntegerLiteral: {
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeIntegerLiteralExpr(
          expr.as<slang::ast::IntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::UnbasedUnsizedIntegerLiteral: {
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeUnbasedUnsizedLiteralExpr(
          expr.as<slang::ast::UnbasedUnsizedIntegerLiteral>(), *type_id, span);
    }

    case slang::ast::ExpressionKind::StringLiteral: {
      const auto& sl = expr.as<slang::ast::StringLiteral>();
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeStringLiteralExpr(std::string{sl.getValue()}, *type_id, span);
    }

    // LRM 5.8: a time literal is a `realtime` value scaled to the time unit of
    // the scope it sits in, so `5us`, `5000ns` and `5` all name the number 5
    // where that unit is 1us. The front end has already applied that scaling,
    // and the unit the literal was written with is spent in producing the
    // number -- what reaches HIR is a real, and its type says which real.
    case slang::ast::ExpressionKind::TimeLiteral: {
      const auto& tl = expr.as<slang::ast::TimeLiteral>();
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeRealLiteralExpr(tl.getValue(), *type_id, span);
    }

    case slang::ast::ExpressionKind::RealLiteral: {
      const auto& rl = expr.as<slang::ast::RealLiteral>();
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeRealLiteralExpr(rl.getValue(), *type_id, span);
    }

    case slang::ast::ExpressionKind::NullLiteral: {
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return MakeNullLiteralExpr(*type_id, span);
    }

    case slang::ast::ExpressionKind::NamedValue:
      if constexpr (kProcedural) {
        return LowerNamedValueProc(
            lowerer, frame, expr.as<slang::ast::NamedValueExpression>());
      } else {
        return LowerNamedValueStructural(
            unit_lowerer, frame, expr.as<slang::ast::NamedValueExpression>());
      }

    case slang::ast::ExpressionKind::HierarchicalValue:
      return LowerHierarchicalValue(
          unit_lowerer, frame,
          expr.as<slang::ast::HierarchicalValueExpression>());

    case slang::ast::ExpressionKind::LValueReference:
      throw InternalError(
          "LowerExpr: slang LValueReference must not reach HIR; compound "
          "assignment is lowered as a single AssignExpr with compound_op, and "
          "the LValueReference-bearing BinaryOp tree slang constructed is "
          "discarded at AST -> HIR");

    case slang::ast::ExpressionKind::Conversion:
      return LowerConversionExpr(
          lowerer, frame, expr.as<slang::ast::ConversionExpression>(), span);

    case slang::ast::ExpressionKind::UnaryOp: {
      const auto& un = expr.as<slang::ast::UnaryExpression>();
      if (slang::ast::OpInfo::isLValue(un.op)) {
        if constexpr (kProcedural) {
          return LowerIncDecExprProc(lowerer, frame, un, span);
        } else {
          // The front end refuses an increment or a decrement outside
          // procedural code, so a structural walk meeting one means it
          // accepted an expression the language does not admit there.
          throw InternalError(
              "structural expression lowering: an increment or decrement "
              "reached a structural expression");
        }
      }
      return LowerUnaryExpr(lowerer, frame, un, span);
    }

    case slang::ast::ExpressionKind::BinaryOp:
      return LowerBinaryExpr(
          lowerer, frame, expr.as<slang::ast::BinaryExpression>(), span);

    case slang::ast::ExpressionKind::ConditionalOp:
      return LowerConditionalExpr(
          lowerer, frame, expr.as<slang::ast::ConditionalExpression>(), span);

    case slang::ast::ExpressionKind::Call:
      return LowerCallExpr(
          lowerer, frame, expr.as<slang::ast::CallExpression>(), span);

    case slang::ast::ExpressionKind::Assignment:
      if constexpr (kProcedural) {
        return LowerAssignmentExprProc(
            lowerer, frame, expr.as<slang::ast::AssignmentExpression>(), span);
      } else {
        // As above: the front end refuses an assignment expression in this
        // position, so reaching here means one was accepted where the language
        // admits none.
        throw InternalError(
            "structural expression lowering: an assignment expression reached "
            "a structural expression");
      }

    case slang::ast::ExpressionKind::ValueRange: {
      const auto& vr = expr.as<slang::ast::ValueRangeExpression>();
      if (vr.rangeKind != slang::ast::ValueRangeKind::Simple) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedExpressionForm,
            "tolerance-range form of a value range is not yet supported "
            "(LRM 11.4.13)");
      }
      auto lo_or = lowerer.LowerExpr(vr.left(), frame);
      if (!lo_or) return std::unexpected(std::move(lo_or.error()));
      const hir::ExprId lo_id = frame.Exprs().Add(*std::move(lo_or));
      auto hi_or = lowerer.LowerExpr(vr.right(), frame);
      if (!hi_or) return std::unexpected(std::move(hi_or.error()));
      const hir::ExprId hi_id = frame.Exprs().Add(*std::move(hi_or));
      auto type_id = unit_lowerer.InternType(*expr.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      return hir::Expr{
          .type = *type_id,
          .data = hir::ValueRangeExpr{.lo = lo_id, .hi = hi_id},
          .span = span};
    }

    case slang::ast::ExpressionKind::Inside:
      return LowerInsideExpr(
          lowerer, frame, expr.as<slang::ast::InsideExpression>(), span);

    case slang::ast::ExpressionKind::ElementSelect:
      return LowerElementSelectExpr(
          lowerer, frame, expr.as<slang::ast::ElementSelectExpression>(), span);

    case slang::ast::ExpressionKind::RangeSelect:
      return LowerRangeSelectExpr(
          lowerer, frame, expr.as<slang::ast::RangeSelectExpression>(), span);

    case slang::ast::ExpressionKind::MemberAccess:
      return LowerMemberAccessExpr(
          lowerer, frame, expr.as<slang::ast::MemberAccessExpression>(), span);

    case slang::ast::ExpressionKind::UnboundedLiteral:
      if constexpr (kProcedural) {
        return LowerUnboundedLiteralProc(lowerer, frame, span);
      } else {
        // LRM 7.10 makes `$` the last index of a queue and says nothing about
        // where it may be written, and the front end admits it here, so what
        // is missing is this walk's reach rather than the source's right to
        // write it.
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
            "a queue index of `$` is not yet supported outside procedural "
            "code (LRM 7.10)");
      }

    case slang::ast::ExpressionKind::Concatenation:
      return LowerConcatExpr(
          lowerer, frame, expr.as<slang::ast::ConcatenationExpression>(), span);

    case slang::ast::ExpressionKind::Replication:
      return LowerReplicationExpr(
          lowerer, frame, expr.as<slang::ast::ReplicationExpression>(), span);

    case slang::ast::ExpressionKind::SimpleAssignmentPattern:
      return LowerSimpleAssignmentPattern(
          lowerer, frame,
          expr.as<slang::ast::SimpleAssignmentPatternExpression>(), span);

    case slang::ast::ExpressionKind::StructuredAssignmentPattern:
      return LowerStructuredAssignmentPattern(
          lowerer, frame,
          expr.as<slang::ast::StructuredAssignmentPatternExpression>(), span);

    case slang::ast::ExpressionKind::ReplicatedAssignmentPattern:
      return LowerReplicatedAssignmentPatternExpr(
          lowerer, frame,
          expr.as<slang::ast::ReplicatedAssignmentPatternExpression>(), span);

    case slang::ast::ExpressionKind::NewArray:
      return LowerNewArrayExpr(
          lowerer, frame, expr.as<slang::ast::NewArrayExpression>(), span);

    case slang::ast::ExpressionKind::NewClass:
      return LowerNewClassExpr(
          lowerer, frame, expr.as<slang::ast::NewClassExpression>(), span);

    case slang::ast::ExpressionKind::TaggedUnion: {
      const auto& tu = expr.as<slang::ast::TaggedUnionExpression>();
      auto type_id = lowerer.Owner().InternType(*tu.type, span);
      if (!type_id) return std::unexpected(std::move(type_id.error()));
      std::optional<hir::ExprId> payload_id;
      if (tu.valueExpr != nullptr) {
        auto payload_or = lowerer.LowerExpr(*tu.valueExpr, frame);
        if (!payload_or) {
          return std::unexpected(std::move(payload_or.error()));
        }
        payload_id = frame.Exprs().Add(*std::move(payload_or));
      }
      // LRM 11.9: `tagged Member primary` names the member by declaration-
      // order position within the union. slang resolves the identifier to a
      // FieldSymbol whose `fieldIndex` gives that position -- HIR carries the
      // position, dropping the name.
      const auto& field = tu.member.as<slang::ast::FieldSymbol>();
      return hir::Expr{
          .type = *type_id,
          .data =
              hir::TaggedUnionExpr{
                  .member_index =
                      base::ComponentIndex{
                          static_cast<std::uint32_t>(field.fieldIndex)},
                  .payload = payload_id},
          .span = span};
    }

    case slang::ast::ExpressionKind::Streaming:
      return LowerStreamingConcatExpr(
          lowerer, frame,
          expr.as<slang::ast::StreamingConcatenationExpression>(), span);

    case slang::ast::ExpressionKind::TypeReference:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "the `type` operator is not yet supported (LRM 6.23)");

    case slang::ast::ExpressionKind::DataType:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a data type in an expression position is not yet supported");

    case slang::ast::ExpressionKind::ArbitrarySymbol:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a name that denotes no value is not yet supported here");

    case slang::ast::ExpressionKind::EmptyArgument:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "an omitted subroutine argument is not yet supported");

    case slang::ast::ExpressionKind::MinTypMax:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a min:typ:max expression is not yet supported");

    case slang::ast::ExpressionKind::CopyClass:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a shallow copy of an object is not yet supported (LRM 8.12)");

    case slang::ast::ExpressionKind::Dist:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a distribution constraint is not yet supported (LRM 18.5)");

    case slang::ast::ExpressionKind::NewCovergroup:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "constructing a covergroup is not yet supported (LRM 19)");

    case slang::ast::ExpressionKind::AssertionInstance:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a sequence or property instance is not yet supported here");

    case slang::ast::ExpressionKind::ClockingEvent:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "a clocking event in an expression is not yet supported");

    // Lowering runs only over an AST the front end accepted, so an expression
    // it could not build never reaches here.
    case slang::ast::ExpressionKind::Invalid:
      throw InternalError("LowerExpr: an invalid expression was lowered");
  }
  throw InternalError("LowerExpr: unknown slang ExpressionKind");
}

}  // namespace

// Class method wrappers. Both pass-class entries delegate to the one
// dispatcher template above; this keeps per-kind handlers free from
// class-method declaration growth and forces the two instantiations here.

auto ProcessLowerer::LowerExpr(
    const slang::ast::Expression& expr, WalkFrame frame)
    -> diag::Result<hir::Expr> {
  return LowerExprImpl(*this, frame, expr);
}

auto StructuralScopeLowerer::LowerExpr(
    const slang::ast::Expression& expr, WalkFrame frame)
    -> diag::Result<hir::Expr> {
  return LowerExprImpl(*this, frame, expr);
}

}  // namespace lyra::lowering::ast_to_hir
