#include "lyra/lowering/ast_to_hir/statement_timing.hpp"

#include <cstdint>
#include <optional>
#include <vector>

#include <slang/ast/TimingControl.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/statement.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Validates an index expression for use in dynamic edge triggers.
// Recursively accepts: variables, constants, +, -, *, &, |, ^, <<, >>,
// unary -, ~, and implicit conversion expressions.
// Rejects: division, modulo, function calls, ternary, etc.
// Each leaf variable must have bit width <= 64.
auto ValidateIndexExpression(
    const slang::ast::Expression& expr, SourceSpan span, Context* ctx) -> bool {
  using slang::ast::BinaryOperator;
  using slang::ast::ExpressionKind;
  using slang::ast::UnaryOperator;

  switch (expr.kind) {
    case ExpressionKind::NamedValue:
    case ExpressionKind::HierarchicalValue: {
      if (expr.type->getBitWidth() > 64) {
        ctx->sink->Unsupported(
            span,
            "dynamic index edge triggers require index width "
            "<= 64 bits",
            UnsupportedCategory::kFeature);
        return false;
      }
      return true;
    }
    case ExpressionKind::IntegerLiteral:
      return true;
    case ExpressionKind::BinaryOp: {
      const auto& bin = expr.as<slang::ast::BinaryExpression>();
      switch (bin.op) {
        case BinaryOperator::Add:
        case BinaryOperator::Subtract:
        case BinaryOperator::Multiply:
        case BinaryOperator::BinaryAnd:
        case BinaryOperator::BinaryOr:
        case BinaryOperator::BinaryXor:
        case BinaryOperator::LogicalShiftLeft:
        case BinaryOperator::LogicalShiftRight:
        case BinaryOperator::Divide:
        case BinaryOperator::Mod:
        case BinaryOperator::ArithmeticShiftLeft:
        case BinaryOperator::ArithmeticShiftRight:
          return ValidateIndexExpression(bin.left(), span, ctx) &&
                 ValidateIndexExpression(bin.right(), span, ctx);
        default:
          ctx->sink->Unsupported(
              span,
              "unsupported operator in dynamic index edge trigger expression",
              UnsupportedCategory::kFeature);
          return false;
      }
    }
    case ExpressionKind::UnaryOp: {
      const auto& unary = expr.as<slang::ast::UnaryExpression>();
      switch (unary.op) {
        case UnaryOperator::Minus:
        case UnaryOperator::BitwiseNot:
          return ValidateIndexExpression(unary.operand(), span, ctx);
        default:
          ctx->sink->Unsupported(
              span,
              "unsupported operator in dynamic index edge trigger expression",
              UnsupportedCategory::kFeature);
          return false;
      }
    }
    case ExpressionKind::Conversion: {
      const auto& conv = expr.as<slang::ast::ConversionExpression>();
      return ValidateIndexExpression(conv.operand(), span, ctx);
    }
    default:
      ctx->sink->Unsupported(
          span, "unsupported expression in dynamic index edge trigger",
          UnsupportedCategory::kFeature);
      return false;
  }
}

// Validates edge trigger expressions (posedge/negedge/edge).
// Accepts: NamedValue, HierarchicalValue, constant bit/element-selects on
// packed types (any width), packed struct fields (any width), constant-index
// unpacked array elements (any width), unpacked struct fields (any width),
// constant range/part-selects on packed types (any width).
// Edge behavior on multi-bit sub-expressions samples expr[0] (LSB).
// Returns true if valid (fall through to create trigger), false if rejected.
auto ValidateEdgeTriggerExpression(
    const slang::ast::Expression& expr, hir::EventEdgeKind edge,
    SourceSpan span, Context* ctx) -> bool {
  using slang::ast::ExpressionKind;

  if (edge == hir::EventEdgeKind::kNone ||
      expr.kind == ExpressionKind::NamedValue ||
      expr.kind == ExpressionKind::HierarchicalValue) {
    return true;
  }

  if (expr.kind == ExpressionKind::ElementSelect) {
    const auto& select = expr.as<slang::ast::ElementSelectExpression>();
    const auto& base_type = select.value().type->getCanonicalType();
    if (base_type.isIntegral() || base_type.isPackedArray()) {
      const auto* cv = select.selector().getConstant();
      if (cv != nullptr && !cv->bad() && cv->isInteger()) {
        const auto& idx = cv->integer();
        if (idx.hasUnknown()) {
          ctx->sink->Error(
              span,
              "edge trigger bit-select index is out of range "
              "for the packed type width");
          return false;
        }
        auto idx_val = idx.as<int64_t>();
        auto range = base_type.getFixedRange();
        if (idx_val && *idx_val >= range.lower() && *idx_val <= range.upper()) {
          return true;
        }
        ctx->sink->Error(
            span,
            "edge trigger bit-select index is out of range "
            "for the packed type width");
        return false;
      }
      return ValidateIndexExpression(select.selector(), span, ctx);
    }
    if (base_type.isFixedSize()) {
      const auto* cv = select.selector().getConstant();
      if (cv != nullptr && !cv->bad() && cv->isInteger()) {
        const auto& idx = cv->integer();
        if (idx.hasUnknown()) {
          ctx->sink->Error(span, "edge trigger array index is out of range");
          return false;
        }
        auto idx_val = idx.as<int64_t>();
        auto range = base_type.getFixedRange();
        if (!idx_val || *idx_val < range.lower() || *idx_val > range.upper()) {
          ctx->sink->Error(span, "edge trigger array index is out of range");
          return false;
        }
        return true;
      }
      return ValidateIndexExpression(select.selector(), span, ctx);
    }
    if (base_type.kind == slang::ast::SymbolKind::DynamicArrayType ||
        base_type.isQueue()) {
      return ValidateIndexExpression(select.selector(), span, ctx);
    }
    if (base_type.isAssociativeArray()) {
      ctx->sink->Unsupported(
          span,
          "edge triggers on associative array elements are not yet supported",
          UnsupportedCategory::kFeature);
      return false;
    }
    ctx->sink->Unsupported(
        span, "edge triggers on this array type are not supported",
        UnsupportedCategory::kFeature);
    return false;
  }

  if (expr.kind == ExpressionKind::MemberAccess) {
    const auto& access = expr.as<slang::ast::MemberAccessExpression>();
    const auto& base_type = access.value().type->getCanonicalType();
    if (base_type.kind != slang::ast::SymbolKind::PackedStructType &&
        base_type.kind != slang::ast::SymbolKind::UnpackedStructType) {
      ctx->sink->Unsupported(
          span,
          "edge triggers on non-struct member access are not "
          "supported; use @(*) or @(signal) for level-sensitive "
          "observation",
          UnsupportedCategory::kFeature);
      return false;
    }
    return true;
  }

  if (expr.kind == ExpressionKind::RangeSelect) {
    const auto& select = expr.as<slang::ast::RangeSelectExpression>();
    const auto& base_type = select.value().type->getCanonicalType();
    if (!base_type.isIntegral() && !base_type.isPackedArray()) {
      ctx->sink->Unsupported(
          span,
          "edge triggers on non-packed range selects are not "
          "supported; use @(*) or @(signal) for level-sensitive "
          "observation",
          UnsupportedCategory::kFeature);
      return false;
    }
    auto selection_kind = select.getSelectionKind();
    if (selection_kind == slang::ast::RangeSelectionKind::Simple) {
      auto range = base_type.getFixedRange();

      const auto* left_cv = select.left().getConstant();
      if (left_cv == nullptr || left_cv->bad() || !left_cv->isInteger()) {
        ctx->sink->Error(
            span,
            "edge trigger range-select index is out of range "
            "for the packed type width");
        return false;
      }
      const auto& left_idx = left_cv->integer();
      if (left_idx.hasUnknown()) {
        ctx->sink->Error(
            span,
            "edge trigger range-select index is out of range "
            "for the packed type width");
        return false;
      }
      auto left_val = left_idx.as<int64_t>();
      if (!left_val || *left_val < range.lower() || *left_val > range.upper()) {
        ctx->sink->Error(
            span,
            "edge trigger range-select index is out of range "
            "for the packed type width");
        return false;
      }

      const auto* right_cv = select.right().getConstant();
      if (right_cv == nullptr || right_cv->bad() || !right_cv->isInteger()) {
        ctx->sink->Error(
            span,
            "edge trigger range-select index is out of range "
            "for the packed type width");
        return false;
      }
      const auto& right_idx = right_cv->integer();
      if (right_idx.hasUnknown()) {
        ctx->sink->Error(
            span,
            "edge trigger range-select index is out of range "
            "for the packed type width");
        return false;
      }
      auto right_val = right_idx.as<int64_t>();
      if (!right_val || *right_val < range.lower() ||
          *right_val > range.upper()) {
        ctx->sink->Error(
            span,
            "edge trigger range-select index is out of range "
            "for the packed type width");
        return false;
      }
      return true;
    }
    const auto* cv = select.left().getConstant();
    if (cv != nullptr && !cv->bad() && cv->isInteger()) {
      const auto& idx = cv->integer();
      if (idx.hasUnknown()) {
        ctx->sink->Error(
            span,
            "edge trigger part-select index is out of range "
            "for the packed type width");
        return false;
      }
      auto idx_val = idx.as<int64_t>();
      auto range = base_type.getFixedRange();
      if (!idx_val || *idx_val < range.lower() || *idx_val > range.upper()) {
        ctx->sink->Error(
            span,
            "edge trigger part-select index is out of range "
            "for the packed type width");
        return false;
      }
      if (selection_kind == slang::ast::RangeSelectionKind::IndexedDown) {
        auto bit_width = static_cast<int64_t>(expr.type->getBitWidth());
        if (bit_width > 1) {
          auto lsb_val = *idx_val - (bit_width - 1);
          if (lsb_val < range.lower()) {
            ctx->sink->Error(
                span,
                "edge trigger part-select LSB is out of range "
                "for the packed type width");
            return false;
          }
        }
      }
      return true;
    }
    return ValidateIndexExpression(select.left(), span, ctx);
  }

  ctx->sink->Unsupported(
      span,
      "edge triggers (@posedge/@negedge) are only supported on plain "
      "signal variables, constant bit/element-selects on packed types, "
      "packed struct fields, unpacked struct fields, "
      "constant-index unpacked array elements, and "
      "constant range/part-selects on packed types; use @(*) or @(signal) "
      "for level-sensitive observation",
      UnsupportedCategory::kFeature);
  return false;
}

auto MapEdgeKind(slang::ast::EdgeKind edge) -> hir::EventEdgeKind {
  switch (edge) {
    case slang::ast::EdgeKind::None:
      return hir::EventEdgeKind::kNone;
    case slang::ast::EdgeKind::PosEdge:
      return hir::EventEdgeKind::kPosedge;
    case slang::ast::EdgeKind::NegEdge:
      return hir::EventEdgeKind::kNegedge;
    case slang::ast::EdgeKind::BothEdges:
      return hir::EventEdgeKind::kBothEdges;
  }
  return hir::EventEdgeKind::kNone;
}

// Append the body statement after a wait/delay, wrapping in a block if needed.
auto AppendTimedBody(
    hir::StatementId prefix_stmt, const slang::ast::Statement& body_stmt,
    SourceSpan span, ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  if (body_stmt.kind == slang::ast::StatementKind::Empty) {
    return prefix_stmt;
  }
  auto body_result = LowerStatement(body_stmt, lowerer);
  if (!body_result.has_value()) {
    return prefix_stmt;
  }
  if (!*body_result) {
    return hir::kInvalidStatementId;
  }
  auto* ctx = &lowerer.Ctx();
  return ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kBlock,
          .span = span,
          .data =
              hir::BlockStatementData{
                  .statements = {prefix_stmt, *body_result}},
      });
}

auto LowerDelayControl(
    const slang::ast::TimedStatement& timed, SourceSpan span,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  auto* ctx = &lowerer.Ctx();
  const auto& delay_ctrl = timed.timing.as<slang::ast::DelayControl>();
  const slang::ast::Expression& delay_expr = delay_ctrl.expr;

  uint64_t ticks = 0;

  if (delay_expr.kind == slang::ast::ExpressionKind::IntegerLiteral) {
    const auto& literal = delay_expr.as<slang::ast::IntegerLiteral>();
    auto val = literal.getValue();
    if (val.hasUnknown()) {
      ctx->sink->Error(span, "delay value cannot contain X or Z");
      return hir::kInvalidStatementId;
    }
    if (val.isSigned() && val.isNegative()) {
      ctx->sink->Error(span, "delay value cannot be negative");
      return hir::kInvalidStatementId;
    }
    auto literal_value = static_cast<uint64_t>(val.as<uint64_t>().value());
    ticks = lowerer.ScaleDelayTicks(literal_value);
  } else if (delay_expr.kind == slang::ast::ExpressionKind::TimeLiteral) {
    const auto& literal = delay_expr.as<slang::ast::TimeLiteral>();
    double value = literal.getValue();
    ticks = lowerer.ScaleDelayReal(value);
  } else {
    ctx->sink->Error(span, "only constant delays are supported");
    return hir::kInvalidStatementId;
  }

  hir::StatementId delay_stmt = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kDelay,
          .span = span,
          .data = hir::DelayStatementData{.ticks = ticks},
      });

  return AppendTimedBody(delay_stmt, timed.stmt, span, lowerer);
}

auto LowerSignalEvent(
    const slang::ast::TimedStatement& timed, SourceSpan span,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();

  auto lower_expr = [&](const slang::ast::Expression& expr) {
    return LowerScopedExpression(expr, *ctx, registrar, lowerer.Frame());
  };

  const auto& sig_event = timed.timing.as<slang::ast::SignalEventControl>();

  hir::ExpressionId signal_expr = lower_expr(sig_event.expr);
  if (!signal_expr) {
    return hir::kInvalidStatementId;
  }

  hir::EventEdgeKind edge = MapEdgeKind(sig_event.edge);

  if (!ValidateEdgeTriggerExpression(sig_event.expr, edge, span, ctx)) {
    return hir::kInvalidStatementId;
  }

  std::vector<hir::EventTrigger> triggers;
  triggers.push_back({.signal = signal_expr, .edge = edge});

  hir::StatementId wait_stmt = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kEventWait,
          .span = span,
          .data = hir::EventWaitStatementData{.triggers = std::move(triggers)},
      });

  return AppendTimedBody(wait_stmt, timed.stmt, span, lowerer);
}

auto LowerEventList(
    const slang::ast::TimedStatement& timed, SourceSpan span,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId> {
  auto& registrar = lowerer.Registrar();
  auto* ctx = &lowerer.Ctx();

  auto lower_expr = [&](const slang::ast::Expression& expr) {
    return LowerScopedExpression(expr, *ctx, registrar, lowerer.Frame());
  };

  const auto& event_list = timed.timing.as<slang::ast::EventListControl>();

  std::vector<hir::EventTrigger> triggers;
  for (const slang::ast::TimingControl* event : event_list.events) {
    if (event->kind != slang::ast::TimingControlKind::SignalEvent) {
      ctx->sink->Error(span, "only signal events are supported in event lists");
      return hir::kInvalidStatementId;
    }
    const auto& sig_event = event->as<slang::ast::SignalEventControl>();

    hir::ExpressionId signal_expr = lower_expr(sig_event.expr);
    if (!signal_expr) {
      return hir::kInvalidStatementId;
    }

    hir::EventEdgeKind edge = MapEdgeKind(sig_event.edge);

    if (!ValidateEdgeTriggerExpression(sig_event.expr, edge, span, ctx)) {
      return hir::kInvalidStatementId;
    }

    triggers.push_back({.signal = signal_expr, .edge = edge});
  }

  hir::StatementId wait_stmt = ctx->hir_arena->AddStatement(
      hir::Statement{
          .kind = hir::StatementKind::kEventWait,
          .span = span,
          .data = hir::EventWaitStatementData{.triggers = std::move(triggers)},
      });

  return AppendTimedBody(wait_stmt, timed.stmt, span, lowerer);
}

}  // namespace

auto LowerTimedStatement(
    const slang::ast::TimedStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId> {
  auto* ctx = &lowerer.Ctx();
  SourceSpan span = ctx->SpanOf(stmt.sourceRange);

  if (stmt.timing.kind == slang::ast::TimingControlKind::Delay) {
    return LowerDelayControl(stmt, span, lowerer);
  }

  if (stmt.timing.kind == slang::ast::TimingControlKind::SignalEvent) {
    return LowerSignalEvent(stmt, span, lowerer);
  }

  if (stmt.timing.kind == slang::ast::TimingControlKind::EventList) {
    return LowerEventList(stmt, span, lowerer);
  }

  ctx->sink->Error(
      span, "unsupported timing control kind (only #N and @ supported)");
  return hir::kInvalidStatementId;
}

}  // namespace lyra::lowering::ast_to_hir
