#include "lyra/lowering/ast_to_hir/expression_aggregate.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/pattern.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using slang::ast::ExpressionKind;

auto IsPackedContainer(const slang::ast::Type& ct) -> bool {
  if (ct.kind == slang::ast::SymbolKind::PackedStructType ||
      ct.kind == slang::ast::SymbolKind::PackedUnionType) {
    return false;
  }
  return ct.isPackedArray() ||
         (ct.isIntegral() && ct.kind != slang::ast::SymbolKind::ScalarType);
}

// Check if expression is a fill literal ('0/'1/'x/'z or 'b0/'b1/etc.)
// These represent single fill bits for all bit positions
auto IsPackedStruct(const slang::ast::Type& ct) -> bool {
  return ct.kind == slang::ast::SymbolKind::PackedStructType;
}

auto GetElements(const slang::ast::Expression& expr)
    -> std::span<const slang::ast::Expression* const> {
  if (expr.kind == ExpressionKind::SimpleAssignmentPattern) {
    return expr.as<slang::ast::SimpleAssignmentPatternExpression>().elements();
  }
  return expr.as<slang::ast::StructuredAssignmentPatternExpression>()
      .elements();
}

auto LowerElementList(
    std::span<const slang::ast::Expression* const> elements,
    ExpressionLoweringView view)
    -> std::optional<std::vector<hir::ExpressionId>> {
  std::vector<hir::ExpressionId> result;
  result.reserve(elements.size());
  for (const auto* elem : elements) {
    hir::ExpressionId id = LowerExpression(*elem, view);
    if (!id) {
      return std::nullopt;
    }
    result.push_back(id);
  }
  return result;
}

auto MakeArrayLiteral(
    std::vector<hir::ExpressionId> elements, TypeId type, SourceSpan span,
    Context* ctx) -> hir::ExpressionId {
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kArrayLiteral,
          .type = type,
          .span = span,
          .data = hir::ArrayLiteralExpressionData{
              .elements = std::move(elements)}});
}

auto LowerReplicatedAssignmentPattern(
    const slang::ast::ReplicatedAssignmentPatternExpression& repl,
    const slang::ast::Type& ct, SourceSpan span, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;

  if (!ct.isUnpackedArray()) {
    ctx->sink->Error(span, "replication pattern not supported for structs");
    return hir::kInvalidExpressionId;
  }

  const auto* count_cv = repl.count().getConstant();
  if (count_cv == nullptr || !count_cv->isInteger()) {
    SourceSpan count_span = ctx->SpanOf(repl.count().sourceRange);
    ctx->sink->Error(count_span, "variable-count replication not supported");
    return hir::kInvalidExpressionId;
  }

  auto count = count_cv->integer().as<int64_t>();
  if (!count || *count < 0) {
    SourceSpan count_span = ctx->SpanOf(repl.count().sourceRange);
    ctx->sink->Error(count_span, "replication count must be non-negative");
    return hir::kInvalidExpressionId;
  }

  auto lowered = LowerElementList(repl.elements(), view);
  if (!lowered) {
    return hir::kInvalidExpressionId;
  }

  std::vector<hir::ExpressionId> element_ids;
  element_ids.reserve(static_cast<size_t>(*count) * lowered->size());
  for (int64_t i = 0; i < *count; ++i) {
    for (hir::ExpressionId id : *lowered) {
      element_ids.push_back(id);
    }
  }

  TypeId type = LowerType(*repl.type, span, ctx);
  if (!type) {
    return hir::kInvalidExpressionId;
  }

  return MakeArrayLiteral(std::move(element_ids), type, span, ctx);
}

auto LowerUnpackedArrayWithSetters(
    const slang::ast::StructuredAssignmentPatternExpression& structured,
    const slang::ast::FixedSizeUnpackedArrayType& arr, SourceSpan span,
    ExpressionLoweringView view) -> hir::ExpressionId {
  auto* ctx = view.context;

  hir::ExpressionId default_id =
      LowerExpression(*structured.defaultSetter, view);
  if (!default_id) {
    return hir::kInvalidExpressionId;
  }

  auto expected_size = arr.range.width();
  std::vector<hir::ExpressionId> element_ids(expected_size, default_id);

  for (const auto& setter : structured.indexSetters) {
    SourceSpan index_span = ctx->SpanOf(setter.index->sourceRange);
    const auto* index_cv = setter.index->getConstant();
    if (index_cv == nullptr || !index_cv->isInteger()) {
      ctx->sink->Error(
          index_span, "array index in assignment pattern must be constant");
      return hir::kInvalidExpressionId;
    }

    const auto& sv_int = index_cv->integer();
    if (sv_int.hasUnknown()) {
      ctx->sink->Error(
          index_span, "array index in assignment pattern has unknown bits");
      return hir::kInvalidExpressionId;
    }

    auto maybe_index = sv_int.as<int32_t>();
    if (!maybe_index) {
      ctx->sink->Error(
          index_span, "array index in assignment pattern out of range");
      return hir::kInvalidExpressionId;
    }

    int32_t index_value = *maybe_index;
    if (!arr.range.containsPoint(index_value)) {
      ctx->sink->Error(
          index_span, "array index in assignment pattern out of range");
      return hir::kInvalidExpressionId;
    }

    int32_t translated = arr.range.translateIndex(index_value);
    if (translated < 0 ||
        static_cast<size_t>(translated) >= element_ids.size()) {
      ctx->sink->Error(
          index_span, "array index in assignment pattern out of range");
      return hir::kInvalidExpressionId;
    }

    hir::ExpressionId value_id = LowerExpression(*setter.expr, view);
    if (!value_id) {
      return hir::kInvalidExpressionId;
    }
    element_ids[static_cast<size_t>(translated)] = value_id;
  }

  TypeId type = LowerType(*structured.type, span, ctx);
  if (!type) {
    return hir::kInvalidExpressionId;
  }

  return MakeArrayLiteral(std::move(element_ids), type, span, ctx);
}

auto LowerSimpleArrayLiteral(
    std::span<const slang::ast::Expression* const> elements,
    const slang::ast::Type& ct, SourceSpan span, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;

  bool is_dynamic = ct.kind == slang::ast::SymbolKind::DynamicArrayType;
  bool is_queue = ct.isQueue();

  if (ct.isUnpackedArray() && !is_dynamic && !is_queue) {
    const auto& arr = ct.as<slang::ast::FixedSizeUnpackedArrayType>();
    auto expected_size = arr.range.width();
    if (elements.size() != expected_size) {
      ctx->sink->Error(span, "array literal size mismatch");
      return hir::kInvalidExpressionId;
    }
  }

  auto lowered = LowerElementList(elements, view);
  if (!lowered) {
    return hir::kInvalidExpressionId;
  }

  TypeId type = LowerType(ct, span, ctx);
  if (!type) {
    return hir::kInvalidExpressionId;
  }

  return MakeArrayLiteral(std::move(*lowered), type, span, ctx);
}

auto GetStructFieldCount(const slang::ast::Type& ct) -> size_t {
  if (ct.isUnpackedStruct()) {
    return ct.as<slang::ast::UnpackedStructType>().fields.size();
  }
  size_t count = 0;
  for ([[maybe_unused]] const auto& _ :
       ct.as<slang::ast::PackedStructType>()
           .membersOfType<slang::ast::FieldSymbol>()) {
    ++count;
  }
  return count;
}

auto LowerStructLiteral(
    std::span<const slang::ast::Expression* const> elements,
    const slang::ast::Type& ct, SourceSpan span, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;

  size_t field_count = GetStructFieldCount(ct);
  if (elements.size() != field_count) {
    ctx->sink->Error(
        span, std::format(
                  "struct assignment pattern did not resolve all fields "
                  "(got {}, expected {})",
                  elements.size(), field_count));
    return hir::kInvalidExpressionId;
  }

  auto lowered = LowerElementList(elements, view);
  if (!lowered) {
    return hir::kInvalidExpressionId;
  }

  TypeId type = LowerType(ct, span, ctx);
  if (!type) {
    return hir::kInvalidExpressionId;
  }

  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kStructLiteral,
          .type = type,
          .span = span,
          .data = hir::StructLiteralExpressionData{
              .field_values = std::move(*lowered)}});
}

auto LowerPackedDefaultPattern(
    const slang::ast::StructuredAssignmentPatternExpression& structured,
    const slang::ast::Type& /*ct*/, SourceSpan span,
    ExpressionLoweringView view) -> hir::ExpressionId {
  auto* ctx = view.context;

  // Lower target type first
  TypeId target_type = LowerType(*structured.type, span, ctx);
  if (!target_type) {
    return hir::kInvalidExpressionId;
  }

  // Use LowerPattern to create the pattern, then wrap in
  // kMaterializeInitializer
  hir::PatternId pattern_id = LowerPattern(structured, target_type, span, view);
  if (!pattern_id) {
    return hir::kInvalidExpressionId;
  }

  // Wrap pattern in kMaterializeInitializer expression
  // At HIR->MIR lowering, this creates a temp, emits FillPackedEffect, returns
  // Use(temp)
  return ctx->hir_arena->AddExpression(
      hir::Expression{
          .kind = hir::ExpressionKind::kMaterializeInitializer,
          .type = target_type,
          .span = span,
          .data = hir::MaterializeInitializerExpressionData{
              .pattern = pattern_id}});
}

auto LowerArrayAssignmentPattern(
    const slang::ast::Expression& expr, const slang::ast::Type& ct,
    SourceSpan span, ExpressionLoweringView view) -> hir::ExpressionId {
  bool is_dynamic = ct.kind == slang::ast::SymbolKind::DynamicArrayType;
  bool is_queue = ct.isQueue();
  bool is_fixed = ct.isUnpackedArray() && !is_dynamic && !is_queue;

  // Handle structured pattern with default+index setters on fixed arrays
  if (expr.kind == ExpressionKind::StructuredAssignmentPattern && is_fixed) {
    const auto& structured =
        expr.as<slang::ast::StructuredAssignmentPatternExpression>();
    if (structured.defaultSetter != nullptr && structured.typeSetters.empty()) {
      const auto& arr = ct.as<slang::ast::FixedSizeUnpackedArrayType>();
      return LowerUnpackedArrayWithSetters(structured, arr, span, view);
    }
  }

  return LowerSimpleArrayLiteral(GetElements(expr), ct, span, view);
}

}  // namespace

auto LowerAssignmentPatternExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
  SourceSpan span = ctx->SpanOf(expr.sourceRange);

  if (expr.type == nullptr) {
    ctx->sink->Error(span, "assignment pattern has no resolved type");
    return hir::kInvalidExpressionId;
  }
  const slang::ast::Type& ct = expr.type->getCanonicalType();

  // Dispatch by expression kind and target type
  if (expr.kind == ExpressionKind::ReplicatedAssignmentPattern) {
    const auto& repl =
        expr.as<slang::ast::ReplicatedAssignmentPatternExpression>();
    return LowerReplicatedAssignmentPattern(repl, ct, span, view);
  }

  bool is_dynamic = ct.kind == slang::ast::SymbolKind::DynamicArrayType;
  if (ct.isUnpackedArray() || is_dynamic || ct.isQueue()) {
    return LowerArrayAssignmentPattern(expr, ct, span, view);
  }

  if (IsPackedContainer(ct) &&
      expr.kind == ExpressionKind::StructuredAssignmentPattern) {
    const auto& structured =
        expr.as<slang::ast::StructuredAssignmentPatternExpression>();
    return LowerPackedDefaultPattern(structured, ct, span, view);
  }

  if (ct.isUnpackedStruct() || IsPackedStruct(ct)) {
    return LowerStructLiteral(GetElements(expr), ct, span, view);
  }

  ctx->sink->Error(span, "assignment pattern requires struct type");
  return hir::kInvalidExpressionId;
}

auto LowerReplicationExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
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

  hir::ExpressionId inner_id = LowerExpression(repl.concat(), view);
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
          .data = hir::ConcatExpressionData{.operands = std::move(operands)}});
}

auto LowerConcatenationExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
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
        "LowerConcatenationExpression",
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
            span, "string concatenation operand must be string or integral");
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
    hir::ExpressionId op_id = LowerExpression(*op, view);
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
        "LowerConcatenationExpression",
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
          .data = hir::ConcatExpressionData{.operands = std::move(operands)}});
}

}  // namespace lyra::lowering::ast_to_hir
