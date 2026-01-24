#include "lyra/lowering/ast_to_hir/expression_aggregate.hpp"

#include <format>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentPatternExpression(
    const slang::ast::Expression& expr, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  using slang::ast::ExpressionKind;

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

        std::vector<hir::ExpressionId> element_ids(expected_size, default_id);
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
  bool is_packed_struct = ct.kind == slang::ast::SymbolKind::PackedStructType;
  if (!is_unpacked_struct && !is_packed_struct) {
    ctx->sink->Error(span, "assignment pattern requires struct type");
    return hir::kInvalidExpressionId;
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
        span, std::format(
                  "struct assignment pattern did not resolve all fields "
                  "(got {}, expected {})",
                  elements.size(), field_count));
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

auto LowerReplicationExpression(
    const slang::ast::Expression& expr, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  using slang::ast::ExpressionKind;

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

  hir::ExpressionId inner_id = LowerExpression(repl.concat(), registrar, ctx);
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
    const slang::ast::Expression& expr, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ExpressionId {
  using slang::ast::ExpressionKind;

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
