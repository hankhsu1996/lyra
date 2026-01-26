#include "lyra/lowering/ast_to_hir/expression_access.hpp"

#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/constant_arena.hpp"
#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/detail/expression_lowering.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerConversionExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  using slang::ast::ExpressionKind;

  auto* ctx = view.context;
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

  const slang::ast::Type& src_type = conv.operand().type->getCanonicalType();

  // Handle integral -> string conversion (packed bits to byte string)
  if (tgt_type.isString() && src_type.isIntegral()) {
    hir::ExpressionId operand = LowerExpression(conv.operand(), view);
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
    hir::ExpressionId operand = LowerExpression(conv.operand(), view);
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
    ctx->sink->Error(span, "conversion requires integral or floating types");
    return hir::kInvalidExpressionId;
  }
  // Note: 4-state -> 2-state converts X/Z to 0 (lossy but well-defined)
  // Note: 2-state -> 4-state is lossless (no X/Z bits introduced)
  // Note: real -> integral truncates toward zero

  hir::ExpressionId operand = LowerExpression(conv.operand(), view);
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

auto LowerElementSelectExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
  const auto& select = expr.as<slang::ast::ElementSelectExpression>();
  SourceSpan span = ctx->SpanOf(expr.sourceRange);

  const slang::ast::Type& value_type = select.value().type->getCanonicalType();

  // Check if this is a packed array select
  if (value_type.isPackedArray()) {
    hir::ExpressionId base = LowerExpression(select.value(), view);
    if (!base) {
      return hir::kInvalidExpressionId;
    }
    hir::ExpressionId index = LowerExpression(select.selector(), view);
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
    hir::ExpressionId base = LowerExpression(select.value(), view);
    if (!base) {
      return hir::kInvalidExpressionId;
    }
    hir::ExpressionId index = LowerExpression(select.selector(), view);
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
            .data = hir::BitSelectExpressionData{.base = base, .index = index},
        });
  }

  // Unpacked array case
  if (!value_type.isUnpackedArray()) {
    ctx->ErrorFmt(
        span, "element select not supported on type: {}",
        value_type.toString());
    return hir::kInvalidExpressionId;
  }

  hir::ExpressionId base = LowerExpression(select.value(), view);
  if (!base) {
    return hir::kInvalidExpressionId;
  }
  hir::ExpressionId index = LowerExpression(select.selector(), view);
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
              hir::ElementAccessExpressionData{.base = base, .index = index},
      });
}

auto LowerRangeSelectExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
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

    hir::ExpressionId base = LowerExpression(select.value(), view);
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

  hir::ExpressionId base = LowerExpression(select.value(), view);
  if (!base) {
    return hir::kInvalidExpressionId;
  }

  // Index expression (dynamic)
  hir::ExpressionId index = LowerExpression(select.left(), view);
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

auto LowerMemberAccessExpression(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> hir::ExpressionId {
  auto* ctx = view.context;
  const auto& access = expr.as<slang::ast::MemberAccessExpression>();
  SourceSpan span = ctx->SpanOf(expr.sourceRange);

  const slang::ast::Type& value_type = access.value().type->getCanonicalType();

  const auto* field = access.member.as_if<slang::ast::FieldSymbol>();
  if (field == nullptr) {
    ctx->sink->Error(span, "only struct field access is supported");
    return hir::kInvalidExpressionId;
  }

  hir::ExpressionId base = LowerExpression(access.value(), view);
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

  // Handle unpacked union member access
  if (value_type.isUnpackedUnion()) {
    return ctx->hir_arena->AddExpression(
        hir::Expression{
            .kind = hir::ExpressionKind::kUnionMemberAccess,
            .type = type,
            .span = span,
            .data =
                hir::UnionMemberAccessExpressionData{
                    .base = base, .member_index = field_index},
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

}  // namespace lyra::lowering::ast_to_hir
