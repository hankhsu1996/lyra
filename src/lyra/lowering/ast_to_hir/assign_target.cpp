#include "lyra/lowering/ast_to_hir/assign_target.hpp"

#include <algorithm>
#include <format>
#include <ranges>

#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/hir/assign_target.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Check if a conversion is "location-preserving" - i.e., the converted value
// still refers to the same storage location. These can be safely peeled when
// walking LHS expressions.
auto IsLocationPreservingConversion(
    const slang::ast::ConversionExpression& conv) -> bool {
  using CK = slang::ast::ConversionKind;
  switch (conv.conversionKind) {
    case CK::Implicit:
    case CK::Propagated:
      return true;
    case CK::Explicit:
    case CK::StreamingConcat:
    case CK::BitstreamCast:
      return false;
  }
  return false;
}

// Look up FieldId for a struct/union field by ordinal.
// The field must already be interned in the TypeArena when the struct type
// was lowered (during LowerType).
// expected_name is for debug validation that ordinal alignment is correct.
auto LookupFieldId(
    TypeId struct_type, uint32_t field_ordinal, std::string_view expected_name,
    ExpressionLoweringView view) -> FieldId {
  FieldId fid =
      view.context->type_arena->GetFieldId(struct_type, field_ordinal);
  if (!fid) {
    throw common::InternalError(
        "LookupFieldId",
        "field not interned - type lowering should have registered it");
  }

  // Debug invariant: verify ordinal alignment
  const FieldInfo& info = view.context->type_arena->GetField(fid);
  if (info.name != expected_name) {
    throw common::InternalError(
        "LookupFieldId", std::format(
                             "ordinal mismatch: expected '{}', got '{}'",
                             expected_name, info.name));
  }

  return fid;
}

// Build the final AssignTarget from the accumulated path and root symbol.
// The symbol must be a ValueSymbol (variable, net, or formal argument).
auto BuildTarget(
    const slang::ast::ValueSymbol& root_sym, std::vector<hir::Projection>& path,
    const slang::ast::Expression& original_expr, ExpressionLoweringView view)
    -> std::optional<hir::AssignTarget> {
  Context* ctx = view.context;
  SourceSpan span = ctx->SpanOf(original_expr.sourceRange);

  // Look up the root symbol (must already be registered)
  SymbolId root_id = view.registrar->Lookup(root_sym);
  if (!root_id) {
    ctx->ErrorFmt(
        span, "undefined symbol '{}' in assignment target", root_sym.name);
    return std::nullopt;
  }

  // Lower root type
  TypeId root_type = LowerType(root_sym.getType(), span, ctx);
  if (!root_type) {
    return std::nullopt;
  }

  // Lower result type
  TypeId result_type = LowerType(*original_expr.type, span, ctx);
  if (!result_type) {
    return std::nullopt;
  }

  // Reverse path since we built it from expression to root
  std::ranges::reverse(path);

  return hir::AssignTarget{
      .root_symbol = root_id,
      .root_type = root_type,
      .path = std::move(path),
      .result_type = result_type,
      .span = span,
  };
}

}  // namespace

auto LowerAssignTarget(
    const slang::ast::Expression& expr, ExpressionLoweringView view)
    -> std::optional<hir::AssignTarget> {
  using slang::ast::ExpressionKind;

  Context* ctx = view.context;
  std::vector<hir::Projection> path;
  const slang::ast::Expression* current = &expr;

  while (true) {
    switch (current->kind) {
      case ExpressionKind::NamedValue: {
        const auto& named = current->as<slang::ast::NamedValueExpression>();
        return BuildTarget(named.symbol, path, expr, view);
      }

      case ExpressionKind::HierarchicalValue: {
        const auto& hier =
            current->as<slang::ast::HierarchicalValueExpression>();
        return BuildTarget(hier.symbol, path, expr, view);
      }

      case ExpressionKind::MemberAccess: {
        const auto& mem = current->as<slang::ast::MemberAccessExpression>();
        const auto* field = mem.member.as_if<slang::ast::FieldSymbol>();
        if (field == nullptr) {
          ctx->ErrorFmt(
              ctx->SpanOf(current->sourceRange),
              "only field member access supported in assignment target");
          return std::nullopt;
        }

        // Lower base type to determine if packed or unpacked
        TypeId base_ty = LowerType(
            *mem.value().type, ctx->SpanOf(current->sourceRange), ctx);
        if (!base_ty) {
          return std::nullopt;
        }

        const Type& base_type = (*ctx->type_arena)[base_ty];

        if (base_type.Kind() == TypeKind::kPackedStruct) {
          // Packed struct field access
          auto field_ordinal = static_cast<uint32_t>(field->fieldIndex);
          FieldId fid =
              LookupFieldId(base_ty, field_ordinal, field->name, view);

          // Get layout from FieldInfo (single source of truth)
          const FieldInfo& field_info = ctx->type_arena->GetField(fid);
          if (!field_info.bit_offset || !field_info.bit_width) {
            throw common::InternalError(
                "LowerAssignTarget", "packed field missing layout info");
          }

          path.emplace_back(
              hir::PackedFieldProjection{
                  .field = fid,
                  .bit_offset = *field_info.bit_offset,
                  .bit_width = *field_info.bit_width,
              });
        } else if (base_type.Kind() == TypeKind::kUnpackedStruct) {
          // Unpacked struct field access
          auto field_ordinal = static_cast<uint32_t>(field->fieldIndex);
          FieldId fid =
              LookupFieldId(base_ty, field_ordinal, field->name, view);
          path.emplace_back(hir::MemberProjection{.field = fid});
        } else if (base_type.Kind() == TypeKind::kUnpackedUnion) {
          // Unpacked union member access
          auto field_ordinal = static_cast<uint32_t>(field->fieldIndex);
          FieldId fid =
              LookupFieldId(base_ty, field_ordinal, field->name, view);
          path.emplace_back(hir::UnionMemberProjection{.member = fid});
        } else {
          ctx->ErrorFmt(
              ctx->SpanOf(current->sourceRange),
              "unsupported base type for member access in assignment target");
          return std::nullopt;
        }

        current = &mem.value();
        break;
      }

      case ExpressionKind::ElementSelect: {
        const auto& sel = current->as<slang::ast::ElementSelectExpression>();

        // Lower base type to classify
        TypeId base_ty = LowerType(
            *sel.value().type, ctx->SpanOf(current->sourceRange), ctx);
        if (!base_ty) {
          return std::nullopt;
        }

        const Type& base_type = (*ctx->type_arena)[base_ty];

        if (IsPacked(base_type)) {
          // Packed array/integral bit select - dynamic index
          // Lower the index expression to get ExpressionId
          hir::ExpressionId index_id = LowerExpression(sel.selector(), view);
          if (!index_id) {
            return std::nullopt;
          }
          path.emplace_back(
              hir::PackedSelectProjection{
                  .kind = hir::PackedSelectKind::kBitSelect,
                  .params = index_id,
              });
        } else {
          // Unpacked array/queue/dynarray index
          hir::ExpressionId index_id = LowerExpression(sel.selector(), view);
          if (!index_id) {
            return std::nullopt;
          }
          path.emplace_back(hir::IndexProjection{.index = index_id});
        }

        current = &sel.value();
        break;
      }

      case ExpressionKind::RangeSelect: {
        const auto& sel = current->as<slang::ast::RangeSelectExpression>();
        SourceSpan span = ctx->SpanOf(current->sourceRange);

        // Early gate: reject unpacked array slicing as assignment target
        // Range/part selects are only valid on packed types.
        TypeId base_ty = LowerType(*sel.value().type, span, ctx);
        if (!base_ty) {
          return std::nullopt;
        }
        const Type& base_type = (*ctx->type_arena)[base_ty];
        if (!IsPacked(base_type)) {
          ctx->sink->Unsupported(
              span, "unpacked array slicing not supported as assignment target",
              UnsupportedCategory::kFeature);
          return std::nullopt;
        }

        auto selection_kind = sel.getSelectionKind();

        if (selection_kind == slang::ast::RangeSelectionKind::Simple) {
          // Constant range: x[a:b]
          const auto* left_const = sel.left().getConstant();
          const auto* right_const = sel.right().getConstant();
          if (left_const == nullptr || right_const == nullptr) {
            ctx->sink->Error(span, "range select bounds must be constant");
            return std::nullopt;
          }
          auto left = left_const->integer().as<int32_t>();
          auto right = right_const->integer().as<int32_t>();
          if (!left || !right) {
            ctx->sink->Error(span, "range select bounds must fit in int32");
            return std::nullopt;
          }
          path.emplace_back(
              hir::PackedSelectProjection{
                  .kind = hir::PackedSelectKind::kConstantRange,
                  .params = std::make_pair(*left, *right),
              });
        } else if (
            selection_kind == slang::ast::RangeSelectionKind::IndexedUp) {
          // x[i +: w]
          hir::ExpressionId index_id = LowerExpression(sel.left(), view);
          if (!index_id) {
            return std::nullopt;
          }
          const auto* width_const = sel.right().getConstant();
          if (width_const == nullptr) {
            ctx->sink->Error(
                span, "indexed part-select width must be constant");
            return std::nullopt;
          }
          auto width = width_const->integer().as<uint32_t>();
          if (!width || *width == 0) {
            ctx->sink->Error(
                span, "indexed part-select width must be positive");
            return std::nullopt;
          }
          path.emplace_back(
              hir::PackedSelectProjection{
                  .kind = hir::PackedSelectKind::kPartSelectUp,
                  .params = std::make_pair(index_id, *width),
              });
        } else if (
            selection_kind == slang::ast::RangeSelectionKind::IndexedDown) {
          // x[i -: w]
          hir::ExpressionId index_id = LowerExpression(sel.left(), view);
          if (!index_id) {
            return std::nullopt;
          }
          const auto* width_const = sel.right().getConstant();
          if (width_const == nullptr) {
            ctx->sink->Error(
                span, "indexed part-select width must be constant");
            return std::nullopt;
          }
          auto width = width_const->integer().as<uint32_t>();
          if (!width || *width == 0) {
            ctx->sink->Error(
                span, "indexed part-select width must be positive");
            return std::nullopt;
          }
          path.emplace_back(
              hir::PackedSelectProjection{
                  .kind = hir::PackedSelectKind::kPartSelectDown,
                  .params = std::make_pair(index_id, *width),
              });
        } else {
          ctx->sink->Error(span, "unsupported range selection kind");
          return std::nullopt;
        }

        current = &sel.value();
        break;
      }

      case ExpressionKind::Conversion: {
        const auto& conv = current->as<slang::ast::ConversionExpression>();
        // Only peel location-preserving conversions
        if (!IsLocationPreservingConversion(conv)) {
          ctx->ErrorFmt(
              ctx->SpanOf(current->sourceRange),
              "explicit cast not allowed as assignment target");
          return std::nullopt;
        }
        current = &conv.operand();
        break;
      }

      // Hard errors for unsupported shapes
      case ExpressionKind::Concatenation:
        ctx->ErrorFmt(
            ctx->SpanOf(current->sourceRange),
            "concatenation assignment target not supported");
        return std::nullopt;

      case ExpressionKind::Streaming:
        ctx->ErrorFmt(
            ctx->SpanOf(current->sourceRange),
            "streaming assignment target not supported");
        return std::nullopt;

      case ExpressionKind::SimpleAssignmentPattern:
      case ExpressionKind::StructuredAssignmentPattern:
      case ExpressionKind::ReplicatedAssignmentPattern:
        ctx->ErrorFmt(
            ctx->SpanOf(current->sourceRange),
            "assignment pattern as assignment target not supported");
        return std::nullopt;

      default:
        ctx->ErrorFmt(
            ctx->SpanOf(current->sourceRange),
            "unsupported assignment target kind: {}", toString(current->kind));
        return std::nullopt;
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
