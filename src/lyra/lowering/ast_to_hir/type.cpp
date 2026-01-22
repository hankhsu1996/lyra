#include "lyra/lowering/ast_to_hir/type.hpp"

#include <format>

#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerType(const slang::ast::Type& type, SourceSpan source, Context* ctx)
    -> TypeId {
  if (type.isError()) {
    return kInvalidTypeId;
  }

  // Resolve type aliases once at entry. Use 'canonical' for all type checks
  // and casts. Keep 'type' for error messages to show the original type name.
  const auto& canonical = type.getCanonicalType();

  if (canonical.isVoid()) {
    return ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
  }

  // Check isPackedArray BEFORE isIntegral - packed arrays are integral in slang
  // but we want them as a distinct type kind with their own range/direction.
  if (canonical.isPackedArray()) {
    const auto& packed = canonical.as<slang::ast::PackedArrayType>();
    TypeId element = LowerType(packed.elementType, source, ctx);
    return ctx->type_arena->Intern(
        TypeKind::kPackedArray,
        PackedArrayInfo{
            .element_type = element,
            .range = ConstantRange{
                .left = packed.range.left, .right = packed.range.right}});
  }

  // Check for packed struct BEFORE isIntegral - packed structs are integral in
  // slang but we want them as a distinct type kind with field layout info.
  if (canonical.kind == slang::ast::SymbolKind::PackedStructType) {
    const auto& pst = canonical.as<slang::ast::PackedStructType>();
    std::string name = std::string(pst.name);
    std::vector<PackedStructField> fields;

    auto total_width = static_cast<uint32_t>(pst.bitWidth);

    // Iterate over scope members to find fields
    for (const auto& member : pst.members()) {
      if (member.kind != slang::ast::SymbolKind::Field) {
        continue;
      }
      const auto& field = member.as<slang::ast::FieldSymbol>();
      TypeId field_type = LowerType(field.getType(), source, ctx);
      if (!field_type) {
        return kInvalidTypeId;
      }
      auto field_width = static_cast<uint32_t>(field.getType().getBitWidth());
      // slang already computes bitOffset from LSB for packed types
      auto bit_offset = static_cast<uint32_t>(field.bitOffset);

      fields.push_back({
          .name = std::string(field.name),
          .type = field_type,
          .bit_offset = bit_offset,
          .bit_width = field_width,
      });
    }

    return ctx->type_arena->Intern(
        TypeKind::kPackedStruct, PackedStructInfo{
                                     .name = std::move(name),
                                     .fields = std::move(fields),
                                     .total_bit_width = total_width,
                                     .is_signed = pst.isSigned,
                                     .is_four_state = pst.isFourState});
  }

  if (canonical.isIntegral()) {
    return ctx->type_arena->Intern(
        TypeKind::kIntegral, IntegralInfo{
                                 .bit_width = canonical.getBitWidth(),
                                 .is_signed = canonical.isSigned(),
                                 .is_four_state = canonical.isFourState()});
  }

  if (canonical.isString()) {
    return ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
  }

  if (canonical.isFloating()) {
    const auto& ft = canonical.as<slang::ast::FloatingType>();
    if (ft.floatKind != slang::ast::FloatingType::Real) {
      ctx->sink->Error(
          source, std::format(
                      "unsupported floating type '{}' (only 'real' supported)",
                      type.toString()));
      return kInvalidTypeId;
    }
    return ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
  }

  // Must check before isUnpackedArray() - dynamic arrays are a subset
  if (canonical.kind == slang::ast::SymbolKind::DynamicArrayType) {
    const auto& dynamic = canonical.as<slang::ast::DynamicArrayType>();
    TypeId element = LowerType(dynamic.elementType, source, ctx);
    if (!element) {
      return kInvalidTypeId;
    }
    return ctx->type_arena->Intern(
        TypeKind::kDynamicArray, DynamicArrayInfo{.element_type = element});
  }

  // Must check before isUnpackedArray() - queues are a subset
  if (canonical.isQueue()) {
    const auto& queue = canonical.as<slang::ast::QueueType>();
    TypeId element = LowerType(queue.elementType, source, ctx);
    if (!element) {
      return kInvalidTypeId;
    }
    return ctx->type_arena->Intern(
        TypeKind::kQueue,
        QueueInfo{.element_type = element, .max_bound = queue.maxBound});
  }

  if (canonical.isUnpackedArray()) {
    if (canonical.kind != slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      ctx->sink->Error(source, "only fixed-size unpacked arrays supported");
      return kInvalidTypeId;
    }
    const auto& array = canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
    TypeId element = LowerType(array.elementType, source, ctx);
    return ctx->type_arena->Intern(
        TypeKind::kUnpackedArray,
        UnpackedArrayInfo{
            .element_type = element,
            .range = ConstantRange{
                .left = array.range.left, .right = array.range.right}});
  }

  if (canonical.isUnpackedStruct()) {
    const auto& ust = canonical.as<slang::ast::UnpackedStructType>();
    std::string name = std::string(ust.name);
    std::vector<StructField> fields;

    for (const auto* field : ust.fields) {
      TypeId field_type = LowerType(field->getType(), source, ctx);
      if (!field_type) {
        return kInvalidTypeId;
      }
      fields.push_back({.name = std::string(field->name), .type = field_type});
    }

    return ctx->type_arena->Intern(
        TypeKind::kUnpackedStruct,
        UnpackedStructInfo{
            .name = std::move(name), .fields = std::move(fields)});
  }

  if (canonical.isStruct()) {
    ctx->sink->Error(source, "only packed and unpacked structs supported");
    return kInvalidTypeId;
  }

  ctx->sink->Error(
      source, std::format("unsupported type '{}'", type.toString()));
  return kInvalidTypeId;
}

}  // namespace lyra::lowering::ast_to_hir
