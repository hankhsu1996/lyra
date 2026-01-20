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

  if (type.isVoid()) {
    return ctx->type_arena->Intern(TypeKind::kVoid, std::monostate{});
  }

  // Check isPackedArray BEFORE isIntegral - packed arrays are integral in slang
  // but we want them as a distinct type kind with their own range/direction.
  if (type.isPackedArray()) {
    const auto& packed = type.as<slang::ast::PackedArrayType>();
    TypeId element = LowerType(packed.elementType, source, ctx);
    return ctx->type_arena->Intern(
        TypeKind::kPackedArray,
        PackedArrayInfo{
            .element_type = element,
            .range = ConstantRange{
                .left = packed.range.left, .right = packed.range.right}});
  }

  if (type.isIntegral()) {
    return ctx->type_arena->Intern(
        TypeKind::kIntegral, IntegralInfo{
                                 .bit_width = type.getBitWidth(),
                                 .is_signed = type.isSigned(),
                                 .is_four_state = type.isFourState()});
  }

  if (type.isString()) {
    return ctx->type_arena->Intern(TypeKind::kString, std::monostate{});
  }

  if (type.isFloating()) {
    const auto& ft = type.as<slang::ast::FloatingType>();
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
  if (type.kind == slang::ast::SymbolKind::DynamicArrayType) {
    const auto& dynamic = type.as<slang::ast::DynamicArrayType>();
    TypeId element = LowerType(dynamic.elementType, source, ctx);
    if (!element) {
      return kInvalidTypeId;
    }
    return ctx->type_arena->Intern(
        TypeKind::kDynamicArray, DynamicArrayInfo{.element_type = element});
  }

  // Must check before isUnpackedArray() - queues are a subset
  if (type.isQueue()) {
    const auto& queue = type.as<slang::ast::QueueType>();
    TypeId element = LowerType(queue.elementType, source, ctx);
    if (!element) {
      return kInvalidTypeId;
    }
    return ctx->type_arena->Intern(
        TypeKind::kQueue,
        QueueInfo{.element_type = element, .max_bound = queue.maxBound});
  }

  if (type.isUnpackedArray()) {
    if (type.kind != slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      ctx->sink->Error(source, "only fixed-size unpacked arrays supported");
      return kInvalidTypeId;
    }
    const auto& array = type.as<slang::ast::FixedSizeUnpackedArrayType>();
    TypeId element = LowerType(array.elementType, source, ctx);
    return ctx->type_arena->Intern(
        TypeKind::kUnpackedArray,
        UnpackedArrayInfo{
            .element_type = element,
            .range = ConstantRange{
                .left = array.range.left, .right = array.range.right}});
  }

  if (type.isUnpackedStruct()) {
    const auto& ust =
        type.getCanonicalType().as<slang::ast::UnpackedStructType>();
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

  if (type.isStruct()) {
    ctx->sink->Error(source, "only unpacked structs supported");
    return kInvalidTypeId;
  }

  ctx->sink->Error(
      source, std::format("unsupported type '{}'", type.toString()));
  return kInvalidTypeId;
}

}  // namespace lyra::lowering::ast_to_hir
