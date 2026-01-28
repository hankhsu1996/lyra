#include "lyra/lowering/ast_to_hir/type.hpp"

#include <algorithm>
#include <format>

#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/diagnostic/diagnostic_sink.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Returns true if AST type can be a supported union member
auto IsUnionMemberSupported(const slang::ast::Type& type) -> bool {
  const auto& ct = type.getCanonicalType();
  if (ct.isIntegral()) {
    return true;  // int, logic, bit, packed array, packed struct, packed union
  }
  if (ct.isFloating()) {
    return true;  // real, shortreal
  }
  if (ct.isUnpackedStruct()) {
    const auto& ust = ct.as<slang::ast::UnpackedStructType>();
    return std::ranges::all_of(ust.fields, [](const auto* field) {
      return IsUnionMemberSupported(field->getType());
    });
  }
  if (ct.isUnpackedArray() &&
      ct.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
    const auto& arr = ct.as<slang::ast::FixedSizeUnpackedArrayType>();
    return IsUnionMemberSupported(arr.elementType);
  }
  return false;  // Reject: dynamic arrays, queues, strings, classes, etc.
}

}  // namespace

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

    TypeId type_id = ctx->type_arena->Intern(
        TypeKind::kPackedStruct, PackedStructInfo{
                                     .name = std::move(name),
                                     .fields = std::move(fields),
                                     .total_bit_width = total_width,
                                     .is_signed = pst.isSigned,
                                     .is_four_state = pst.isFourState});

    // Intern fields for O(1) lookup by (TypeId, ordinal)
    const auto& interned_fields =
        (*ctx->type_arena)[type_id].AsPackedStruct().fields;
    for (uint32_t i = 0; i < interned_fields.size(); ++i) {
      const auto& f = interned_fields[i];
      ctx->type_arena->InternField(
          type_id, i,
          FieldInfo{
              .name = f.name,
              .type = f.type,
              .bit_offset = f.bit_offset,
              .bit_width = f.bit_width,
          });
    }
    return type_id;
  }

  // Check for packed union BEFORE isIntegral - packed unions are integral in
  // slang but we want field layout info. Reuse PackedStruct representation.
  if (canonical.kind == slang::ast::SymbolKind::PackedUnionType) {
    const auto& put = canonical.as<slang::ast::PackedUnionType>();
    std::string name = std::string(put.name);
    std::vector<PackedStructField> fields;

    auto total_width = static_cast<uint32_t>(put.bitWidth);

    for (const auto& member : put.members()) {
      if (member.kind != slang::ast::SymbolKind::Field) {
        continue;
      }
      const auto& field = member.as<slang::ast::FieldSymbol>();
      TypeId field_type = LowerType(field.getType(), source, ctx);
      if (!field_type) {
        return kInvalidTypeId;
      }
      auto field_width = static_cast<uint32_t>(field.getType().getBitWidth());
      // slang provides bitOffset=0 for all union members (LSB aligned)
      auto bit_offset = static_cast<uint32_t>(field.bitOffset);

      // Invariant: field must fit within union storage
      if (bit_offset + field_width > total_width) {
        throw common::InternalError(
            "LowerType", "packed union field exceeds container width");
      }

      fields.push_back({
          .name = std::string(field.name),
          .type = field_type,
          .bit_offset = bit_offset,
          .bit_width = field_width,
      });
    }

    TypeId type_id = ctx->type_arena->Intern(
        TypeKind::kPackedStruct, PackedStructInfo{
                                     .name = std::move(name),
                                     .fields = std::move(fields),
                                     .total_bit_width = total_width,
                                     .is_signed = put.isSigned,
                                     .is_four_state = put.isFourState});

    // Intern fields for O(1) lookup by (TypeId, ordinal)
    const auto& interned_fields =
        (*ctx->type_arena)[type_id].AsPackedStruct().fields;
    for (uint32_t i = 0; i < interned_fields.size(); ++i) {
      const auto& f = interned_fields[i];
      ctx->type_arena->InternField(
          type_id, i,
          FieldInfo{
              .name = f.name,
              .type = f.type,
              .bit_offset = f.bit_offset,
              .bit_width = f.bit_width,
          });
    }
    return type_id;
  }

  // Check for enum types BEFORE isIntegral() - enums are integrals in slang
  // but we want to preserve enum identity for methods like first/last/num.
  if (canonical.isEnum()) {
    const auto& enum_type = canonical.as<slang::ast::EnumType>();

    // Lower the base type first
    TypeId base_type = LowerType(enum_type.baseType, source, ctx);
    if (!base_type) {
      return kInvalidTypeId;
    }

    // Extract enum members, converting each value to IntegralConstant
    std::vector<EnumMember> members;
    for (const auto& value_sym : enum_type.values()) {
      const auto& cv = value_sym.getValue();
      IntegralConstant ic = LowerSVIntToIntegralConstant(cv.integer());
      members.push_back({
          .name = std::string(value_sym.name),
          .value = std::move(ic),
      });
    }

    return ctx->type_arena->Intern(
        TypeKind::kEnum,
        EnumInfo{.base_type = base_type, .members = std::move(members)});
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
    switch (ft.floatKind) {
      case slang::ast::FloatingType::Real:
      case slang::ast::FloatingType::RealTime:
        return ctx->type_arena->Intern(TypeKind::kReal, std::monostate{});
      case slang::ast::FloatingType::ShortReal:
        return ctx->type_arena->Intern(TypeKind::kShortReal, std::monostate{});
    }
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

    TypeId type_id = ctx->type_arena->Intern(
        TypeKind::kUnpackedStruct,
        UnpackedStructInfo{
            .name = std::move(name), .fields = std::move(fields)});

    // Intern fields for O(1) lookup by (TypeId, ordinal)
    const auto& interned_fields =
        (*ctx->type_arena)[type_id].AsUnpackedStruct().fields;
    for (uint32_t i = 0; i < interned_fields.size(); ++i) {
      const auto& f = interned_fields[i];
      ctx->type_arena->InternField(
          type_id, i,
          FieldInfo{
              .name = f.name,
              .type = f.type,
              .bit_offset = std::nullopt,
              .bit_width = std::nullopt,
          });
    }
    return type_id;
  }

  if (canonical.isUnpackedUnion()) {
    const auto& uut = canonical.as<slang::ast::UnpackedUnionType>();
    std::vector<StructField> members;

    // First pass: lower all member types
    for (const auto* field : uut.fields) {
      const auto& ft = field->getType();

      if (!IsUnionMemberSupported(ft)) {
        ctx->sink->Error(source, "unsupported unpacked union member type");
        return kInvalidTypeId;
      }

      TypeId member_type = LowerType(ft, source, ctx);
      if (!member_type) {
        return kInvalidTypeId;
      }

      members.push_back(
          {.name = std::string(field->name), .type = member_type});
    }

    // Second pass: compute max width, 4-state flag, and float flag
    // from LOWERED types using type_utils helpers
    uint32_t max_width = 0;
    bool storage_is_four_state = false;
    bool contains_float = false;
    for (const auto& member : members) {
      uint32_t member_width = BlobBitSize(member.type, *ctx->type_arena);
      max_width = std::max(max_width, member_width);
      if (IsFourStateType(member.type, *ctx->type_arena)) {
        storage_is_four_state = true;
      }
      if (ContainsFloat(member.type, *ctx->type_arena)) {
        contains_float = true;
      }
    }

    TypeId type_id = ctx->type_arena->Intern(
        TypeKind::kUnpackedUnion,
        UnpackedUnionInfo{
            .members = std::move(members),
            .storage_bit_width = max_width,
            .contains_float = contains_float,
            .storage_is_four_state = storage_is_four_state,
        });

    // Intern members for O(1) lookup by (TypeId, ordinal)
    const auto& interned_members =
        (*ctx->type_arena)[type_id].AsUnpackedUnion().members;
    for (uint32_t i = 0; i < interned_members.size(); ++i) {
      const auto& m = interned_members[i];
      ctx->type_arena->InternField(
          type_id, i,
          FieldInfo{
              .name = m.name,
              .type = m.type,
              .bit_offset = std::nullopt,
              .bit_width = std::nullopt,
          });
    }
    return type_id;
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
