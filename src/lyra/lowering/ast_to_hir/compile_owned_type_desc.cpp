#include "lyra/lowering/ast_to_hir/compile_owned_type_desc.hpp"

#include <cstdint>
#include <format>
#include <string>

#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto PayloadMatchesKind(TypeDescKind kind, const TypeDescPayload& payload)
    -> bool {
  switch (kind) {
    case TypeDescKind::kVoid:
    case TypeDescKind::kReal:
    case TypeDescKind::kShortReal:
    case TypeDescKind::kString:
      return std::holds_alternative<std::monostate>(payload);
    case TypeDescKind::kIntegral:
      return std::holds_alternative<IntegralDesc>(payload);
    case TypeDescKind::kPackedArray:
      return std::holds_alternative<PackedArrayDesc>(payload);
    case TypeDescKind::kPackedStruct:
      return std::holds_alternative<PackedStructDesc>(payload);
    case TypeDescKind::kPackedUnion:
      return std::holds_alternative<PackedUnionDesc>(payload);
    case TypeDescKind::kEnum:
      return std::holds_alternative<EnumDesc>(payload);
    case TypeDescKind::kUnpackedArray:
      return std::holds_alternative<UnpackedArrayDesc>(payload);
    case TypeDescKind::kUnpackedStruct:
      return std::holds_alternative<UnpackedStructDesc>(payload);
    case TypeDescKind::kUnpackedUnion:
      return std::holds_alternative<UnpackedUnionDesc>(payload);
    case TypeDescKind::kDynamicArray:
      return std::holds_alternative<DynamicArrayDesc>(payload);
    case TypeDescKind::kQueue:
      return std::holds_alternative<QueueDesc>(payload);
    case TypeDescKind::kAssociativeArray:
      return std::holds_alternative<AssociativeArrayDesc>(payload);
  }
  return false;
}

// Interns a type entry into the store. Deduplicates structurally identical
// entries via linear scan. This is O(n) per insertion where n is the current
// store size -- acceptable for per-definition stores (typically tens of
// entries), but should migrate to hash-based interning if store sizes grow.
auto InternEntry(
    CompileOwnedTypeStore& store, TypeDescKind kind, TypeDescPayload payload)
    -> TypeDescId {
  if (!PayloadMatchesKind(kind, payload)) {
    throw common::InternalError(
        "CompileOwnedTypeStore::InternEntry",
        std::format(
            "payload variant index {} does not match kind {}", payload.index(),
            static_cast<int>(kind)));
  }
  for (uint32_t i = 0; i < store.entries.size(); ++i) {
    if (store.entries[i].kind == kind && store.entries[i].payload == payload) {
      return TypeDescId{i};
    }
  }
  auto id = TypeDescId{static_cast<uint32_t>(store.entries.size())};
  store.entries.push_back({kind, std::move(payload)});
  return id;
}

auto LowerTypeToStore(
    const slang::ast::Type& type, CompileOwnedTypeStore& store) -> TypeDescId {
  const auto& canonical = type.getCanonicalType();

  if (canonical.isVoid()) {
    return InternEntry(store, TypeDescKind::kVoid, std::monostate{});
  }

  // Packed array before isIntegral
  if (canonical.isPackedArray()) {
    const auto& packed = canonical.as<slang::ast::PackedArrayType>();
    auto element = LowerTypeToStore(packed.elementType, store);
    return InternEntry(
        store, TypeDescKind::kPackedArray,
        PackedArrayDesc{
            .element = element,
            .left = packed.range.left,
            .right = packed.range.right});
  }

  // Packed struct before isIntegral
  if (canonical.kind == slang::ast::SymbolKind::PackedStructType) {
    const auto& pst = canonical.as<slang::ast::PackedStructType>();
    std::vector<PackedStructFieldDesc> fields;
    for (const auto& member : pst.members()) {
      if (member.kind != slang::ast::SymbolKind::Field) {
        continue;
      }
      const auto& field = member.as<slang::ast::FieldSymbol>();
      auto field_type = LowerTypeToStore(field.getType(), store);
      fields.push_back(
          {.type = field_type,
           .bit_offset = static_cast<uint32_t>(field.bitOffset),
           .bit_width = static_cast<uint32_t>(field.getType().getBitWidth())});
    }
    return InternEntry(
        store, TypeDescKind::kPackedStruct,
        PackedStructDesc{
            .fields = std::move(fields),
            .total_bit_width = static_cast<uint32_t>(pst.bitWidth),
            .is_signed = pst.isSigned,
            .is_four_state = pst.isFourState});
  }

  // Packed union before isIntegral
  if (canonical.kind == slang::ast::SymbolKind::PackedUnionType) {
    const auto& put = canonical.as<slang::ast::PackedUnionType>();
    std::vector<PackedStructFieldDesc> members;
    for (const auto& member : put.members()) {
      if (member.kind != slang::ast::SymbolKind::Field) {
        continue;
      }
      const auto& field = member.as<slang::ast::FieldSymbol>();
      auto field_type = LowerTypeToStore(field.getType(), store);
      members.push_back(
          {.type = field_type,
           .bit_offset = static_cast<uint32_t>(field.bitOffset),
           .bit_width = static_cast<uint32_t>(field.getType().getBitWidth())});
    }
    return InternEntry(
        store, TypeDescKind::kPackedUnion,
        PackedUnionDesc{
            .members = std::move(members),
            .total_bit_width = static_cast<uint32_t>(put.bitWidth),
            .is_signed = put.isSigned,
            .is_four_state = put.isFourState});
  }

  // Enum before isIntegral
  if (canonical.isEnum()) {
    const auto& enum_type = canonical.as<slang::ast::EnumType>();
    auto base_type = LowerTypeToStore(enum_type.baseType, store);
    std::vector<EnumMemberDesc> members;
    for (const auto& value_sym : enum_type.values()) {
      const auto& cv = value_sym.getValue();
      members.push_back({.value = LowerSVIntToIntegralConstant(cv.integer())});
    }
    return InternEntry(
        store, TypeDescKind::kEnum,
        EnumDesc{.base_type = base_type, .members = std::move(members)});
  }

  // Generic integral (scalar, predefined int, etc.)
  if (canonical.isIntegral()) {
    return InternEntry(
        store, TypeDescKind::kIntegral,
        IntegralDesc{
            .bit_width = static_cast<uint32_t>(canonical.getBitWidth()),
            .is_signed = canonical.isSigned(),
            .is_four_state = canonical.isFourState()});
  }

  if (canonical.isString()) {
    return InternEntry(store, TypeDescKind::kString, std::monostate{});
  }

  if (canonical.isFloating()) {
    const auto& ft = canonical.as<slang::ast::FloatingType>();
    switch (ft.floatKind) {
      case slang::ast::FloatingType::Real:
      case slang::ast::FloatingType::RealTime:
        return InternEntry(store, TypeDescKind::kReal, std::monostate{});
      case slang::ast::FloatingType::ShortReal:
        return InternEntry(store, TypeDescKind::kShortReal, std::monostate{});
    }
  }

  // Associative array before isUnpackedArray
  if (canonical.isAssociativeArray()) {
    const auto& assoc = canonical.as<slang::ast::AssociativeArrayType>();
    auto element = LowerTypeToStore(assoc.elementType, store);
    std::optional<TypeDescId> key;
    if (assoc.indexType != nullptr) {
      key = LowerTypeToStore(*assoc.indexType, store);
    }
    return InternEntry(
        store, TypeDescKind::kAssociativeArray,
        AssociativeArrayDesc{.element = element, .key = key});
  }

  // Dynamic array before isUnpackedArray
  if (canonical.kind == slang::ast::SymbolKind::DynamicArrayType) {
    const auto& dynamic = canonical.as<slang::ast::DynamicArrayType>();
    auto element = LowerTypeToStore(dynamic.elementType, store);
    return InternEntry(
        store, TypeDescKind::kDynamicArray,
        DynamicArrayDesc{.element = element});
  }

  // Queue before isUnpackedArray
  if (canonical.isQueue()) {
    const auto& queue = canonical.as<slang::ast::QueueType>();
    auto element = LowerTypeToStore(queue.elementType, store);
    return InternEntry(
        store, TypeDescKind::kQueue, QueueDesc{.element = element});
  }

  // Unpacked array (element type only, no dimensions)
  if (canonical.isUnpackedArray()) {
    if (canonical.kind == slang::ast::SymbolKind::FixedSizeUnpackedArrayType) {
      const auto& arr = canonical.as<slang::ast::FixedSizeUnpackedArrayType>();
      auto element = LowerTypeToStore(arr.elementType, store);
      return InternEntry(
          store, TypeDescKind::kUnpackedArray,
          UnpackedArrayDesc{.element = element});
    }
    throw common::InternalError(
        "InternCompileOwnedType",
        "unexpected unpacked array kind in type store lowering");
  }

  // Unpacked struct (field types only, no names)
  if (canonical.isUnpackedStruct()) {
    const auto& ust = canonical.as<slang::ast::UnpackedStructType>();
    std::vector<UnpackedFieldDesc> fields;
    for (const auto* field : ust.fields) {
      auto field_type = LowerTypeToStore(field->getType(), store);
      fields.push_back({.type = field_type});
    }
    return InternEntry(
        store, TypeDescKind::kUnpackedStruct,
        UnpackedStructDesc{std::move(fields)});
  }

  // Unpacked union (member types only, no names)
  if (canonical.isUnpackedUnion()) {
    const auto& uut = canonical.as<slang::ast::UnpackedUnionType>();
    std::vector<UnpackedFieldDesc> members;
    for (const auto* field : uut.fields) {
      auto field_type = LowerTypeToStore(field->getType(), store);
      members.push_back({.type = field_type});
    }
    return InternEntry(
        store, TypeDescKind::kUnpackedUnion,
        UnpackedUnionDesc{std::move(members)});
  }

  throw common::InternalError(
      "InternCompileOwnedType",
      std::format(
          "unsupported type '{}' in compile-owned type lowering",
          type.toString()));
}

auto DumpTypeDescEntry(const CompileOwnedTypeStore& store, TypeDescId id)
    -> std::string {
  const auto& entry = store.entries[id.value];
  switch (entry.kind) {
    case TypeDescKind::kVoid:
      return "void";
    case TypeDescKind::kReal:
      return "real";
    case TypeDescKind::kShortReal:
      return "shortreal";
    case TypeDescKind::kString:
      return "string";

    case TypeDescKind::kIntegral: {
      const auto& desc = std::get<IntegralDesc>(entry.payload);
      return std::format(
          "integral<{},{},{}>", desc.bit_width, desc.is_signed ? "s" : "u",
          desc.is_four_state ? "4s" : "2s");
    }

    case TypeDescKind::kPackedArray: {
      const auto& desc = std::get<PackedArrayDesc>(entry.payload);
      return std::format(
          "packed_array<{}>[{}:{}]", DumpTypeDescEntry(store, desc.element),
          desc.left, desc.right);
    }

    case TypeDescKind::kPackedStruct: {
      const auto& desc = std::get<PackedStructDesc>(entry.payload);
      std::string result = "packed_struct{";
      for (size_t i = 0; i < desc.fields.size(); ++i) {
        if (i > 0) {
          result += ", ";
        }
        result += DumpTypeDescEntry(store, desc.fields[i].type);
      }
      result += "}";
      return result;
    }

    case TypeDescKind::kPackedUnion: {
      const auto& desc = std::get<PackedUnionDesc>(entry.payload);
      std::string result = "packed_union{";
      for (size_t i = 0; i < desc.members.size(); ++i) {
        if (i > 0) {
          result += ", ";
        }
        result += DumpTypeDescEntry(store, desc.members[i].type);
      }
      result += "}";
      return result;
    }

    case TypeDescKind::kEnum: {
      const auto& desc = std::get<EnumDesc>(entry.payload);
      return std::format(
          "enum<{},{}>", DumpTypeDescEntry(store, desc.base_type),
          desc.members.size());
    }

    case TypeDescKind::kUnpackedArray: {
      const auto& desc = std::get<UnpackedArrayDesc>(entry.payload);
      return std::format(
          "unpacked_array<{}>", DumpTypeDescEntry(store, desc.element));
    }

    case TypeDescKind::kUnpackedStruct: {
      const auto& desc = std::get<UnpackedStructDesc>(entry.payload);
      std::string result = "unpacked_struct{";
      for (size_t i = 0; i < desc.fields.size(); ++i) {
        if (i > 0) {
          result += ", ";
        }
        result += DumpTypeDescEntry(store, desc.fields[i].type);
      }
      result += "}";
      return result;
    }

    case TypeDescKind::kUnpackedUnion: {
      const auto& desc = std::get<UnpackedUnionDesc>(entry.payload);
      std::string result = "unpacked_union{";
      for (size_t i = 0; i < desc.members.size(); ++i) {
        if (i > 0) {
          result += ", ";
        }
        result += DumpTypeDescEntry(store, desc.members[i].type);
      }
      result += "}";
      return result;
    }

    case TypeDescKind::kDynamicArray: {
      const auto& desc = std::get<DynamicArrayDesc>(entry.payload);
      return std::format(
          "dynamic_array<{}>", DumpTypeDescEntry(store, desc.element));
    }

    case TypeDescKind::kQueue: {
      const auto& desc = std::get<QueueDesc>(entry.payload);
      return std::format("queue<{}>", DumpTypeDescEntry(store, desc.element));
    }

    case TypeDescKind::kAssociativeArray: {
      const auto& desc = std::get<AssociativeArrayDesc>(entry.payload);
      if (desc.key) {
        return std::format(
            "assoc_array<{},{}>", DumpTypeDescEntry(store, desc.element),
            DumpTypeDescEntry(store, *desc.key));
      }
      return std::format(
          "assoc_array<{},*>", DumpTypeDescEntry(store, desc.element));
    }
  }
  return "unknown";
}

}  // namespace

auto InternCompileOwnedType(
    const slang::ast::Type& root_type, CompileOwnedTypeStore& store)
    -> TypeDescId {
  return LowerTypeToStore(root_type, store);
}

auto DumpCompileOwnedType(const CompileOwnedTypeStore& store, TypeDescId root)
    -> std::string {
  return DumpTypeDescEntry(store, root);
}

}  // namespace lyra::lowering::ast_to_hir
