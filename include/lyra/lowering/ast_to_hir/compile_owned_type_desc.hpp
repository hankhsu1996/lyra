#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/integral_constant.hpp"

namespace slang::ast {
class Type;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

enum class TypeDescKind : uint8_t {
  kVoid,
  kIntegral,
  kReal,
  kShortReal,
  kString,
  kPackedArray,
  kPackedStruct,
  kPackedUnion,
  kEnum,
  kUnpackedArray,
  kUnpackedStruct,
  kUnpackedUnion,
  kDynamicArray,
  kQueue,
  kAssociativeArray,
  kChandle,
};

struct TypeDescId {
  uint32_t value;
  auto operator==(const TypeDescId&) const -> bool = default;
};

struct IntegralDesc {
  uint32_t bit_width;
  bool is_signed;
  bool is_four_state;

  auto operator==(const IntegralDesc&) const -> bool = default;
};

struct PackedStructFieldDesc {
  TypeDescId type;
  uint32_t bit_offset;
  uint32_t bit_width;

  auto operator==(const PackedStructFieldDesc&) const -> bool = default;
};

struct PackedArrayDesc {
  TypeDescId element;
  int32_t left;
  int32_t right;

  auto operator==(const PackedArrayDesc&) const -> bool = default;
};

struct PackedStructDesc {
  std::vector<PackedStructFieldDesc> fields;
  uint32_t total_bit_width;
  bool is_signed;
  bool is_four_state;

  auto operator==(const PackedStructDesc&) const -> bool = default;
};

struct PackedUnionDesc {
  std::vector<PackedStructFieldDesc> members;
  uint32_t total_bit_width;
  bool is_signed;
  bool is_four_state;

  auto operator==(const PackedUnionDesc&) const -> bool = default;
};

struct EnumMemberDesc {
  IntegralConstant value;

  auto operator==(const EnumMemberDesc&) const -> bool = default;
};

struct EnumDesc {
  TypeDescId base_type;
  std::vector<EnumMemberDesc> members;

  auto operator==(const EnumDesc&) const -> bool = default;
};

struct UnpackedArrayDesc {
  TypeDescId element;

  auto operator==(const UnpackedArrayDesc&) const -> bool = default;
};

struct UnpackedFieldDesc {
  TypeDescId type;

  auto operator==(const UnpackedFieldDesc&) const -> bool = default;
};

struct UnpackedStructDesc {
  std::vector<UnpackedFieldDesc> fields;

  auto operator==(const UnpackedStructDesc&) const -> bool = default;
};

struct UnpackedUnionDesc {
  std::vector<UnpackedFieldDesc> members;

  auto operator==(const UnpackedUnionDesc&) const -> bool = default;
};

struct DynamicArrayDesc {
  TypeDescId element;

  auto operator==(const DynamicArrayDesc&) const -> bool = default;
};

struct QueueDesc {
  TypeDescId element;

  auto operator==(const QueueDesc&) const -> bool = default;
};

struct AssociativeArrayDesc {
  TypeDescId element;
  std::optional<TypeDescId> key;

  auto operator==(const AssociativeArrayDesc&) const -> bool = default;
};

using TypeDescPayload = std::variant<
    std::monostate, IntegralDesc, PackedArrayDesc, PackedStructDesc,
    PackedUnionDesc, EnumDesc, UnpackedArrayDesc, UnpackedStructDesc,
    UnpackedUnionDesc, DynamicArrayDesc, QueueDesc, AssociativeArrayDesc>;

struct TypeDescEntry {
  TypeDescKind kind;
  TypeDescPayload payload;

  auto operator==(const TypeDescEntry&) const -> bool = default;
};

struct CompileOwnedTypeStore {
  std::vector<TypeDescEntry> entries;
};

// Interns a slang type into the compile-owned type store, returning the ID of
// the root entry. Identical type shapes within the same store reuse the same
// ID. Captures only compile-owned facts (representation shape); strips
// runtime-owned properties (unpacked dimensions, queue bounds, names).
//
// The store is deterministic per-definition: insertion order depends only on
// declaration encounter order, and interning deduplicates structurally
// identical entries.
auto InternCompileOwnedType(
    const slang::ast::Type& root_type, CompileOwnedTypeStore& store)
    -> TypeDescId;

// Text dump of a type descriptor for inspection and testing.
// Produces a human-readable representation like "logic[7:0]" or
// "packed_struct{integral<8,u,4s>, integral<16,s,4s>}".
auto DumpCompileOwnedType(const CompileOwnedTypeStore& store, TypeDescId root)
    -> std::string;

}  // namespace lyra::lowering::ast_to_hir
