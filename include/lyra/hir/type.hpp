#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

enum class TypeKind {
  kPackedArray,
  kPackedStruct,
  kEnum,
  kUnpackedArray,
  kDynamicArray,
  kQueue,
  kAssociativeArray,
  kString,
  kEvent,
  kReal,
  kShortReal,
  kRealTime,
  kChandle,
  kVoid,
};

enum class BitAtom {
  kBit,
  kLogic,
  kReg,
};

enum class PackedArrayForm {
  kExplicit,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kInteger,
  kTime,
};

struct PackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto ElementCount() const -> std::uint64_t;
  [[nodiscard]] auto IsAscending() const -> bool;
  [[nodiscard]] auto Contains(std::int64_t index) const -> bool;
  [[nodiscard]] auto LinearOffset(std::int64_t index) const -> std::uint64_t;
};

struct PackedArrayType {
  BitAtom atom;
  Signedness signedness;
  std::vector<PackedRange> dims;
  PackedArrayForm form;

  [[nodiscard]] auto BitWidth() const -> std::uint64_t;
  [[nodiscard]] auto IsFourState() const -> bool;
  [[nodiscard]] auto DefaultInitialValue() const -> IntegralConstant;
};

struct EnumMember {
  std::string name;
  IntegralConstant value;
};

struct EnumType {
  TypeId base_type;
  std::vector<EnumMember> members;
};

// LRM 7.2.1: each field occupies a contiguous bit range within the struct's
// packed bit space. `bit_offset` is the LSB position of the field (slang's
// FieldSymbol::bitOffset); the first-declared field has the highest offset.
struct PackedStructField {
  std::string name;
  TypeId type;
  std::uint64_t bit_offset;
  std::uint64_t bit_width;
};

// LRM 7.2.1: "When a packed structure appears as a primary, it shall be
// treated as a single vector." `base` is that whole-vector projection (width
// = sum of field widths; 4-state iff any field is 4-state); value-level uses
// route through it. `fields` is the per-member offset/width table that
// member-access expressions consult.
struct PackedStructType {
  PackedArrayType base;
  std::vector<PackedStructField> fields;
};

struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;
};

struct UnpackedArrayType {
  TypeId element_type;
  std::vector<UnpackedRange> dims;
};

struct DynamicArrayType {
  TypeId element_type;
};

struct QueueType {
  TypeId element_type;
  std::optional<std::uint64_t> max_bound;
};

struct AssociativeArrayType {
  TypeId element_type;
  std::optional<TypeId> key_type;
};

struct StringType {};
struct EventType {};
struct RealType {};
struct ShortRealType {};
struct RealTimeType {};
struct ChandleType {};
struct VoidType {};

using TypeData = std::variant<
    PackedArrayType, PackedStructType, EnumType, UnpackedArrayType,
    DynamicArrayType, QueueType, AssociativeArrayType, StringType, EventType,
    RealType, ShortRealType, RealTimeType, ChandleType, VoidType>;

struct Type {
  TypeData data;

  [[nodiscard]] auto Kind() const -> TypeKind;
  [[nodiscard]] auto IsPackedArray() const -> bool;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayType&;
  [[nodiscard]] auto IsPackedStruct() const -> bool;
  [[nodiscard]] auto AsPackedStruct() const -> const PackedStructType&;
  [[nodiscard]] auto IsEnum() const -> bool;
  [[nodiscard]] auto AsEnum() const -> const EnumType&;
};

}  // namespace lyra::hir
