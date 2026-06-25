#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

enum class TypeKind {
  kPackedArray,
  kPackedStruct,
  kPackedUnion,
  kEnum,
  kUnpackedStruct,
  kUnpackedUnion,
  kUnpackedArray,
  kDynamicArray,
  kQueue,
  kAssociativeArray,
  kWildcardIndex,
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

// A named bit-range member of a packed aggregate (struct or union). For a
// packed struct each field has its own contiguous slot computed by slang
// (`FieldSymbol::bitOffset`); for an untagged packed union every field has
// `bit_offset = 0` and `bit_width = member's own width` (LRM 7.3.1: members
// are right-justified to the LSBs).
struct PackedAggregateField {
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
  std::vector<PackedAggregateField> fields;
};

// LRM 7.3.1 untagged packed union. `base` is the "single vector" projection
// (width = max member width; 4-state iff any member is 4-state). Members
// overlap at the LSBs; for hard packed unions every member equals the union
// width, for soft packed unions a narrower member's bits sit at the LSBs and
// MSBs beyond the member are preserved across writes. Tagged unions are a
// separate future feature (require runtime tag-bit logic, LRM 11.9).
struct PackedUnionType {
  PackedArrayType base;
  std::vector<PackedAggregateField> fields;
};

// A named member of an unpacked aggregate (struct or union). Unlike a packed
// aggregate field, an unpacked member has its own independent storage of its
// declared type -- there is no shared bit vector, so no offset or width. The
// member is identified by its position in declaration order (LRM 7.2 / 7.3),
// the same index a member-access expression carries.
struct UnpackedAggregateField {
  std::string name;
  TypeId type;
};

// LRM 7.2 unpacked structure: a heterogeneous aggregate whose members each hold
// independent storage of their declared type. Distinct from a packed struct,
// whose members share one bit vector; an unpacked member may be any type,
// including a string, another unpacked aggregate, or a variable-size container.
struct UnpackedStructType {
  std::vector<UnpackedAggregateField> fields;
};

// LRM 7.3 unpacked union: one storage shared across the member types, with one
// member usable at a time. `tagged` (LRM 7.3.2) marks a type-checked union that
// carries a tag identifying the active member; an untagged union (the default)
// is the type-loophole form with no tag. A tagged union may declare a `void`
// member (LRM 7.3.2) when all information is in the tag.
struct UnpackedUnionType {
  std::vector<UnpackedAggregateField> fields;
  bool tagged;
};

struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;
};

struct UnpackedArrayType {
  TypeId element_type;
  UnpackedRange dim;
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
  TypeId key_type;
};

// LRM 7.8.1 wildcard index type (`[*]`): the key type of an associative array
// indexed by any integral value, identified by magnitude regardless of the
// index expression's width. It declares no member structure of its own; it is
// the type that distinguishes a wildcard-keyed array from a string- or
// integral-keyed one.
struct WildcardIndexType {};

struct StringType {};
struct EventType {};
struct RealType {};
struct ShortRealType {};
struct RealTimeType {};
struct ChandleType {};
struct VoidType {};

using TypeData = std::variant<
    PackedArrayType, PackedStructType, PackedUnionType, EnumType,
    UnpackedStructType, UnpackedUnionType, UnpackedArrayType, DynamicArrayType,
    QueueType, AssociativeArrayType, WildcardIndexType, StringType, EventType,
    RealType, ShortRealType, RealTimeType, ChandleType, VoidType>;

struct Type {
  TypeData data;

  // The declaration site that first interned this type. A downstream layer
  // that cannot represent the type anchors its diagnostic here. Builtin types
  // have no source and carry `UnknownSpan`.
  diag::DiagSpan span;

  [[nodiscard]] auto Kind() const -> TypeKind;
  [[nodiscard]] auto IsPackedArray() const -> bool;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayType&;
  [[nodiscard]] auto IsPackedStruct() const -> bool;
  [[nodiscard]] auto AsPackedStruct() const -> const PackedStructType&;
  [[nodiscard]] auto IsPackedUnion() const -> bool;
  [[nodiscard]] auto AsPackedUnion() const -> const PackedUnionType&;
  [[nodiscard]] auto IsEnum() const -> bool;
  [[nodiscard]] auto AsEnum() const -> const EnumType&;

  // True for the value types -- those a value-change event can react to (LRM
  // 9.4.2). A handle / event / void is not a value and drives no such event.
  [[nodiscard]] auto IsValueChangeObservable() const -> bool;
};

}  // namespace lyra::hir
