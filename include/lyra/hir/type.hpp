#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/constant_value.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

enum class TypeKind {
  kScalarBit,
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
  kClassHandle,
  kNull,
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

// A single bit, the terminal of every integral type. `logic`, `bit`, and `reg`
// are this leaf; so is the element a packed array bottoms out at (LRM 7.4.1).
// The atom carries the bit's 2-state (`bit`) versus 4-state (`logic` / `reg`)
// nature.
struct ScalarBitType {
  BitAtom atom;
};

// LRM 7.4.1: a packed array is one declared dimension over an element type,
// "recursively other packed arrays and packed structures." One node per
// dimension; the element is named by its `TypeId`, so an array of a packed
// aggregate carries that aggregate's identity. Multi-dim nests via
// `element_type`. Signedness is the whole-vector property (the outermost node
// is authoritative; elements are unsigned unless of a named signed type).
struct PackedArrayType {
  PackedRange dim;
  TypeId element_type;
  Signedness signedness;
  PackedArrayForm form;
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

// LRM 7.2.1: a packed struct is a heterogeneous set of bit-fields packed into
// one vector. `fields` is the per-member offset/width table member-access
// consults; `signedness` and `four_state` are the integral attributes the
// struct carries as a whole (4-state iff any field is, LRM 7.2.1). The
// single-vector projection -- total width is the sum of the field widths -- is
// computed at HIR-to-MIR, not stored.
struct PackedStructType {
  std::vector<PackedAggregateField> fields;
  Signedness signedness;
  bool four_state;
};

// LRM 7.3.1 untagged packed union. Members overlap at the LSBs (each field's
// `bit_offset` is 0); for hard packed unions every member equals the union
// width, for soft packed unions a narrower member's bits sit at the LSBs.
// `signedness` and `four_state` are the union's integral attributes. The
// single-vector projection -- total width is the widest member -- is computed
// at HIR-to-MIR. Tagged unions are a separate future feature (runtime tag-bit
// logic, LRM 11.9).
struct PackedUnionType {
  std::vector<PackedAggregateField> fields;
  Signedness signedness;
  bool four_state;
};

// A named member of an unpacked aggregate (struct or union). Unlike a packed
// aggregate field, an unpacked member has its own independent storage of its
// declared type -- there is no shared bit vector, so no offset or width. The
// member is identified by its position in declaration order (LRM 7.2 / 7.3),
// the same index a member-access expression carries. `default_init` holds the
// member's own declaration initializer (LRM 7.2.2) as a folded constant value
// -- type metadata, like an enum member's value -- which takes precedence over
// the member type's Table 7-1 default when the enclosing struct is
// default-constructed; absent when the member has no initializer.
struct UnpackedAggregateField {
  std::string name;
  TypeId type;
  std::optional<ConstantValue> default_init;
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

// LRM 8.3 class handle: the type of a variable that refers to a class object.
// Null is a legal value, the object is reached through the handle, and the
// referenced class is named by its declaration id (resolved through the unit's
// class registry).
struct ClassHandleType {
  ClassId class_id;
};

// LRM 8.4: the type slang gives the `null` literal. It is assignment- and
// comparison-compatible with any class handle; the contextual handle determines
// the operation, so this type carries no class identity of its own.
struct NullType {};

using TypeData = std::variant<
    ScalarBitType, PackedArrayType, PackedStructType, PackedUnionType, EnumType,
    UnpackedStructType, UnpackedUnionType, UnpackedArrayType, DynamicArrayType,
    QueueType, AssociativeArrayType, WildcardIndexType, StringType, EventType,
    RealType, ShortRealType, RealTimeType, ChandleType, ClassHandleType,
    NullType, VoidType>;

struct Type {
  TypeData data;

  [[nodiscard]] auto Kind() const -> TypeKind;
  [[nodiscard]] auto IsScalarBit() const -> bool;
  [[nodiscard]] auto AsScalarBit() const -> const ScalarBitType&;
  [[nodiscard]] auto IsPackedArray() const -> bool;
  [[nodiscard]] auto AsPackedArray() const -> const PackedArrayType&;

  // A single bit or a packed array of bits -- the integral operands an edge
  // event, a bit / part select, and an unpacked-array element read accept.
  [[nodiscard]] auto IsBitVector() const -> bool;
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
