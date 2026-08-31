#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/constant_value.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/imported_runtime_class.hpp"

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
  kImportedClassHandle,
  kUnitObject,
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

  auto operator==(const PackedRange&) const -> bool = default;
};

// A single bit, the terminal of every integral type. `logic`, `bit`, and `reg`
// are this leaf; so is the element a packed array bottoms out at (LRM 7.4.1).
// The atom carries the bit's 2-state (`bit`) versus 4-state (`logic` / `reg`)
// nature.
struct ScalarBitType {
  BitAtom atom;

  auto operator==(const ScalarBitType&) const -> bool = default;
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

  auto operator==(const PackedArrayType&) const -> bool = default;
};

struct EnumMember {
  std::string name;
  IntegralConstant value;

  auto operator==(const EnumMember&) const -> bool = default;
};

struct EnumType {
  TypeId base_type;
  std::vector<EnumMember> members;

  auto operator==(const EnumType&) const -> bool = default;
};

// A named member of a packed aggregate (struct or union), identified by its
// declaration position. Where the member sits in the aggregate's vector and
// how wide it is both follow from the member list and the members' own types,
// so they are projected at HIR-to-MIR rather than carried here -- a stored
// position could disagree with the declaration it came from.
struct PackedAggregateField {
  std::string name;
  TypeId type;

  auto operator==(const PackedAggregateField&) const -> bool = default;
};

// LRM 7.2.1: a packed struct is a heterogeneous set of bit-fields packed into
// one vector, the first member declared occupying the most significant bits.
// `signedness` is declared (`struct packed signed`); the struct's width and
// its 4-state-ness follow from the fields and are projected at HIR-to-MIR.
struct PackedStructType {
  std::vector<PackedAggregateField> fields;
  Signedness signedness;

  auto operator==(const PackedStructType&) const -> bool = default;
};

// LRM 7.3.1 / 7.3.2: a packed union's members overlap at the least significant
// bits. `tagged` is the source keyword: a tagged union additionally carries a
// tag at the most significant bits naming the member it holds, and every
// dot-notation access is checked against it (LRM 11.9), where an untagged
// union lets a member written as another be read back. `signedness` is
// declared; the union's width, its tag width, and its 4-state-ness follow from
// the members and are projected at HIR-to-MIR.
struct PackedUnionType {
  std::vector<PackedAggregateField> fields;
  Signedness signedness;
  bool tagged;

  auto operator==(const PackedUnionType&) const -> bool = default;
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

  auto operator==(const UnpackedAggregateField&) const -> bool = default;
};

// LRM 7.2 unpacked structure: a heterogeneous aggregate whose members each hold
// independent storage of their declared type. Distinct from a packed struct,
// whose members share one bit vector; an unpacked member may be any type,
// including a string, another unpacked aggregate, or a variable-size container.
struct UnpackedStructType {
  std::vector<UnpackedAggregateField> fields;

  auto operator==(const UnpackedStructType&) const -> bool = default;
};

// LRM 7.3 unpacked union: one storage shared across the member types, with one
// member usable at a time. `tagged` (LRM 7.3.2) marks a type-checked union that
// carries a tag identifying the active member; an untagged union (the default)
// is the type-loophole form with no tag. A tagged union may declare a `void`
// member (LRM 7.3.2) when all information is in the tag.
struct UnpackedUnionType {
  std::vector<UnpackedAggregateField> fields;
  bool tagged;

  auto operator==(const UnpackedUnionType&) const -> bool = default;
};

struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;

  auto operator==(const UnpackedRange&) const -> bool = default;
};

struct UnpackedArrayType {
  TypeId element_type;
  UnpackedRange dim;

  auto operator==(const UnpackedArrayType&) const -> bool = default;
};

struct DynamicArrayType {
  TypeId element_type;

  auto operator==(const DynamicArrayType&) const -> bool = default;
};

struct QueueType {
  TypeId element_type;
  std::optional<std::uint64_t> max_bound;

  auto operator==(const QueueType&) const -> bool = default;
};

struct AssociativeArrayType {
  TypeId element_type;
  TypeId key_type;

  auto operator==(const AssociativeArrayType&) const -> bool = default;
};

// LRM 7.8.1 wildcard index type (`[*]`): the key type of an associative array
// indexed by any integral value, identified by magnitude regardless of the
// index expression's width. It declares no member structure of its own; it is
// the type that distinguishes a wildcard-keyed array from a string- or
// integral-keyed one.
// These types carry nothing beyond being themselves, so two are equal exactly
// when they are the same type.
struct WildcardIndexType {
  auto operator==(const WildcardIndexType&) const -> bool = default;
};

struct StringType {
  auto operator==(const StringType&) const -> bool = default;
};
struct EventType {
  auto operator==(const EventType&) const -> bool = default;
};
struct RealType {
  auto operator==(const RealType&) const -> bool = default;
};
struct ShortRealType {
  auto operator==(const ShortRealType&) const -> bool = default;
};
struct RealTimeType {
  auto operator==(const RealTimeType&) const -> bool = default;
};
struct ChandleType {
  auto operator==(const ChandleType&) const -> bool = default;
};
struct VoidType {
  auto operator==(const VoidType&) const -> bool = default;
};

// LRM 8.3 class handle: the type of a variable that refers to a class object.
// Null is a legal value, the object is reached through the handle, and the
// referenced class is named by a `ClassRef`: a local declaration id when the
// class is declared by this unit, or a by-name reference resolved against
// another unit's signature when the class is declared elsewhere.
struct ClassHandleType {
  ClassRef class_ref;

  auto operator==(const ClassHandleType&) const -> bool = default;
};

// LRM 8.3 class handle whose referenced class is an imported runtime-library
// class rather than a unit-declared one. Managed and null-legal like
// ClassHandleType, but the class is named by its library identity, not a unit
// class id.
struct ImportedClassHandleType {
  support::ImportedRuntimeClass klass;

  auto operator==(const ImportedClassHandleType&) const -> bool = default;
};

// The object an instance of another compilation unit is -- what an interface
// port names (LRM 25.3). Named by the declaring unit alone: the object is
// always another unit's, so there is no local form, and the name is what
// identifies it from anywhere, which is what lets the type cross a signature
// unchanged. Only the members that unit published are reachable through it.
struct UnitObjectType {
  std::string unit_name;

  auto operator==(const UnitObjectType&) const -> bool = default;
};

// LRM 8.4: the type slang gives the `null` literal. It is assignment- and
// comparison-compatible with any class handle; the contextual handle determines
// the operation, so this type carries no class identity of its own.
struct NullType {
  auto operator==(const NullType&) const -> bool = default;
};

using TypeData = std::variant<
    ScalarBitType, PackedArrayType, PackedStructType, PackedUnionType, EnumType,
    UnpackedStructType, UnpackedUnionType, UnpackedArrayType, DynamicArrayType,
    QueueType, AssociativeArrayType, WildcardIndexType, StringType, EventType,
    RealType, ShortRealType, RealTimeType, ChandleType, ClassHandleType,
    ImportedClassHandleType, UnitObjectType, NullType, VoidType>;

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
