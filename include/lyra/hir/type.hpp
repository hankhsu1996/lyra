#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/interner.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/constant_value.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/imported_runtime_class.hpp"

namespace lyra::hir {

// The two single-bit types. LRM 6.11.2 gives `logic` and `reg` as names for
// one 4-state type and prefers `logic`, so a source that says `reg` reaches
// the same type a source saying `logic` does.
enum class BitAtom {
  kBit,
  kLogic,
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

  [[nodiscard]] auto ElementCount() const -> std::uint64_t;

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

// A type one HIR compilation unit names, and the vocabulary for asking what it
// is. The alternatives are a closed set, consumed by visiting them: a visitor
// that names each one rather than defaulting is what makes an alternative added
// here fail to compile until every consumer says what the new one means.
//
// A question belongs to the type when its answer spans several alternatives or
// states a rule over them. Testing for one alternative, or reaching into it,
// stays at the call site; a question about how a later stage will treat a type
// belongs to that stage.
class Type {
 private:
  using Data = std::variant<
      ScalarBitType, PackedArrayType, PackedStructType, PackedUnionType,
      EnumType, UnpackedStructType, UnpackedUnionType, UnpackedArrayType,
      DynamicArrayType, QueueType, AssociativeArrayType, WildcardIndexType,
      StringType, EventType, RealType, ShortRealType, RealTimeType, ChandleType,
      ClassHandleType, ImportedClassHandleType, UnitObjectType, NullType,
      VoidType>;

 public:
  explicit Type(Data data) : data_(std::move(data)) {
  }

  // A single bit or a packed array of bits -- the integral operands an edge
  // event, a bit / part select, and an unpacked-array element read accept.
  [[nodiscard]] auto IsBitVector() const -> bool;

  // True for the value types -- those a value-change event can react to (LRM
  // 9.4.2). A handle / event / void is not a value and drives no such event.
  [[nodiscard]] auto IsValueChangeObservable() const -> bool;

  template <typename T>
  [[nodiscard]] auto Is() const -> bool {
    return std::holds_alternative<T>(data_);
  }

  // Null where the type is a different alternative, for a caller whose next
  // step depends on which one it met.
  template <typename T>
  [[nodiscard]] auto As() const -> const T* {
    return std::get_if<T>(&data_);
  }

  // For a caller that has already established which alternative this is, so
  // meeting another one is that caller's own invariant broken.
  template <typename T>
  [[nodiscard]] auto Get() const -> const T& {
    const T* arm = std::get_if<T>(&data_);
    if (arm == nullptr) {
      throw InternalError(
          "hir::Type::Get: type is not the alternative asked for");
    }
    return *arm;
  }

  template <typename Visitor>
  auto Visit(Visitor&& visitor) const -> decltype(auto) {
    return std::visit(std::forward<Visitor>(visitor), data_);
  }

  auto operator==(const Type&) const -> bool = default;

  // How a pool spreads the types it holds. It reads the type's alternative and
  // the identities that alternative names rather than walking a member list,
  // because equality decides the answer and a hash only has to separate the
  // types one unit actually holds.
  struct Hash {
    auto operator()(const Type& type) const -> std::size_t;
  };

 private:
  Data data_;
};

// The types one compilation unit names. A type's identity here is its
// structure, so two declarations spelling the same type are one entry however
// each was reached -- read off the frontend, composed by the lowering, or taken
// out of another unit's signature. That is what makes a `TypeId` mean a type
// rather than a place where one happened to be written, and it is what keeps a
// type arriving from outside from becoming a second copy of one already here.
//
// Identity is structural and not the frontend's: a pool outlives the frontend
// object that fed it and belongs to one unit alone, so nothing in it may rest
// on a table shared with another unit.
using TypePool = base::Interner<Type, TypeId, Type::Hash>;

}  // namespace lyra::hir
