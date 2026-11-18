#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/interner.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/data_object.hpp"

namespace lyra::hir {

struct TypespecId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const TypespecId&) const -> std::strong_ordering = default;
};

// The keyword a predefined integer type is written with (LRM 6.11), which
// fixes its width.
enum class IntegerKeyword : std::uint8_t {
  kInt,
  kShortInt,
  kLongInt,
  kByte,
  kInteger,
  kTime,
};

// A predefined integer type, signed or not as it is declared.
struct IntegerTypespec {
  IntegerKeyword keyword = IntegerKeyword::kInt;
  bool is_signed = false;

  auto operator==(const IntegerTypespec&) const -> bool = default;
};

// The keyword a single bit is written with (LRM 6.11): `bit`, or `logic`,
// which `reg` is too.
enum class BitKeyword : std::uint8_t {
  kBit,
  kLogic,
};

// A single bit, signed or not as it is declared.
struct BitTypespec {
  BitKeyword keyword = BitKeyword::kLogic;
  bool is_signed = false;

  auto operator==(const BitTypespec&) const -> bool = default;
};

// A packed array (LRM 7.4.1): its outermost dimension's range and the
// typespec of one element of that dimension -- the next dimension's, or a
// single bit's for its last (LRM 37.25 detail 8) -- as wide as its elements
// together, and signed or not as it is declared where it stands.
struct PackedArrayTypespec {
  support::Range range;
  TypespecId element;
  bool is_signed = false;

  auto operator==(const PackedArrayTypespec&) const -> bool = default;
};

// A packed structure (LRM 7.2.1): the type, which states its members and its
// width; whether it is signed where it stands, which an element of a packed
// array is not unless it is of a named type declared signed (LRM 7.4.1); and,
// for each member in the order the type states them, the typespec the
// declaration wrote it with.
struct PackedStructTypespec {
  TypeId type;
  bool is_signed = false;
  std::vector<TypespecId> members;

  auto operator==(const PackedStructTypespec&) const -> bool = default;
};

// A packed union (LRM 7.3.1), stated as a packed structure is, its type also
// stating whether it is tagged; and whether it is soft, which only an untagged
// one may be.
struct PackedUnionTypespec {
  TypeId type;
  bool is_signed = false;
  std::vector<TypespecId> members;
  bool soft = false;

  auto operator==(const PackedUnionTypespec&) const -> bool = default;
};

// An enumeration (LRM 6.19): its base type, which is an integral type as wide
// as the enumeration, `int` where the source states none, and the type whose
// members a reader is told.
struct EnumTypespec {
  TypespecId base;
  TypeId enumeration;

  auto operator==(const EnumTypespec&) const -> bool = default;
};

// A type a reader is told nothing of beyond what it is: a real type, an
// event, a string, a chandle or a virtual interface.
enum class PlainKind : std::uint8_t {
  kReal,
  kShortReal,
  kRealtime,
  kEvent,
  kString,
  kChandle,
  kVirtualInterface,
};

struct PlainTypespec {
  PlainKind kind = PlainKind::kReal;

  auto operator==(const PlainTypespec&) const -> bool = default;
};

// A class (LRM 37.32): the class itself, whatever typedef the source names it
// by, and the name it is reported under (detail 4).
struct ClassTypespec {
  std::string name;

  auto operator==(const ClassTypespec&) const -> bool = default;
};

// An unpacked structure or union (LRM 7.2, 7.3): the type, whose declaration
// states its members -- their identifiers, their order, how each is declared
// random, and whether a union is tagged -- and, for each member in that order,
// its typespec, which is how the declaration wrote its type. The typespec is
// this unit's spelling of a type another unit may declare, so it lists its own
// members' typespecs where the type lists the members.
struct UnpackedAggregateTypespec {
  TypeId type;
  std::vector<TypespecId> members;

  auto operator==(const UnpackedAggregateTypespec&) const -> bool = default;
};

// A fixed-size unpacked array (LRM 7.4.2): its range and one element's
// typespec.
struct FixedArrayTypespec {
  support::Range range;
  TypespecId element;

  auto operator==(const FixedArrayTypespec&) const -> bool = default;
};

// A dynamic array (LRM 7.5), whose size the run sets.
struct DynamicArrayTypespec {
  TypespecId element;

  auto operator==(const DynamicArrayTypespec&) const -> bool = default;
};

// A queue (LRM 7.10), whose size the run sets.
struct QueueTypespec {
  TypespecId element;

  auto operator==(const QueueTypespec&) const -> bool = default;
};

// An associative array (LRM 7.8): one element's typespec and its index's, none
// for a wildcard index (LRM 7.8.1).
struct AssociativeArrayTypespec {
  TypespecId element;
  std::optional<TypespecId> index;

  auto operator==(const AssociativeArrayTypespec&) const -> bool = default;
};

// A typedef (LRM 37.25 details 1, 6): its name and the typespec of the type it
// names, which is what it reports, and whether it aliases that typespec, which
// it does where that type is itself named -- by another typedef or by a
// predefined type's keyword.
struct TypedefTypespec {
  std::string name;
  TypespecId names;
  bool aliases = false;

  auto operator==(const TypedefTypespec&) const -> bool = default;
};

using TypespecData = std::variant<
    IntegerTypespec, BitTypespec, PackedArrayTypespec, PackedStructTypespec,
    PackedUnionTypespec, EnumTypespec, PlainTypespec, ClassTypespec,
    UnpackedAggregateTypespec, FixedArrayTypespec, DynamicArrayTypespec,
    QueueTypespec, AssociativeArrayTypespec, TypedefTypespec>;

// A data object's type as the source declared it (LRM 37.25), which a reader
// reaching the design by name reports, apart from the type its values have: an
// `int` and a `bit signed [31:0]` hold the same values and are two typespecs.
// An array, packed or unpacked, is a dimension over the typespec of one
// element of it, so each dimension is one typespec and an element's typespec
// is the next.
struct Typespec {
  TypespecData data;

  auto operator==(const Typespec&) const -> bool = default;

  struct Hash {
    auto operator()(const Typespec& typespec) const -> std::size_t;
  };
};

// The typespecs one compilation unit states, each once.
using TypespecPool = base::Interner<Typespec, TypespecId, Typespec::Hash>;

}  // namespace lyra::hir

template <>
struct std::hash<lyra::hir::TypespecId> {
  auto operator()(lyra::hir::TypespecId id) const noexcept -> std::size_t {
    return std::hash<std::uint32_t>{}(id.value);
  }
};
