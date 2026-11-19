#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

// Whether a bit of a value of `type` can be unknown, so that read for its truth
// or compared it can answer neither true nor false (LRM 11.4.5, 11.4.7).
[[nodiscard]] auto CarriesUnknowns(const CompilationUnit& unit, TypeId type)
    -> bool;

// The one-bit type of a truth or a comparison over values of `operands`, which
// is four-state exactly where one of them carries unknowns.
[[nodiscard]] auto OneBitAnswerType(
    const CompilationUnit& unit, std::span<const TypeId> operands) -> TypeId;

// The types a value of `type` holds values of -- a product's components, a
// union's members, a container's elements.
[[nodiscard]] auto PartTypes(const CompilationUnit& unit, TypeId type)
    -> std::vector<TypeId>;

// How wide a stream is, and whether its bits carry x or z (LRM 11.4.14: a pack
// over any 4-state part yields a 4-state stream).
struct StreamShape {
  std::uint64_t width;
  IntegralStateKind state_kind;
};

// The shape of the stream a value of `type` makes where the type fixes its
// width (LRM 6.24.3), and nothing for a type whose bit count only the running
// program has, or that holds no bit stream at all.
[[nodiscard]] auto FixedStreamShapeOf(const CompilationUnit& unit, TypeId type)
    -> std::optional<StreamShape>;

// The shape of the stream a product of `parts` makes: each part's laid end to
// end, the first most significant.
[[nodiscard]] auto FixedStreamShapeOfParts(
    const CompilationUnit& unit, std::span<const TypeId> parts)
    -> std::optional<StreamShape>;

// Whether a value of `type` makes a bit stream at all (LRM 6.24.3): neither a
// real nor a reference owning nothing -- a chandle, a virtual interface -- is
// a bit-stream type, nor is a value holding one, and a tagged union is
// streamed not at all (LRM 11.4.14.1).
[[nodiscard]] auto HasBitStream(const CompilationUnit& unit, TypeId type)
    -> bool;

// Whether a value of `type` holds a class handle anywhere in it, whose stream
// is the properties of the object it refers to (LRM 11.4.14.1).
[[nodiscard]] auto HoldsClassHandle(const CompilationUnit& unit, TypeId type)
    -> bool;

// Whether a stream is read back into a value of `type`: an associative array
// is illegal as a destination wherever in it one stands (LRM 6.24.3), and the
// object a class handle refers to is not yet read back (LRM 11.4.14.3).
[[nodiscard]] auto IsStreamDestination(const CompilationUnit& unit, TypeId type)
    -> bool;

// Whether the net types' tables resolve a net of `type` position by position
// (LRM 6.6.1, 6.6.3, 6.6.7). They are tables over 4-state bits, and a net
// holds its drivers' contributions as 4-state positions whatever its type,
// showing the result at its type, where a 2-state bit takes x and z as 0 (LRM
// 6.11.2). So a net resolves where every bit it has is a bit of an integral
// value: an integral type of either state, and a fixed-size unpacked array,
// structure or untagged union of such. A net of any other type -- one holding
// a real, a string or a handle, which have no bits a table reads -- has no
// resolution function and is unresolved (LRM 6.6.7).
[[nodiscard]] auto NetResolves(const CompilationUnit& unit, TypeId type)
    -> bool;

// Whether every bit of a value of `type` is a fixed one: an integral value's,
// a real's IEEE 754 bits (LRM 6.12), and a structure's, a union's, a tuple's or
// a fixed-size array's made of such. A member of an untagged union is a view
// of positions of these (LRM 7.3); a string, a handle and a container that
// grows have none.
[[nodiscard]] auto HasPositions(const CompilationUnit& unit, TypeId type)
    -> bool;

// How many positions a value of `type` has, which a procedural continuous
// assignment may hold some of (LRM 10.6.2) and a net resolves each of (LRM
// 6.6.7): an integral value's bits; a real's IEEE 754 bits (LRM 6.12); a
// product's components' and a fixed-size unpacked array's elements' positions
// one part after another, the first part's first; an untagged union's
// storage's, which each member is a view of from the first (LRM 7.3); a tagged
// union's widest member's and then its tag's (LRM 7.3.2); and one for any
// other value, which is held whole.
[[nodiscard]] auto PositionCountOf(const CompilationUnit& unit, TypeId type)
    -> std::uint64_t;

// How many positions the widest of `members` has, which every member of a
// union lies within from the first (LRM 7.3, 7.3.2).
[[nodiscard]] auto WidestPositionCountOf(
    const CompilationUnit& unit, std::span<const TypeId> members)
    -> std::uint64_t;

// How many positions the tag of a tagged union of `members` members takes:
// the bits that count them (LRM 7.3.2).
[[nodiscard]] auto TagPositionCountOf(std::size_t members) -> std::uint64_t;

// The integral type holding every position of a value of `type`, the first
// least significant: what a driver of a part of an unpacked type gives, and
// what a type's positions operations take and answer.
[[nodiscard]] auto PositionsTypeOf(const CompilationUnit& unit, TypeId type)
    -> TypeId;

// The one-bit type `==` and `!=` answer over two values of `type`, which can
// be unknown exactly where what they compare can hold an unknown: every member
// of a struct or a tagged union, every element of an array, the first member
// of an untagged union.
[[nodiscard]] auto EqualityAnswerType(const CompilationUnit& unit, TypeId type)
    -> TypeId;

}  // namespace lyra::mir
