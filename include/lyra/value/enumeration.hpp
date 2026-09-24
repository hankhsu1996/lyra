#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/packed_type.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// The members an enumeration declares, in declared order, and the questions
// the language asks of a value against them (LRM 6.19.5, 6.24.2). Each member
// is its whole value at the base type -- a 4-state base admits members holding
// x or z bits, and a base may be wider than a machine word -- so a value is a
// member when it is bit-identical to one, unknown bits included.
//
// It is built from its members' word planes because that is the form a compiler
// states constants in: member after member, each member's value plane and then,
// over a 4-state base, its unknown plane. Each member is assembled once here
// and every question afterwards compares whole values.
class Enumeration {
 public:
  Enumeration(
      const PackedType& base, std::span<const std::uint64_t> planes,
      std::span<const char* const> names);

  // LRM 6.24.2: whether a value is one of the declared members.
  [[nodiscard]] auto Has(const PackedArray& value) const -> bool;

  // LRM 6.19.5.6: the member's name, or the empty string for a value that is
  // no member.
  [[nodiscard]] auto Name(const PackedArray& value) const -> String;

  // LRM 6.19.5.3 / 6.19.5.4: the member `count` places after or before the
  // value in declared order, wrapping at either end, or the base type's default
  // (Table 6-7) for a value that is no member. `count` is the `int unsigned`
  // the source passed.
  [[nodiscard]] auto Next(
      const PackedArray& value, const PackedArray& count) const -> PackedArray;
  [[nodiscard]] auto Prev(
      const PackedArray& value, const PackedArray& count) const -> PackedArray;

 private:
  [[nodiscard]] auto PositionOf(const PackedArray& value) const
      -> std::optional<std::size_t>;
  [[nodiscard]] auto StepCount(const PackedArray& count) const -> std::size_t;

  PackedType base_;
  std::vector<PackedArray> members_;
  std::vector<std::string> names_;
};

}  // namespace lyra::value
