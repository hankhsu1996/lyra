#include "lyra/value/enumeration.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/packed_type.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

Enumeration::Enumeration(
    const PackedType& base, std::span<const std::uint64_t> planes,
    std::span<const char* const> names)
    : base_(base) {
  // A 2-state base has no unknown plane, so a member contributes its value
  // plane alone.
  const std::size_t words = (base.bit_width + 63U) / 64U;
  const std::size_t unknown = base.is_four_state ? words : 0U;
  members_.reserve(names.size());
  names_.reserve(names.size());
  for (std::size_t i = 0; i < names.size(); ++i) {
    const std::span<const std::uint64_t> member =
        planes.subspan(i * (words + unknown), words + unknown);
    members_.push_back(
        PackedArray::FromWords(
            member.first(words), member.subspan(words, unknown), base));
    names_.emplace_back(names[i]);
  }
}

auto Enumeration::PositionOf(const PackedArray& value) const
    -> std::optional<std::size_t> {
  for (std::size_t i = 0; i < members_.size(); ++i) {
    if (members_[i].IsBitIdentical(value)) {
      return i;
    }
  }
  return std::nullopt;
}

auto Enumeration::StepCount(const PackedArray& count) const -> std::size_t {
  return static_cast<std::size_t>(
      static_cast<std::uint64_t>(count.ToInt64()) % members_.size());
}

auto Enumeration::Has(const PackedArray& value) const -> bool {
  return PositionOf(value).has_value();
}

auto Enumeration::Name(const PackedArray& value) const -> String {
  const std::optional<std::size_t> at = PositionOf(value);
  return at ? String(names_[*at]) : String();
}

auto Enumeration::Next(const PackedArray& value, const PackedArray& count) const
    -> PackedArray {
  const std::optional<std::size_t> at = PositionOf(value);
  if (!at) {
    return PackedArray(base_);
  }
  return members_[(*at + StepCount(count)) % members_.size()];
}

auto Enumeration::Prev(const PackedArray& value, const PackedArray& count) const
    -> PackedArray {
  const std::optional<std::size_t> at = PositionOf(value);
  if (!at) {
    return PackedArray(base_);
  }
  return members_[(*at + members_.size() - StepCount(count)) % members_.size()];
}

}  // namespace lyra::value
