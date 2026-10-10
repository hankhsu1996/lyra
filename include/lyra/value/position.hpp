#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>

#include "lyra/value/integral.hpp"

namespace lyra::value {

// LRM 7.4.5 / 7.10.1: the element the position `at` names -- the number a
// position value the program computed holds, none where it holds x or z -- in
// a container holding `size` elements, or none where it names no element
// there, which is the read-default / write-discard path. Every unpacked family
// reaches an element through this, so none of them can disagree about which
// exist.
[[nodiscard]] constexpr auto ElementOrdinal(
    std::optional<std::int64_t> at, std::size_t size)
    -> std::optional<std::size_t> {
  if (!at || *at < 0 || static_cast<std::uint64_t>(*at) >= size) {
    return std::nullopt;
  }
  return static_cast<std::size_t>(*at);
}

template <IntegralValue P>
[[nodiscard]] constexpr auto ElementOrdinal(const P& position, std::size_t size)
    -> std::optional<std::size_t> {
  return ElementOrdinal(ReadPosition(position), size);
}

}  // namespace lyra::value
