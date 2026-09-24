#include "lyra/value/position.hpp"

#include <cstddef>
#include <cstdint>
#include <optional>

namespace lyra::value {

auto ElementOrdinal(const PackedArray& position, std::size_t size)
    -> std::optional<std::size_t> {
  const std::optional<std::int64_t> at = ReadPosition(position);
  if (!at || *at < 0 || static_cast<std::uint64_t>(*at) >= size) {
    return std::nullopt;
  }
  return static_cast<std::size_t>(*at);
}

}  // namespace lyra::value
