#include "lyra/value/value_type.hpp"

#include <cstddef>

namespace lyra::value {

namespace {

// How far past `whole` the member at `member` lies.
auto OffsetIn(const void* whole, const void* member) -> std::size_t {
  return static_cast<std::size_t>(
      static_cast<const std::byte*>(member) -
      static_cast<const std::byte*>(whole));
}

}  // namespace

ValueType::~ValueType() = default;

auto ValueType::SizeStatedAt() const -> StatedAt {
  return StatedAt{.offset = OffsetIn(this, &size_), .bytes = sizeof(size_)};
}

auto ValueType::AlignStatedAt() const -> StatedAt {
  return StatedAt{.offset = OffsetIn(this, &align_), .bytes = sizeof(align_)};
}

}  // namespace lyra::value
