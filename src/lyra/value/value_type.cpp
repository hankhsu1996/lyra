#include "lyra/value/value_type.hpp"

namespace lyra::value {

ValueType::~ValueType() = default;

auto ValueType::SizeStatedAt() const -> StatedAt {
  return StatedAtIn(this, size_);
}

auto ValueType::AlignStatedAt() const -> StatedAt {
  return StatedAtIn(this, align_);
}

}  // namespace lyra::value
