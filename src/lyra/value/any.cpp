#include "lyra/value/any.hpp"

namespace lyra::value {

auto Any::IsBitIdentical(const Any& other) const -> bool {
  return Identity() == other.Identity() && held_.IsBitIdentical(other.held_);
}

}  // namespace lyra::value
