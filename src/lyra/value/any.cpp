#include "lyra/value/any.hpp"

#include "lyra/value/integral_words.hpp"

namespace lyra::value {

auto Any::operator==(const Any& other) const -> FourStateBit {
  if (Identity() != other.Identity()) {
    return FourStateBitOf(false, false);
  }
  if (HeldValue() == nullptr || other.HeldValue() == nullptr) {
    return FourStateBitOf(HeldValue() == other.HeldValue(), false);
  }
  return held_ == other.held_;
}

auto Any::operator!=(const Any& other) const -> FourStateBit {
  return Inverted(*this == other);
}

auto Any::IsBitIdentical(const Any& other) const -> bool {
  return Identity() == other.Identity() && held_.IsBitIdentical(other.held_);
}

auto Any::HasUnknown() const -> bool {
  return held_.HasUnknown();
}

}  // namespace lyra::value
