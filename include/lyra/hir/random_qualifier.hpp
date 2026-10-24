#pragma once

#include <cstdint>

namespace lyra::hir {

// How a class property or a structure member is declared random (LRM 18.4):
// not at all, `rand`, or `randc`.
enum class RandomQualifier : std::uint8_t {
  kNone,
  kRand,
  kRandc,
};

// Whether a declaration so qualified is random at all.
[[nodiscard]] constexpr auto IsRandom(RandomQualifier random) -> bool {
  switch (random) {
    case RandomQualifier::kNone:
      return false;
    case RandomQualifier::kRand:
    case RandomQualifier::kRandc:
      return true;
  }
  return false;
}

}  // namespace lyra::hir
