#pragma once

#include <cstdint>

namespace lyra::hir {

// The two hooks `randomize()` runs around itself, which every class has
// whatever it declares and which a class may declare its own of, taking the
// place of the one it would otherwise have (LRM 18.6.2).
enum class RandomizationHook : std::uint8_t {
  kPreRandomize,
  kPostRandomize,
};

}  // namespace lyra::hir
