#pragma once

#include <cstdint>

namespace lyra::hir {

// The methods LRM 6.19.5 defines on an enumerated type. An enumeration's
// declared members answer every one of them: `first` / `last` / `num` are
// constants of that member table, and `name` / `next` / `prev` are a search
// through it. No runtime library carries out any of them, so their identity
// belongs to the layer that still holds the declaration and to no layer below
// it.
enum class EnumMethod : std::uint8_t {
  kFirst,
  kLast,
  kNum,
  kName,
  kNext,
  kPrev,
};

}  // namespace lyra::hir
