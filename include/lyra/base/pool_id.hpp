#pragma once

#include <cstdint>

namespace lyra::base {

// The value an identity carries before a pool has given it one. An identity
// means "position N in the pool that minted it", so every position a pool holds
// is meaningful and none of them can stand for "none yet". This is the value
// outside every pool: reading through it fails the bounds check each pool
// already performs, so an identity that was declared and never assigned reports
// itself instead of silently naming the pool's first entry.
//
// It is the value a default-constructed identity holds, not a state a program
// may pass around: an identity that may legitimately be absent is a
// `std::optional`, which says so in the signature.
inline constexpr std::uint32_t kUnassignedId = ~std::uint32_t{0};

}  // namespace lyra::base
