#pragma once

#include <cstdint>

namespace lyra::base {

// The value a wrapper naming a slot carries before anything has assigned it
// one. Both kinds of wrapper need it: an identity is "position N in the pool
// that minted it", and a structural index is "position N in the aggregate's own
// member order", so in each case every position that exists is meaningful and
// none of them is free to stand for "none yet". This is the value outside every
// such range: reading through it fails the bounds check the container already
// performs, so a wrapper that was declared and never assigned reports itself
// instead of silently naming the first entry.
//
// It is the value a default-constructed one holds, not a state a program may
// pass around: something that may legitimately be absent is a `std::optional`,
// which says so in the signature. Every wrapper of this shape carries it as a
// default member initializer, so a partly written aggregate is caught rather
// than read.
//
// A count is a different shape and does not carry it. A hop count's zero is a
// meaningful answer -- no hops, the reader's own scope -- so it defaults to
// that, and it has no value outside its range to spare for "none yet".
inline constexpr std::uint32_t kUnassignedId = ~std::uint32_t{0};

}  // namespace lyra::base
