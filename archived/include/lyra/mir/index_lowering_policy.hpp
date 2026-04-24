#pragma once

namespace lyra::mir {

// Minimum width (in bits) for index-vs-bounds comparisons.
// Backends implementing IndexInRange must widen index and bounds to at least
// this width before signed comparison. This avoids misleading narrow-integer
// IR and covers the common case where SV indices are >= 32 bits.
inline constexpr unsigned kMinIndexCompareBits = 32;

}  // namespace lyra::mir
