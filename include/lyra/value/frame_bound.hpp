#pragma once

#include <cstddef>

namespace lyra::value {

// The most bytes a value may be and still be kept where it is used rather than
// in storage of its own. A frame is taken from a stack whose size no design
// chose, so a frame keeping a value whose size followed its declared width
// would follow that width too; a larger value is kept apart, through a box the
// program states, and a holder keeps one apart likewise.
inline constexpr std::size_t kMaxInlineBytes = 1024;

}  // namespace lyra::value
