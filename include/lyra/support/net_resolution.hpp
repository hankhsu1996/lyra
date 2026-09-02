#pragma once

#include <cstdint>

namespace lyra::support {

// How a net folds its drivers' contributions into its value (LRM 6.6). The
// source net type picks it: `wire` and `tri` name the same tri-state fold, and
// the wired-logic, charge-storage, pull, and supply net types each name their
// own. Two nets of one data type resolve differently when their net types
// differ, so nothing below can recover the fold from the value type or invent
// it -- it is carried down and named here.
//
// Two sides name it, which is why it lives beside them rather than in either. A
// backend classifies a net's type into a fold; the runtime realizes storage
// that resolves under it.
enum class NetResolution : std::uint8_t { kTriState };

}  // namespace lyra::support
