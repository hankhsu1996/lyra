#pragma once

#include <cstdint>

namespace lyra::value {

// The truth table a net folds two driver contributions under (LRM 6.6). The
// net's declared net type picks it and it is carried down to here, where the
// per-bit fold is realized: `wire` / `tri` resolve tri-state (LRM 6.6.1 Table
// 6-2), `wand` / `triand` wired-and and `wor` / `trior` wired-or (LRM 6.6.3,
// Tables 6-3 and 6-4). All three share high-impedance as the identity, so it is
// not a fold of its own -- it is the value of a position no driver drives.
enum class NetResolution : std::uint8_t { kTriState, kWiredAnd, kWiredOr };

}  // namespace lyra::value
