#pragma once

#include <cstdint>

namespace lyra::value {

// The truth table a net folds two contributions of equal strength under (LRM
// 6.6, 28.12.4). The net's declared net type picks it, and this is where the
// per-bit fold is realized: `wire` / `tri` resolve tri-state (LRM 6.6.1 Table
// 6-2), `wand` / `triand` wired-and and `wor` / `trior` wired-or (LRM 6.6.3,
// Tables 6-3 and 6-4). All three share high-impedance as the identity, so it is
// not a fold of its own -- it is what a contribution says where it drives
// nothing.
enum class NetResolution : std::uint8_t { kTriState, kWiredAnd, kWiredOr };

}  // namespace lyra::value
