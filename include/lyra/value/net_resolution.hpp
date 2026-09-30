#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"

namespace lyra::value {

// The truth table a net folds two contributions of equal strength under (LRM
// 6.6, 28.12.4). The net's declared net type picks it, and this is where the
// per-bit fold is realized: `wire` / `tri` resolve tri-state (LRM 6.6.1 Table
// 6-2), `wand` / `triand` wired-and and `wor` / `trior` wired-or (LRM 6.6.3,
// Tables 6-3 and 6-4). All three share high-impedance as the identity, so it is
// not a fold of its own -- it is what a contribution says where it drives
// nothing.
enum class NetResolution : std::uint8_t { kTriState, kWiredAnd, kWiredOr };

// Two contributions folded under the table `fold` names. A net holds which
// table it resolves under as its own state, installed with its net type, while
// a value answers each table as an operation of its own, so this is where the
// one selects the other.
template <typename T>
[[nodiscard]] auto ResolvedUnder(NetResolution fold, const T& lhs, const T& rhs)
    -> T {
  switch (fold) {
    case NetResolution::kTriState:
      return lhs.ResolveTriState(rhs);
    case NetResolution::kWiredAnd:
      return lhs.ResolveWiredAnd(rhs);
    case NetResolution::kWiredOr:
      return lhs.ResolveWiredOr(rhs);
  }
  throw InternalError("ResolvedUnder: unknown net resolution");
}

}  // namespace lyra::value
