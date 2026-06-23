#pragma once

#include <cstdint>

namespace lyra::support {

// LRM 21.3.1 pre-bound file descriptors. Bit 31 set selects the FD encoding;
// the low bits index the reserved slot. Shared between lowering (which
// materialises these as literals for $display / $strobe stdout sinks) and
// runtime (which dispatches them).
inline constexpr std::int32_t kFdHighBit =
    static_cast<std::int32_t>(static_cast<std::uint32_t>(1) << 31U);
inline constexpr std::int32_t kStdinFd = kFdHighBit | 0;
inline constexpr std::int32_t kStdoutFd = kFdHighBit | 1;
inline constexpr std::int32_t kStderrFd = kFdHighBit | 2;

}  // namespace lyra::support
