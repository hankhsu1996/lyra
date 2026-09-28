#pragma once

#include <cstdint>

namespace lyra::diag {

enum class DiagKind : std::uint8_t {
  kError,
  kUnsupported,
  kHostError,
  kWarning,
  kNote,
  // Something the compiler could have done better and did not. The program is
  // right either way, so it reaches the terminal only when asked for.
  kRemark,
};

}  // namespace lyra::diag
