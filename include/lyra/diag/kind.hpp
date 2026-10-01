#pragma once

#include <cstdint>

namespace lyra::diag {

enum class DiagKind : std::uint8_t {
  kError,
  kUnsupported,
  kHostError,
  // The compiler failed through a fault of its own. Nothing about the source
  // is wrong, and the reader's next step is to report it.
  kInternalError,
  kWarning,
  kNote,
  // Something the compiler could have done better and did not. The program is
  // right either way, so it reaches the terminal only when asked for.
  kRemark,
};

}  // namespace lyra::diag
