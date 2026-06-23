#pragma once

#include <cstdint>

namespace lyra::diag {

enum class DiagKind : std::uint8_t {
  kError,
  kUnsupported,
  kHostError,
  kWarning,
  kNote,
};

}  // namespace lyra::diag
