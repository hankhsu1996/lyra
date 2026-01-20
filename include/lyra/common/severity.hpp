#pragma once

#include <cstdint>

namespace lyra {

// Severity levels for $info, $warning, $error system tasks.
// $fatal is handled separately as a terminating statement.
enum class Severity : uint8_t {
  kInfo,
  kWarning,
  kError,
};

}  // namespace lyra
