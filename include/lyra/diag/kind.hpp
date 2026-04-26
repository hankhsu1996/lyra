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

// Set iff PrimaryDiagItem::kind == kUnsupported.
enum class UnsupportedCategory : std::uint8_t {
  kType,
  kOperation,
  kFeature,
};

}  // namespace lyra::diag
