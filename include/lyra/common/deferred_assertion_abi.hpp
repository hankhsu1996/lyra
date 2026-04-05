#pragma once

#include <cstdint>

// Shared ABI types for deferred immediate assertions.
// Used by both MIR/codegen (to emit) and runtime (to interpret).
// These values cross the generated-code -> runtime boundary and must
// stay in sync across both layers.

namespace lyra {

// Disposition of a deferred assertion enqueue record.
// Matches mir::DeferredAssertionDisposition by construction.
enum class DeferredAssertionDispositionAbi : uint8_t {
  kDefaultFailReport = 0,
  kFailAction = 1,
  kPassAction = 2,
  kCoverHit = 3,
};

}  // namespace lyra
