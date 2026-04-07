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

// ABI record for a single ref/const-ref binding in a deferred assertion.
// Populated at enqueue time (codegen), stored in DeferredAssertionRecord,
// consumed by the thunk at drain time. Runtime never interprets entries.
// Minimal: only the address is needed. Constness and type are known
// statically by the thunk (from the target signature).
struct DeferredAssertionRefBindingAbi {
  void* addr;
};

}  // namespace lyra
