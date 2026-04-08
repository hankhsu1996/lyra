#pragma once

#include <cstdint>
#include <vector>

#include "lyra/mir/capture_payload.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::mir {

// Realization-layer binding: how one formal maps to payload struct or ref
// array.
struct DeferredActualPlan {
  enum class Source : uint8_t { kPayloadField, kLiveRef };
  Source source;
  uint32_t payload_field_index = 0;
  PlaceId ref_place{};
};

// Complete realization plan for a user-call deferred assertion thunk.
// Produced from a finished DeferredUserCallAction during HIR-to-MIR lowering.
// Consumed by LLVM backend for thunk body emission, enqueue codegen, and
// site metadata emission.
//
// Only user-call actions have realizations. Cover-hit and default-report
// dispositions are handled by built-in runtime paths with no thunk.
struct DeferredUserCallRealization {
  FunctionId thunk;
  FunctionId callee;
  CapturePayloadDesc payload;
  std::vector<DeferredActualPlan> actual_plan;
};

}  // namespace lyra::mir
