#pragma once

#include <cstdint>
#include <span>

#include "lyra/common/index_plan.hpp"

namespace lyra::runtime {

class SlotMetaRegistry;

// Evaluate a plan against design state. Returns the computed index value.
// Sets *has_xz to true and returns 0 if any kReadSlot encounters X/Z bits.
auto EvaluateIndexPlan(
    const void* design_state_base, const SlotMetaRegistry& registry,
    std::span<const IndexPlanOp> plan, bool* has_xz) -> int64_t;

}  // namespace lyra::runtime
