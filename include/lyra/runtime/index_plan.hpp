#pragma once

#include <cstdint>
#include <span>

#include "lyra/common/index_plan.hpp"

namespace lyra::runtime {

class SlotMetaRegistry;

// Evaluate a plan against design state. Returns the computed index value.
// Sets *should_deactivate to true and returns 0 if any kReadSlot encounters
// X/Z bits or if a division/modulo by zero occurs.
auto EvaluateIndexPlan(
    const void* design_state_base, const SlotMetaRegistry& registry,
    std::span<const IndexPlanOp> plan, bool* should_deactivate) -> int64_t;

}  // namespace lyra::runtime
