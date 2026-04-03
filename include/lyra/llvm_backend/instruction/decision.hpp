#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
class SlotAccessResolver;

auto LowerRecordDecisionObservation(
    Context& ctx, const mir::RecordDecisionObservation& obs) -> Result<void>;

auto LowerRecordDecisionObservationDynamic(
    Context& ctx, SlotAccessResolver& resolver,
    const mir::RecordDecisionObservationDynamic& obs) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
