#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
class SlotAccessResolver;

// Decision observation lowering requires explicit process_id.
// The caller must pass a non-null process_id obtained from the function's
// active process ownership context. This ensures decision lowering cannot
// accidentally read ambient nullable state.

auto LowerRecordDecisionObservation(
    Context& ctx, llvm::Value* process_id,
    const mir::RecordDecisionObservation& obs) -> Result<void>;

auto LowerRecordDecisionObservationDynamic(
    Context& ctx, llvm::Value* process_id, SlotAccessResolver& resolver,
    const mir::RecordDecisionObservationDynamic& obs) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
