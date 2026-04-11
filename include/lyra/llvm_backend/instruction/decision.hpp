#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct CuFacts;
class SlotAccessResolver;

// Decision observation lowering requires an explicit decision_owner_id.
// The caller must pass a non-null value obtained from the function's
// active decision owner context. This ensures decision lowering cannot
// accidentally read ambient nullable state.

auto LowerRecordDecisionObservation(
    Context& ctx, const CuFacts& facts, llvm::Value* decision_owner_id,
    const mir::RecordDecisionObservation& obs) -> Result<void>;

auto LowerRecordDecisionObservationDynamic(
    Context& ctx, const CuFacts& facts, llvm::Value* decision_owner_id,
    SlotAccessResolver& resolver,
    const mir::RecordDecisionObservationDynamic& obs) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
