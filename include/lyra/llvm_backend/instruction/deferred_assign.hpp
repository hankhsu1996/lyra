#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower deferred assignment (NBA region).
// Schedules the write for the NBA region and handles notification.
// Supports bit-range, index projection, and simple full-width writes.
//
// Classification is done via MIR TypeOfPlace (not LLVM type shape) for
// correct-by-construction codegen. Three store kinds are supported:
// - kScalar2State: 2-state packed scalar (integer mask)
// - kScalar4State: 4-state packed scalar ({val, unk} struct mask)
// - kAggregateBytes: Aggregate types (byte-level 0xFF mask via runtime)
auto LowerDeferredAssign(Context& context, const mir::DeferredAssign& deferred)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
