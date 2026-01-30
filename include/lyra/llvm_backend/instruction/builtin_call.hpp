#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower container-mutating builtin call.
//
// Handles operations that mutate a receiver container:
// - kArrayDelete: clear dynamic array contents
// - kQueueDelete: clear queue (destroy and null handle)
// - kQueueDeleteAt: remove element at index
// - kQueuePushBack, kQueuePushFront: add element
// - kQueuePopBack, kQueuePopFront: remove and return element
// - kQueueInsert: insert element at index
auto LowerBuiltinCall(Context& context, const mir::BuiltinCall& call)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
