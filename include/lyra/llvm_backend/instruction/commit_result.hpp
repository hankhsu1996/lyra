#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct CuFacts;
class SlotAccessResolver;

// Commit a runtime result value to a destination via the write architecture.
// Routes through RouteWriteTarget, splits plain vs lifecycle.
// All runtime results use kMove (ownership transfers from runtime/staging).
auto CommitRuntimeResult(
    Context& ctx, const CuFacts& facts, SlotAccessResolver& resolver,
    mir::PlaceId dest, llvm::Value* raw_value) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
