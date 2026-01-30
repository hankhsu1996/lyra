#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate aggregate rvalue (unpacked array, struct, or queue literal).
// Does NOT store to any place - caller must handle storage.
// For queue literals, returns a new handle (ownership transferred to caller).
auto LowerAggregateRvalue(
    Context& context, const mir::Compute& compute,
    const mir::AggregateRvalueInfo& info) -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
