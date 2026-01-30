#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower user function call.
//
// Handles three calling conventions:
// 1. Void call (no dest): Just emit the call for side effects
// 2. Sret call (managed return type): Destroy existing value at dest,
//    call with out-param (callee writes directly to dest)
// 3. Register return call: Call function, commit result to dest
auto LowerCall(Context& context, const mir::Call& call) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
