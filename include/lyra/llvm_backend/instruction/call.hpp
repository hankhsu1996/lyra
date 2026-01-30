#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower unified Call instruction.
//
// Dispatches based on callee type:
// - FunctionId: User function call with staging temps
// - SystemTfOpcode: System TF call (e.g., $value$plusargs)
//
// All outputs (return + writebacks) follow staging discipline:
// 1. Results written to tmp places
// 2. tmp values committed to dest places via CommitValue
auto LowerCall(Context& context, const mir::Call& call) -> Result<void>;

// Lower $value$plusargs via unified Call.
// Exposed for testing and internal use.
auto LowerValuePlusargsCall(Context& context, const mir::Call& call)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
