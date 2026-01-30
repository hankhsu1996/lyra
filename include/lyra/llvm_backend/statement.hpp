#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a single MIR statement to LLVM IR
auto LowerStatement(Context& context, const mir::Statement& statement)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
