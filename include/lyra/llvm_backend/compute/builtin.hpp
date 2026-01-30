#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate builtin rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
// Only handles pure builtins (kNewArray, kArraySize, kQueueSize, kEnum*).
// Container-mutating builtins are handled via BuiltinCall instruction.
auto LowerBuiltinRvalue(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
