#ifndef LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP
#define LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP

#include <llvm/IR/Value.h>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate system task/function rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
auto LowerSystemTfRvalueValue(
    Context& context, const mir::Compute& compute,
    const mir::SystemTfRvalueInfo& info) -> Result<llvm::Value*>;

auto LowerSystemTfEffect(Context& context, const mir::SystemTfEffect& effect)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm

#endif  // LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP
