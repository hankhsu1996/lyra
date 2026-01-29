#ifndef LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP
#define LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerSystemTfRvalue(
    Context& context, const mir::Compute& compute,
    const mir::SystemTfRvalueInfo& info) -> Result<void>;

auto LowerSystemTfEffect(Context& context, const mir::SystemTfEffect& effect)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm

#endif  // LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP
