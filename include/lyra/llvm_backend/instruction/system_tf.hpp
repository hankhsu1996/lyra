#ifndef LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP
#define LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

// Evaluate system task/function rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
// System functions are always 2-state (unknown is nullptr).
auto LowerSystemTfRvalue(
    Context& context, const mir::Rvalue& rvalue,
    const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue>;

// Resolver-aware overload.
auto LowerSystemTfRvalue(
    Context& context, SlotAccessResolver& resolver, const mir::Rvalue& rvalue,
    const mir::SystemTfRvalueInfo& info) -> Result<RvalueValue>;

auto LowerSystemTfEffect(Context& context, const mir::SystemTfEffect& effect)
    -> Result<void>;

// Resolver-aware overload.
auto LowerSystemTfEffect(
    Context& context, SlotAccessResolver& resolver,
    const mir::SystemTfEffect& effect) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm

#endif  // LYRA_LLVM_BACKEND_INSTRUCTION_SYSTEM_TF_HPP
