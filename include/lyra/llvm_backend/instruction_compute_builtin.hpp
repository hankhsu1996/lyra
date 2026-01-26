#pragma once

#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Unified builtin handler - dispatches to appropriate builtin based on method.
void LowerBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info);

void LowerDynArrayBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info);

void LowerQueueBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info);

void LowerEnumBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info);

}  // namespace lyra::lowering::mir_to_llvm
