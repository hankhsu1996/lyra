#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate builtin rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
// Only handles pure builtins (kNewArray, kArraySize, kQueueSize, kEnum*).
// Container-mutating builtins are handled via BuiltinCall instruction.
// Builtins are always 2-state (unknown is nullptr).
auto LowerBuiltinRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type,
    const mir::BuiltinCallRvalueInfo& info) -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
