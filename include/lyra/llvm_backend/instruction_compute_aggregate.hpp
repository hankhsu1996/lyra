#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower aggregate rvalue (unpacked array, struct, or queue literal).
auto LowerAggregate(
    Context& context, const mir::Compute& compute,
    const mir::AggregateRvalueInfo& info) -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
