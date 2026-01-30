#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate aggregate rvalue (unpacked array, struct, or queue literal).
// Does NOT store to any place - caller must handle storage.
// For queue literals, returns a new handle (ownership transferred to caller).
// Aggregates are always 2-state at the aggregate level (unknown is nullptr).
auto LowerAggregateRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type,
    const mir::AggregateRvalueInfo& info) -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
