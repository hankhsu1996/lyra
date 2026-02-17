#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Evaluate array query rvalue ($left, $right, $low, $high, $increment, $size,
// $dimensions, $unpacked_dimensions) and return the computed value.
// Result is always i32. Returns FourState when result_type is 4-state.
auto LowerArrayQueryRvalue(
    Context& context, const mir::Rvalue& rvalue,
    const mir::ArrayQueryRvalueInfo& info, TypeId result_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
