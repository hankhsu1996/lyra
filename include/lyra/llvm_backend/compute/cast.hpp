#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Cast lowering - returns computed value with unknown plane for 4-state.
auto LowerCastRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId destination_type)
    -> Result<RvalueValue>;

// Bitcast lowering - returns computed value with unknown plane for 4-state.
auto LowerBitCastRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId destination_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
