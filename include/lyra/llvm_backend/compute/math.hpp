#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Returns true if the rvalue is a math function (IEEE 1800 20.8).
// This includes both real-typed ($ln, $sin, $pow, ...) and integral ($clog2).
auto IsMathRvalue(const mir::Rvalue& rvalue) -> bool;

// Evaluate a math function rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
auto LowerMathRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
