#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Check if rvalue is a real-typed math operation (unary or binary).
// Does NOT match casts - casts are handled by LowerCastUnified.
auto IsRealMathRvalue(Context& context, const mir::Rvalue& rvalue) -> bool;

// Evaluate a real-typed math rvalue and return the computed value.
// Does NOT store to any place - caller must handle storage.
// Real types are always 2-state (unknown is nullptr).
auto LowerRealRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
