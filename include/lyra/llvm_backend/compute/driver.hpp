#pragma once

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Apply width mask to a ComputeResult.
// Masks both value AND unknown planes to semantic width.
auto ApplyWidthMaskToResult(
    Context& context, const ComputeResult& result, uint32_t semantic_width)
    -> ComputeResult;

// Finalize a compute result by storing to target.
// Handles width masking, 4-state packing, and store.
// struct_type is required for 4-state targets (pass nullptr for 2-state).
auto FinalizeCompute(
    Context& context, mir::PlaceId target, const ComputeResult& result,
    uint32_t semantic_width, llvm::StructType* struct_type) -> Result<void>;

// Lower packed core rvalues (binary, unary, concat) with unified dispatch.
// Routes to 2-state or 4-state implementations based on target stateness.
auto LowerPackedCoreRvalue(Context& context, const mir::Compute& compute)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
