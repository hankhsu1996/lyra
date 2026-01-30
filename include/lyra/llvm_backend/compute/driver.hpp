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

// Finalize a compute result by masking and packing.
// Handles width masking and 4-state packing (but does NOT store).
// struct_type is required for 4-state targets (pass nullptr for 2-state).
// Returns the final packed value ready to store.
auto FinalizeCompute(
    Context& context, const ComputeResult& result, uint32_t semantic_width,
    llvm::StructType* struct_type) -> llvm::Value*;

// Lower packed core rvalues (binary, unary, concat) with unified dispatch.
// Routes to 2-state or 4-state implementations based on target stateness.
// Returns the computed value (width-masked) and sets *unknown_out for 4-state.
auto LowerPackedCoreRvalueValue(
    Context& context, const mir::Compute& compute, llvm::Value** unknown_out)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
