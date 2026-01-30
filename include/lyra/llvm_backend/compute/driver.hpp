#pragma once

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/rvalue.hpp"

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
// Routes to 2-state or 4-state implementations based on result type stateness.
auto LowerPackedCoreRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId result_type)
    -> Result<RvalueValue>;

}  // namespace lyra::lowering::mir_to_llvm
