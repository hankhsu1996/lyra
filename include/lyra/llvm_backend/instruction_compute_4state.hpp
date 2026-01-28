#pragma once

#include <vector>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/compute_result.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_rvalue.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower 4-state binary rvalue.
auto LowerBinaryRvalue4State(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 4-state unary rvalue.
auto LowerUnaryRvalue4State(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 4-state packed concatenation rvalue.
auto LowerConcatRvalue4State(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 4-state guarded use (bounds-checked array read).
auto LowerGuardedUse4State(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 4-state runtime query (e.g., $time).
auto LowerRuntimeQuery4State(
    Context& context, const mir::RuntimeQueryRvalueInfo& info,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 4-state user call (function call).
auto LowerUserCall4State(
    Context& context, const mir::UserCallRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower case match op (casez/casex) - returns 2-state result.
// Used by both 2-state and 4-state paths.
auto LowerCaseMatchOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*>;

// Lower case equality op (===, !==) - returns 2-state result.
// Used by both 2-state and 4-state paths.
auto LowerCaseEqualityOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
