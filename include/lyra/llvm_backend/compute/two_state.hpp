#pragma once

#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower 2-state binary rvalue (arithmetic, comparison, shift, etc).
auto LowerBinaryRvalue2State(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state unary rvalue (negation, reduction, etc).
auto LowerUnaryRvalue2State(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state packed concatenation rvalue.
auto LowerConcatRvalue2State(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state index validity check (bounds checking).
auto LowerIndexValidity2State(
    Context& context, const mir::IndexValidityRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state guarded use (bounds-checked array read).
auto LowerGuardedUse2State(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state runtime query (e.g., $time).
auto LowerRuntimeQuery2State(
    Context& context, const mir::RuntimeQueryRvalueInfo& info,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state user call (function call).
auto LowerUserCall2State(
    Context& context, const mir::UserCallRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

}  // namespace lyra::lowering::mir_to_llvm
