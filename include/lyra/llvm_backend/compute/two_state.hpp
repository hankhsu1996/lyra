#pragma once

#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

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

// Lower 2-state packed replication rvalue.
auto LowerReplicateRvalue2State(
    Context& context, const mir::ReplicateRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state IsKnown check (X/Z knownness).
auto LowerIsKnown2State(
    Context& context, const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

// Lower 2-state index-in-range check (bounds checking).
auto LowerIndexInRange2State(
    Context& context, const mir::IndexInRangeRvalueInfo& info,
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

// Resolver-aware overloads.
auto LowerBinaryRvalue2State(
    Context& context, SlotAccessResolver& resolver,
    const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

auto LowerUnaryRvalue2State(
    Context& context, SlotAccessResolver& resolver,
    const mir::UnaryRvalueInfo& info, const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

auto LowerConcatRvalue2State(
    Context& context, SlotAccessResolver& resolver,
    const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

auto LowerReplicateRvalue2State(
    Context& context, SlotAccessResolver& resolver,
    const mir::ReplicateRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

auto LowerIsKnown2State(
    Context& context, SlotAccessResolver& resolver,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

auto LowerIndexInRange2State(
    Context& context, SlotAccessResolver& resolver,
    const mir::IndexInRangeRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

auto LowerGuardedUse2State(
    Context& context, SlotAccessResolver& resolver,
    const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult>;

}  // namespace lyra::lowering::mir_to_llvm
