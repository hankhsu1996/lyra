#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

class SlotAccessResolver;

// Lower a MIR operand to an LLVM Value (coerces 4-state to 2-state integer)
auto LowerOperand(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*>;

// Lower a MIR operand without 4-state coercion (returns struct for 4-state)
auto LowerOperandRaw(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*>;

// Resolver-aware overloads: route module-slot reads through the resolver.
auto LowerOperandRaw(
    Context& context, SlotAccessResolver& resolver, const mir::Operand& operand)
    -> Result<llvm::Value*>;

auto LowerOperand(
    Context& context, SlotAccessResolver& resolver, const mir::Operand& operand)
    -> Result<llvm::Value*>;

// Lower a MIR operand as a storage representation matching target_type exactly.
// Used for aggregate element insertion where LLVM demands type-exact values.
//
// Allowed conversions (all unsigned/zero-extended - storage is typeless bits):
//   - 2-state iN -> 2-state iM  (ZExtOrTrunc for storage width rounding)
//   - 2-state iN -> 4-state {iM, iM}  (wrap as {zext(val), 0})
//   - 4-state {iN, iN} -> 4-state {iM, iM}  (ZExtOrTrunc both planes)
// Any other combination throws InternalError.
auto LowerOperandAsStorage(
    Context& context, const mir::Operand& operand, llvm::Type* target_type)
    -> Result<llvm::Value*>;

auto LowerOperandAsStorage(
    Context& context, SlotAccessResolver& resolver, const mir::Operand& operand,
    llvm::Type* target_type) -> Result<llvm::Value*>;

// Canonical operand-to-place resolution.
// PlaceId -> returns the PlaceId directly.
// ExternalRefId -> returns nullopt (resolved via direct helpers, not Place).
// TempId / Constant -> returns nullopt (no place backing).
auto ResolveOperandPlace(Context& context, const mir::Operand& operand)
    -> std::optional<mir::PlaceId>;

// Canonical operand type helpers. Handle PlaceId, TempId, ExternalRefId,
// and Constant uniformly.
auto GetOperandTypeId(
    const CuFacts& facts, Context& context, const mir::Operand& operand)
    -> TypeId;
auto IsOperandFourState(
    const CuFacts& facts, Context& context, const mir::Operand& operand)
    -> bool;

}  // namespace lyra::lowering::mir_to_llvm
