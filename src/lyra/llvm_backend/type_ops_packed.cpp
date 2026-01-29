#include <expected>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignFourState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy /*policy*/, TypeId type_id) -> Result<void> {
  // 1. Lower source as raw value
  auto raw_or_err = LowerOperandRaw(context, source);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());
  llvm::Value* raw_value = *raw_or_err;

  // 2. No source null-out for packed types (they're value types)
  // 3. Delegate to CommitValue (policy is irrelevant for packed types)
  return CommitValue(
      context, target, raw_value, type_id, OwnershipPolicy::kClone);
}

auto AssignTwoState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy /*policy*/, TypeId type_id) -> Result<void> {
  // 1. Lower source as raw value
  auto raw_or_err = LowerOperandRaw(context, source);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());
  llvm::Value* raw_value = *raw_or_err;

  // 2. No source null-out for packed types (they're value types)
  // 3. Delegate to CommitValue (policy is irrelevant for packed types)
  return CommitValue(
      context, target, raw_value, type_id, OwnershipPolicy::kClone);
}

}  // namespace lyra::lowering::mir_to_llvm
