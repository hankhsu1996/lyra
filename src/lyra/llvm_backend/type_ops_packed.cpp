#include <expected>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops_handlers.hpp"
#include "lyra/llvm_backend/type_ops_store.hpp"

namespace lyra::lowering::mir_to_llvm {

auto AssignFourState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  // 1. Lower source as raw value
  auto raw_or_err = LowerOperandRaw(context, source);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());
  llvm::Value* raw_value = *raw_or_err;

  // 2. No source null-out for packed types (they're value types)

  // 3. Get WriteTarget and delegate to StoreRawToTarget
  auto wt_or_err = context.GetWriteTarget(target);
  if (!wt_or_err) return std::unexpected(wt_or_err.error());

  return StoreRawToTarget(context, *wt_or_err, raw_value, type_id, policy);
}

auto AssignTwoState(
    Context& context, mir::PlaceId target, const mir::Operand& source,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  // 1. Lower source as raw value
  auto raw_or_err = LowerOperandRaw(context, source);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());
  llvm::Value* raw_value = *raw_or_err;

  // 2. No source null-out for packed types (they're value types)

  // 3. Get WriteTarget and delegate to StoreRawToTarget
  auto wt_or_err = context.GetWriteTarget(target);
  if (!wt_or_err) return std::unexpected(wt_or_err.error());

  return StoreRawToTarget(context, *wt_or_err, raw_value, type_id, policy);
}

}  // namespace lyra::lowering::mir_to_llvm
