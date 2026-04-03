#include "lyra/llvm_backend/instruction/decision.hpp"

#include <expected>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerRecordDecisionObservation(
    Context& ctx, const mir::RecordDecisionObservation& obs) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  builder.CreateCall(
      ctx.GetLyraRecordDecisionObservation(),
      {ctx.GetEnginePointer(), ctx.GetCurrentProcessId(),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), obs.id.Index()),
       llvm::ConstantInt::get(
           i8_ty, static_cast<uint8_t>(obs.match_class_const)),
       llvm::ConstantInt::get(
           i8_ty, static_cast<uint8_t>(obs.selected_kind_const)),
       llvm::ConstantInt::get(
           llvm::Type::getInt16Ty(llvm_ctx), obs.selected_arm_const.Index())});
  return {};
}

auto LowerRecordDecisionObservationDynamic(
    Context& ctx, SlotAccessResolver& resolver,
    const mir::RecordDecisionObservationDynamic& obs) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  auto mc = LowerOperand(ctx, resolver, obs.match_class);
  if (!mc) return std::unexpected(mc.error());
  auto sk = LowerOperand(ctx, resolver, obs.selected_kind);
  if (!sk) return std::unexpected(sk.error());
  auto sa = LowerOperand(ctx, resolver, obs.selected_arm);
  if (!sa) return std::unexpected(sa.error());

  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i16_ty = llvm::Type::getInt16Ty(llvm_ctx);

  llvm::Value* mc_v = *mc;
  if (mc_v->getType() != i8_ty) {
    mc_v = builder.CreateIntCast(mc_v, i8_ty, false);
  }
  llvm::Value* sk_v = *sk;
  if (sk_v->getType() != i8_ty) {
    sk_v = builder.CreateIntCast(sk_v, i8_ty, false);
  }
  llvm::Value* sa_v = *sa;
  if (sa_v->getType() != i16_ty) {
    sa_v = builder.CreateIntCast(sa_v, i16_ty, false);
  }

  builder.CreateCall(
      ctx.GetLyraRecordDecisionObservation(),
      {ctx.GetEnginePointer(), ctx.GetCurrentProcessId(),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), obs.id.Index()),
       mc_v, sk_v, sa_v});
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
