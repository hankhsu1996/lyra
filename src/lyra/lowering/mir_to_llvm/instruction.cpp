#include "lyra/lowering/mir_to_llvm/instruction.hpp"

#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lowering/mir_to_llvm/instruction_compute.hpp"
#include "lyra/lowering/mir_to_llvm/instruction_display.hpp"
#include "lyra/lowering/mir_to_llvm/operand.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void LowerAssign(Context& context, const mir::Assign& assign) {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Get or create storage for the target place
  llvm::AllocaInst* alloca = context.GetOrCreatePlaceStorage(assign.target);

  // Lower the source operand
  llvm::Value* source_value = LowerOperand(context, assign.source);

  // Get the place type info for proper sizing
  const auto& place = arena[assign.target];
  const Type& type = types[place.root.type];

  // Get the storage type
  llvm::Type* storage_type = alloca->getAllocatedType();

  // Adjust the value to match storage type if needed (only for integrals)
  if (source_value->getType() != storage_type &&
      source_value->getType()->isIntegerTy() && storage_type->isIntegerTy()) {
    if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
      source_value = builder.CreateSExtOrTrunc(source_value, storage_type);
    } else {
      source_value = builder.CreateZExtOrTrunc(source_value, storage_type);
    }
  }

  // Store to the alloca
  builder.CreateStore(source_value, alloca);
}

void LowerEffectOp(Context& context, const mir::EffectOp& effect_op) {
  std::visit(
      Overloaded{
          [&context](const mir::DisplayEffect& display) {
            LowerDisplayEffect(context, display);
          },
          [](const mir::SeverityEffect& /*severity*/) {
            // TODO(hankhsu): Handle severity effects
          },
      },
      effect_op);
}

}  // namespace

void LowerInstruction(Context& context, const mir::Instruction& instruction) {
  // Set origin for error reporting
  context.SetCurrentOrigin(instruction.origin);

  std::visit(
      Overloaded{
          [&context](const mir::Assign& assign) {
            LowerAssign(context, assign);
          },
          [&context](const mir::Compute& compute) {
            LowerCompute(context, compute);
          },
          [](const mir::GuardedAssign& /*guarded*/) {
            // TODO(hankhsu): Handle guarded assignments
          },
          [&context](const mir::Effect& effect) {
            LowerEffectOp(context, effect.op);
          },
      },
      instruction.data);
}

}  // namespace lyra::lowering::mir_to_llvm
