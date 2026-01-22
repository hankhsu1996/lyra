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

  // Get the place type info for proper sizing
  const auto& place = arena[assign.target];
  const Type& type = types[place.root.type];

  // Handle string assignment with reference counting
  if (type.Kind() == TypeKind::kString) {
    // 1. Release old value in slot
    auto* old_val = builder.CreateLoad(alloca->getAllocatedType(), alloca);
    builder.CreateCall(context.GetLyraStringRelease(), {old_val});

    // 2. Get new value
    llvm::Value* new_val = LowerOperand(context, assign.source);

    // 3. If source is a place reference (borrowed), retain to get owned
    if (std::holds_alternative<mir::PlaceId>(assign.source.payload)) {
      new_val = builder.CreateCall(context.GetLyraStringRetain(), {new_val});
    }
    // else: source is literal, already owned +1 from LyraStringFromLiteral

    // 4. Store owned value (slot now owns)
    builder.CreateStore(new_val, alloca);
    return;
  }

  // Lower the source operand
  llvm::Value* source_value = LowerOperand(context, assign.source);

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

  // RAII guard for statement-scoped cleanup of owned string temps
  StatementScope scope(context);

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
