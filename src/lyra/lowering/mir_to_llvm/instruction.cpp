#include "lyra/lowering/mir_to_llvm/instruction.hpp"

#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lowering/mir_to_llvm/instruction_compute.hpp"
#include "lyra/lowering/mir_to_llvm/instruction_display.hpp"
#include "lyra/lowering/mir_to_llvm/operand.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void LowerAssign(Context& context, const mir::Assign& assign) {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Get pointer to target place storage
  llvm::Value* target_ptr = context.GetPlacePointer(assign.target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(assign.target);

  // Get the effective type (element type if projected, root type otherwise)
  const auto& place = arena[assign.target];
  const Type& type = types[mir::TypeOfPlace(types, place)];

  // Handle string assignment with reference counting
  if (type.Kind() == TypeKind::kString) {
    // 1. Release old value in slot
    auto* old_val = builder.CreateLoad(storage_type, target_ptr);
    builder.CreateCall(context.GetLyraStringRelease(), {old_val});

    // 2. Get new value
    llvm::Value* new_val = LowerOperand(context, assign.source);

    // 3. If source is a place reference (borrowed), retain to get owned
    if (std::holds_alternative<mir::PlaceId>(assign.source.payload)) {
      new_val = builder.CreateCall(context.GetLyraStringRetain(), {new_val});
    }
    // else: source is literal, already owned +1 from LyraStringFromLiteral

    // 4. Store owned value (slot now owns)
    builder.CreateStore(new_val, target_ptr);
    return;
  }

  // Lower the source operand
  llvm::Value* source_value = LowerOperand(context, assign.source);

  // Adjust the value to match storage type if needed (only for integrals)
  if (source_value->getType() != storage_type &&
      source_value->getType()->isIntegerTy() && storage_type->isIntegerTy()) {
    if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
      source_value = builder.CreateSExtOrTrunc(source_value, storage_type);
    } else {
      source_value = builder.CreateZExtOrTrunc(source_value, storage_type);
    }
  }

  // Store to the place
  builder.CreateStore(source_value, target_ptr);
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
