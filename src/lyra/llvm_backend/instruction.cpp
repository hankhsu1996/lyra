#include "lyra/llvm_backend/instruction.hpp"

#include "llvm/IR/DerivedTypes.h"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/instruction_compute.hpp"
#include "lyra/llvm_backend/instruction_display.hpp"
#include "lyra/llvm_backend/operand.hpp"
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

  // 4-state target: construct {value, unknown=0} struct
  if (storage_type->isStructTy()) {
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    auto* elem_type = struct_type->getElementType(0);

    // Load source as raw value (struct if 4-state, integer if 2-state)
    llvm::Value* raw = LowerOperandRaw(context, assign.source);

    llvm::Value* val = nullptr;
    llvm::Value* unk = nullptr;
    if (raw->getType()->isStructTy()) {
      // Source is 4-state: pass through
      val = builder.CreateExtractValue(raw, 0, "assign.val");
      unk = builder.CreateExtractValue(raw, 1, "assign.unk");
    } else {
      // Source is 2-state: wrap as {value, unknown=0}
      val = builder.CreateZExtOrTrunc(raw, elem_type, "assign.val");
      unk = llvm::ConstantInt::get(elem_type, 0);
    }

    // Coerce to elem_type if width differs
    val = builder.CreateZExtOrTrunc(val, elem_type, "assign.val.fit");
    unk = builder.CreateZExtOrTrunc(unk, elem_type, "assign.unk.fit");

    llvm::Value* packed = llvm::UndefValue::get(struct_type);
    packed = builder.CreateInsertValue(packed, val, 0);
    packed = builder.CreateInsertValue(packed, unk, 1);
    builder.CreateStore(packed, target_ptr);
    return;
  }

  // 2-state target: lower source (coerces 4-state to integer)
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
