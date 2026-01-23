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

// Read-modify-write for BitRangeProjection assignment.
// Clears the target bit range in the base value, then OR's in the new value.
void StoreBitRange(
    Context& context, mir::PlaceId target, llvm::Value* source_raw) {
  auto& builder = context.GetBuilder();
  const auto& bitrange = context.GetBitRangeProjection(target);

  llvm::Value* ptr = context.GetPlacePointer(target);
  llvm::Type* base_type = context.GetPlaceBaseType(target);

  // Lower the bit offset
  llvm::Value* offset = LowerOperand(context, bitrange.bit_offset);

  if (base_type->isStructTy()) {
    // 4-state base: RMW both planes independently
    auto* base_struct = llvm::cast<llvm::StructType>(base_type);
    auto* plane_type = base_struct->getElementType(0);
    uint32_t plane_width = plane_type->getIntegerBitWidth();

    llvm::Value* old_packed = builder.CreateLoad(base_type, ptr, "rmw.old");
    llvm::Value* old_val =
        builder.CreateExtractValue(old_packed, 0, "rmw.old.val");
    llvm::Value* old_unk =
        builder.CreateExtractValue(old_packed, 1, "rmw.old.unk");

    auto* shift_amt =
        builder.CreateZExtOrTrunc(offset, plane_type, "rmw.offset");

    // Build mask: getLowBitsSet(plane_width, bitrange.width), shifted
    auto mask_ap = llvm::APInt::getLowBitsSet(plane_width, bitrange.width);
    auto* mask = llvm::ConstantInt::get(plane_type, mask_ap);
    auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
    auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

    // Extract source value/unknown planes
    llvm::Value* src_val = nullptr;
    llvm::Value* src_unk = nullptr;
    if (source_raw->getType()->isStructTy()) {
      src_val = builder.CreateExtractValue(source_raw, 0, "rmw.src.val");
      src_unk = builder.CreateExtractValue(source_raw, 1, "rmw.src.unk");
    } else {
      // 2-state source into 4-state target: unknown = 0 in the range
      src_val = source_raw;
      src_unk = llvm::ConstantInt::get(plane_type, 0);
    }

    // Extend source to plane width and shift into position
    src_val = builder.CreateZExtOrTrunc(src_val, plane_type, "rmw.src.val.ext");
    src_unk = builder.CreateZExtOrTrunc(src_unk, plane_type, "rmw.src.unk.ext");
    auto* new_val_shifted =
        builder.CreateShl(src_val, shift_amt, "rmw.val.shl");
    auto* new_unk_shifted =
        builder.CreateShl(src_unk, shift_amt, "rmw.unk.shl");

    // Clear + OR for both planes
    auto* cleared_val = builder.CreateAnd(old_val, not_mask, "rmw.val.clear");
    auto* result_val =
        builder.CreateOr(cleared_val, new_val_shifted, "rmw.val");
    auto* cleared_unk = builder.CreateAnd(old_unk, not_mask, "rmw.unk.clear");
    auto* result_unk =
        builder.CreateOr(cleared_unk, new_unk_shifted, "rmw.unk");

    // Pack and store
    llvm::Value* packed = llvm::UndefValue::get(base_struct);
    packed = builder.CreateInsertValue(packed, result_val, 0);
    packed = builder.CreateInsertValue(packed, result_unk, 1);
    builder.CreateStore(packed, ptr);
    return;
  }

  // 2-state base: simple RMW
  uint32_t base_width = base_type->getIntegerBitWidth();
  auto* shift_amt = builder.CreateZExtOrTrunc(offset, base_type, "rmw.offset");

  auto mask_ap = llvm::APInt::getLowBitsSet(base_width, bitrange.width);
  auto* mask = llvm::ConstantInt::get(base_type, mask_ap);
  auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
  auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

  llvm::Value* old_val = builder.CreateLoad(base_type, ptr, "rmw.old");
  auto* cleared = builder.CreateAnd(old_val, not_mask, "rmw.clear");

  // Extend source to base width and shift into position
  llvm::Value* src = source_raw;
  if (src->getType()->isStructTy()) {
    // Coerce 4-state source to 2-state
    auto* val = builder.CreateExtractValue(src, 0, "rmw.src.val");
    auto* unk = builder.CreateExtractValue(src, 1, "rmw.src.unk");
    auto* not_unk = builder.CreateNot(unk);
    src = builder.CreateAnd(val, not_unk);
  }
  src = builder.CreateZExtOrTrunc(src, base_type, "rmw.src.ext");
  auto* new_shifted = builder.CreateShl(src, shift_amt, "rmw.src.shl");
  auto* result = builder.CreateOr(cleared, new_shifted, "rmw.result");
  builder.CreateStore(result, ptr);
}

void LowerAssign(Context& context, const mir::Assign& assign) {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // BitRangeProjection targets use read-modify-write
  if (context.HasBitRangeProjection(assign.target)) {
    llvm::Value* source_raw = LowerOperandRaw(context, assign.source);
    StoreBitRange(context, assign.target, source_raw);
    return;
  }

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

void LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded) {
  auto& builder = context.GetBuilder();

  // Source is always evaluated unconditionally (per spec)
  llvm::Value* source_raw = LowerOperandRaw(context, guarded.source);

  // Lower validity predicate to i1
  llvm::Value* valid = LowerOperand(context, guarded.validity);
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "ga.tobool");
  }

  // Branch: only write if valid
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_write_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.write", func);
  auto* skip_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.skip", func);

  builder.CreateCondBr(valid, do_write_bb, skip_bb);

  // Valid path: perform the assignment
  builder.SetInsertPoint(do_write_bb);
  if (context.HasBitRangeProjection(guarded.target)) {
    StoreBitRange(context, guarded.target, source_raw);
  } else {
    // Direct store (same as LowerAssign but with pre-computed source_raw)
    llvm::Value* target_ptr = context.GetPlacePointer(guarded.target);
    llvm::Type* storage_type = context.GetPlaceLlvmType(guarded.target);

    if (storage_type->isStructTy()) {
      // 4-state target
      auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
      auto* elem_type = struct_type->getElementType(0);

      llvm::Value* val = nullptr;
      llvm::Value* unk = nullptr;
      if (source_raw->getType()->isStructTy()) {
        val = builder.CreateExtractValue(source_raw, 0, "ga.val");
        unk = builder.CreateExtractValue(source_raw, 1, "ga.unk");
      } else {
        val = builder.CreateZExtOrTrunc(source_raw, elem_type, "ga.val");
        unk = llvm::ConstantInt::get(elem_type, 0);
      }
      val = builder.CreateZExtOrTrunc(val, elem_type, "ga.val.fit");
      unk = builder.CreateZExtOrTrunc(unk, elem_type, "ga.unk.fit");

      llvm::Value* packed = llvm::UndefValue::get(struct_type);
      packed = builder.CreateInsertValue(packed, val, 0);
      packed = builder.CreateInsertValue(packed, unk, 1);
      builder.CreateStore(packed, target_ptr);
    } else {
      // 2-state target
      llvm::Value* src = source_raw;
      if (src->getType()->isStructTy()) {
        auto* v = builder.CreateExtractValue(src, 0, "ga.val");
        auto* u = builder.CreateExtractValue(src, 1, "ga.unk");
        auto* not_u = builder.CreateNot(u);
        src = builder.CreateAnd(v, not_u);
      }
      if (src->getType() != storage_type && src->getType()->isIntegerTy() &&
          storage_type->isIntegerTy()) {
        src = builder.CreateZExtOrTrunc(src, storage_type, "ga.fit");
      }
      builder.CreateStore(src, target_ptr);
    }
  }
  builder.CreateBr(skip_bb);

  // Continue
  builder.SetInsertPoint(skip_bb);
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
          [&context](const mir::GuardedAssign& guarded) {
            LowerGuardedAssign(context, guarded);
          },
          [&context](const mir::Effect& effect) {
            LowerEffectOp(context, effect.op);
          },
      },
      instruction.data);
}

}  // namespace lyra::lowering::mir_to_llvm
