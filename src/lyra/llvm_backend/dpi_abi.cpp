#include "lyra/llvm_backend/dpi_abi.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <vector>

#include <llvm/IR/CallingConv.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/callable_abi.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/value_repr.hpp"
#include "lyra/mir/dpi_verify.hpp"

namespace lyra::lowering::mir_to_llvm::dpi {

namespace {

// ---------------------------------------------------------------------------
// Scalar coercion helpers (D1/D2, reused for 2-state and pointer types)
// ---------------------------------------------------------------------------

auto CoerceToDpiAbiType(
    Context& context, llvm::Value* value, DpiAbiTypeClass abi_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  llvm::Type* target = GetLlvmScalarDpiType(context.GetLlvmContext(), abi_type);

  if (value->getType() == target) {
    return value;
  }

  if (value->getType()->isIntegerTy() && target->isIntegerTy()) {
    unsigned src_bits = value->getType()->getIntegerBitWidth();
    unsigned dst_bits = target->getIntegerBitWidth();
    if (src_bits < dst_bits) {
      return builder.CreateZExt(value, target);
    }
    return builder.CreateTrunc(value, target);
  }

  if (value->getType()->isFloatTy() && target->isDoubleTy()) {
    return builder.CreateFPExt(value, target);
  }
  if (value->getType()->isDoubleTy() && target->isFloatTy()) {
    return builder.CreateFPTrunc(value, target);
  }

  if (value->getType()->isPointerTy() && target->isPointerTy()) {
    return value;
  }

  throw common::InternalError(
      "CoerceToDpiAbiType",
      "cannot coerce between incompatible LLVM types at DPI boundary");
}

auto CoerceFromDpiAbiType(
    Context& context, llvm::Value* value, DpiAbiTypeClass abi_type,
    llvm::Type* internal_type) -> llvm::Value* {
  llvm::Type* expected_abi =
      GetLlvmScalarDpiType(context.GetLlvmContext(), abi_type);

  bool shape_ok = false;
  if (expected_abi->isIntegerTy() && value->getType()->isIntegerTy()) {
    shape_ok = value->getType()->getIntegerBitWidth() ==
               expected_abi->getIntegerBitWidth();
  } else if (expected_abi->isFloatTy()) {
    shape_ok = value->getType()->isFloatTy();
  } else if (expected_abi->isDoubleTy()) {
    shape_ok = value->getType()->isDoubleTy();
  } else if (expected_abi->isPointerTy()) {
    shape_ok = value->getType()->isPointerTy();
  }
  if (!shape_ok) {
    throw common::InternalError(
        "CoerceFromDpiAbiType",
        std::format(
            "DPI value does not match expected ABI carrier for type {}",
            static_cast<int>(abi_type)));
  }

  if (value->getType() == internal_type) {
    return value;
  }

  auto& builder = context.GetBuilder();

  if (value->getType()->isIntegerTy() && internal_type->isIntegerTy()) {
    unsigned src_bits = value->getType()->getIntegerBitWidth();
    unsigned dst_bits = internal_type->getIntegerBitWidth();
    if (src_bits < dst_bits) {
      return builder.CreateZExt(value, internal_type);
    }
    return builder.CreateTrunc(value, internal_type);
  }

  if (value->getType()->isFloatTy() && internal_type->isDoubleTy()) {
    return builder.CreateFPExt(value, internal_type);
  }
  if (value->getType()->isDoubleTy() && internal_type->isFloatTy()) {
    return builder.CreateFPTrunc(value, internal_type);
  }

  if (value->getType()->isPointerTy() && internal_type->isPointerTy()) {
    return value;
  }

  throw common::InternalError(
      "CoerceFromDpiAbiType",
      std::format(
          "cannot coerce DPI ABI type {} to internal type",
          static_cast<int>(abi_type)));
}

// ---------------------------------------------------------------------------
// String marshaling helpers
// ---------------------------------------------------------------------------

auto MarshalInputString(Context& context, llvm::Value* str_handle)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* is_null = builder.CreateICmpEQ(
      str_handle, llvm::ConstantPointerNull::get(ptr_ty), "dpi.str.null");

  auto* cur_bb = builder.GetInsertBlock();
  auto* parent_fn = cur_bb->getParent();
  auto* nonnull_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.nonnull", parent_fn);
  auto* join_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.join", parent_fn);

  builder.CreateCondBr(is_null, join_bb, nonnull_bb);

  builder.SetInsertPoint(nonnull_bb);
  auto* handle_cstr = builder.CreateCall(
      context.GetLyraStringGetCStr(), {str_handle}, "dpi.str.cstr");
  builder.CreateBr(join_bb);

  builder.SetInsertPoint(join_bb);
  auto* empty_cstr = builder.CreateGlobalStringPtr("", "dpi.str.empty");
  auto* phi = builder.CreatePHI(ptr_ty, 2, "dpi.str.arg");
  phi->addIncoming(empty_cstr, cur_bb);
  phi->addIncoming(handle_cstr, nonnull_bb);

  return phi;
}

auto MaterializeStringWriteback(Context& context, llvm::Value* c_str_ptr)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* is_null = builder.CreateICmpEQ(
      c_str_ptr, llvm::ConstantPointerNull::get(ptr_ty), "dpi.str.wb.null");

  auto* cur_bb = builder.GetInsertBlock();
  auto* parent_fn = cur_bb->getParent();
  auto* null_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.wb.null_br", parent_fn);
  auto* nonnull_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.wb.nonnull", parent_fn);
  auto* join_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "dpi.str.wb.join", parent_fn);

  builder.CreateCondBr(is_null, null_bb, nonnull_bb);

  builder.SetInsertPoint(null_bb);
  auto* empty_cstr = builder.CreateGlobalStringPtr("", "dpi.str.wb.empty");
  auto* empty_handle = builder.CreateCall(
      context.GetLyraStringFromCStr(), {empty_cstr}, "dpi.str.wb.empty_h");
  builder.CreateBr(join_bb);

  builder.SetInsertPoint(nonnull_bb);
  auto* handle = builder.CreateCall(
      context.GetLyraStringFromCStr(), {c_str_ptr}, "dpi.str.wb.handle");
  builder.CreateBr(join_bb);

  builder.SetInsertPoint(join_bb);
  auto* phi = builder.CreatePHI(ptr_ty, 2, "dpi.str.wb.result");
  phi->addIncoming(empty_handle, null_bb);
  phi->addIncoming(handle, nonnull_bb);

  return phi;
}

// ---------------------------------------------------------------------------
// DPI packed-vector layout helpers
// ---------------------------------------------------------------------------

auto GetDpiLogicWordCount(uint32_t sv_width) -> uint32_t {
  return std::max(1U, (sv_width + 31) / 32);
}

auto GetDpiLogicVecWordType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* i32 = llvm::Type::getInt32Ty(ctx);
  return llvm::StructType::get(ctx, {i32, i32});
}

auto GetDpiLogicVecStorageType(llvm::LLVMContext& ctx, uint32_t word_count)
    -> llvm::ArrayType* {
  return llvm::ArrayType::get(GetDpiLogicVecWordType(ctx), word_count);
}

auto GetDpiBitVecStorageType(llvm::LLVMContext& ctx, uint32_t word_count)
    -> llvm::ArrayType* {
  return llvm::ArrayType::get(llvm::Type::getInt32Ty(ctx), word_count);
}

auto GetSemanticWidth(TypeId sv_type, const TypeArena& types) -> uint32_t {
  const auto& type = types[sv_type];
  return PackedBitWidth(type, types);
}

auto LowBitsMask(llvm::Type* backing_ty, uint32_t semantic_width)
    -> llvm::Value* {
  auto* int_ty = llvm::dyn_cast<llvm::IntegerType>(backing_ty);
  if (int_ty == nullptr) {
    throw common::InternalError("LowBitsMask", "expected integer backing type");
  }
  if (int_ty->getBitWidth() < semantic_width) {
    throw common::InternalError(
        "LowBitsMask", std::format(
                           "backing width {} is smaller than semantic width {}",
                           int_ty->getBitWidth(), semantic_width));
  }
  llvm::APInt bits =
      llvm::APInt::getLowBitsSet(int_ty->getBitWidth(), semantic_width);
  return llvm::ConstantInt::get(backing_ty, bits);
}

// ---------------------------------------------------------------------------
// 4-state operand extraction (D3a)
// ---------------------------------------------------------------------------

struct DpiFourStateValue {
  llvm::Value* val;
  llvm::Value* unk;
  uint32_t semantic_width;
};

auto LowerDpiFourStateOperand(
    Context& ctx, const mir::Operand& op, TypeId sv_type)
    -> Result<DpiFourStateValue> {
  auto raw = LowerOperandRaw(ctx, op);
  if (!raw) return std::unexpected(raw.error());
  auto& b = ctx.GetBuilder();
  auto [val, unk] = ExtractFourStateOrZero(b, *raw);
  uint32_t width = GetSemanticWidth(sv_type, ctx.GetTypeArena());

  // Canonicalize: mask to semantic width so backing-width garbage bits never
  // leak into svLogicVecVal words during encode.
  auto* backing_ty = val->getType();
  uint32_t backing_bits = backing_ty->getIntegerBitWidth();
  if (backing_bits > width) {
    llvm::APInt mask_ap = llvm::APInt::getLowBitsSet(backing_bits, width);
    llvm::Value* mask = llvm::ConstantInt::get(backing_ty, mask_ap);
    val = b.CreateAnd(val, mask, "dpi.fs.val.mask");
    unk = b.CreateAnd(unk, mask, "dpi.fs.unk.mask");
  }

  return DpiFourStateValue{.val = val, .unk = unk, .semantic_width = width};
}

// ---------------------------------------------------------------------------
// 4-state encode/decode (D3a)
//
// IEEE 1800-2023 Section 35.5.6:
//   svLogic scalar: sv_0=0, sv_1=1, sv_z=2, sv_x=3
//   svLogicVecVal:  {aval, bval} per 32-bit word
//     0: aval=0,bval=0  1: aval=1,bval=0  Z: aval=0,bval=1  X: aval=1,bval=1
//
// Lyra internal:
//   0: val=0,unk=0  1: val=1,unk=0  X: val=0,unk=1  Z: val=1,unk=1
//
// Lyra->IEEE: aval = val ^ unk,  bval = unk
// IEEE->Lyra: val = aval ^ bval, unk = bval
//
// svLogic scalar: (unk << 1) | (val ^ unk)
//   Verify: (0,0)->0, (1,0)->1, (0,1)->3(X), (1,1)->2(Z). Correct.
// ---------------------------------------------------------------------------

auto EncodeSvLogic(llvm::IRBuilder<>& b, llvm::Value* val1, llvm::Value* unk1)
    -> llvm::Value* {
  auto* i8 = llvm::Type::getInt8Ty(b.getContext());
  auto* v = b.CreateZExt(val1, i8, "dpi.svl.val");
  auto* u = b.CreateZExt(unk1, i8, "dpi.svl.unk");
  auto* xor_vu = b.CreateXor(v, u, "dpi.svl.xor");
  auto* shl_u = b.CreateShl(u, 1, "dpi.svl.shl");
  return b.CreateOr(shl_u, xor_vu, "dpi.svl.enc");
}

auto DecodeSvLogic(llvm::IRBuilder<>& b, llvm::Value* sv_logic)
    -> FourStateValue {
  auto* i8 = llvm::Type::getInt8Ty(b.getContext());
  auto* one = llvm::ConstantInt::get(i8, 1);
  auto* aval = b.CreateAnd(sv_logic, one, "dpi.svl.aval");
  auto* bval = b.CreateAnd(
      b.CreateLShr(sv_logic, 1, "dpi.svl.shr"), one, "dpi.svl.bval");
  auto* val = b.CreateXor(aval, bval, "dpi.svl.dec.val");
  return {.value = val, .unknown = bval};
}

// Decode an svLogic byte and pack into a Lyra 4-state struct at the correct
// backing width. Shared by both writeback and return paths.
auto BuildLyraLogicScalarValue(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* raw_sv_logic, uint32_t semantic_width) -> llvm::Value* {
  auto [val, unk] = DecodeSvLogic(builder, raw_sv_logic);
  auto* struct_ty = GetBackingFourStateType(llvm_ctx, semantic_width);
  auto* elem_ty = struct_ty->getElementType(0);
  auto* val_fit = builder.CreateZExtOrTrunc(val, elem_ty, "dpi.ls.fit.val");
  auto* unk_fit = builder.CreateZExtOrTrunc(unk, elem_ty, "dpi.ls.fit.unk");
  return PackFourState(builder, struct_ty, val_fit, unk_fit);
}

// Extract a 32-bit word from an integer value (may be narrower or wider).
auto ExtractWord32(llvm::IRBuilder<>& b, llvm::Value* wide, uint32_t word_idx)
    -> llvm::Value* {
  auto* i32 = llvm::Type::getInt32Ty(b.getContext());
  if (word_idx == 0) {
    return b.CreateZExtOrTrunc(wide, i32, "dpi.w32.lo");
  }
  uint32_t shift = word_idx * 32;
  auto* shifted = b.CreateLShr(
      wide, llvm::ConstantInt::get(wide->getType(), shift), "dpi.w32.shr");
  return b.CreateZExtOrTrunc(shifted, i32, "dpi.w32.trunc");
}

// Insert a 32-bit word into a wide integer value at the given word index.
auto InsertWord32(
    llvm::IRBuilder<>& b, llvm::Value* accum, llvm::Value* word32,
    uint32_t word_idx) -> llvm::Value* {
  auto* wide_ty = accum->getType();
  auto* extended = b.CreateZExtOrTrunc(word32, wide_ty, "dpi.iw32.ext");
  if (word_idx == 0) {
    return b.CreateOr(accum, extended, "dpi.iw32.or");
  }
  uint32_t shift = word_idx * 32;
  auto* shifted = b.CreateShl(
      extended, llvm::ConstantInt::get(wide_ty, shift), "dpi.iw32.shl");
  return b.CreateOr(accum, shifted, "dpi.iw32.or");
}

// Validate that an array type has the expected svLogicVecVal shape.
void ValidateLogicVecStorageShape(
    llvm::ArrayType* arr_ty, llvm::StructType* word_ty, llvm::LLVMContext& ctx,
    const char* caller) {
  auto* i32 = llvm::Type::getInt32Ty(ctx);
  if (word_ty == nullptr || word_ty->getNumElements() != 2 ||
      word_ty->getElementType(0) != i32 || word_ty->getElementType(1) != i32) {
    throw common::InternalError(
        caller, "expected {i32, i32} word type for svLogicVecVal transport");
  }
  if (arr_ty->getElementType() != word_ty) {
    throw common::InternalError(
        caller, "array element type does not match word type");
  }
}

// Encode Lyra 4-state (val, unk) into svLogicVecVal array [N x {i32, i32}].
// Conversion: aval = val ^ unk, bval = unk (per 32-bit word).
auto BuildDpiLogicVecStorage(
    llvm::IRBuilder<>& b, const DpiFourStateValue& fsv, llvm::ArrayType* arr_ty,
    llvm::StructType* word_ty) -> llvm::Value* {
  ValidateLogicVecStorageShape(
      arr_ty, word_ty, b.getContext(), "BuildDpiLogicVecStorage");
  uint32_t word_count = arr_ty->getNumElements();
  llvm::Value* arr = llvm::UndefValue::get(arr_ty);
  for (uint32_t i = 0; i < word_count; ++i) {
    llvm::Value* val_w = ExtractWord32(b, fsv.val, i);
    llvm::Value* unk_w = ExtractWord32(b, fsv.unk, i);
    llvm::Value* aval = b.CreateXor(val_w, unk_w, "dpi.vec.aval");
    llvm::Value* word = llvm::UndefValue::get(word_ty);
    word = b.CreateInsertValue(word, aval, {0});
    word = b.CreateInsertValue(word, unk_w, {1});
    arr = b.CreateInsertValue(arr, word, {i});
  }
  return arr;
}

// Decode svLogicVecVal array back to Lyra 4-state {iN, iN} struct.
// Conversion: val = aval ^ bval, unk = bval (per 32-bit word).
auto DecodeDpiLogicVecStorage(
    llvm::IRBuilder<>& b, llvm::Value* storage, uint32_t semantic_width,
    llvm::Type* backing_ty) -> llvm::Value* {
  auto* arr_ty = llvm::dyn_cast<llvm::ArrayType>(storage->getType());
  if (arr_ty == nullptr) {
    throw common::InternalError(
        "DecodeDpiLogicVecStorage",
        "expected array storage for svLogicVecVal transport");
  }
  auto* word_ty = llvm::dyn_cast<llvm::StructType>(arr_ty->getElementType());
  ValidateLogicVecStorageShape(
      arr_ty, word_ty, b.getContext(), "DecodeDpiLogicVecStorage");

  auto* backing_int_ty = llvm::dyn_cast<llvm::IntegerType>(backing_ty);
  if (backing_int_ty == nullptr) {
    throw common::InternalError(
        "DecodeDpiLogicVecStorage",
        "expected integer backing type for svLogicVecVal transport");
  }
  if (backing_int_ty->getBitWidth() < semantic_width) {
    throw common::InternalError(
        "DecodeDpiLogicVecStorage",
        std::format(
            "integer backing width {} is smaller than semantic width {}",
            backing_int_ty->getBitWidth(), semantic_width));
  }

  uint32_t word_count = arr_ty->getNumElements();
  llvm::Value* full_aval = llvm::ConstantInt::get(backing_ty, 0);
  llvm::Value* full_bval = llvm::ConstantInt::get(backing_ty, 0);

  for (uint32_t i = 0; i < word_count; ++i) {
    llvm::Value* word = b.CreateExtractValue(storage, {i});
    llvm::Value* aval_w = b.CreateExtractValue(word, {0});
    llvm::Value* bval_w = b.CreateExtractValue(word, {1});
    full_aval = InsertWord32(b, full_aval, aval_w, i);
    full_bval = InsertWord32(b, full_bval, bval_w, i);
  }

  llvm::Value* mask = LowBitsMask(backing_ty, semantic_width);
  llvm::Value* lyra_val =
      b.CreateAnd(b.CreateXor(full_aval, full_bval), mask, "dpi.dec.val");
  llvm::Value* lyra_unk = b.CreateAnd(full_bval, mask, "dpi.dec.unk");

  auto* struct_ty =
      llvm::StructType::get(b.getContext(), {backing_ty, backing_ty});
  return PackFourState(b, struct_ty, lyra_val, lyra_unk);
}

// Encode a Lyra 2-state integer into svBitVecVal array [N x i32].
auto BuildDpiBitVecStorage(
    llvm::IRBuilder<>& b, llvm::Value* two_state_val, uint32_t semantic_width,
    llvm::ArrayType* arr_ty) -> llvm::Value* {
  if (arr_ty->getElementType() != llvm::Type::getInt32Ty(b.getContext())) {
    throw common::InternalError(
        "BuildDpiBitVecStorage",
        "expected [N x i32] storage for svBitVecVal transport");
  }
  auto* val_int_ty =
      llvm::dyn_cast<llvm::IntegerType>(two_state_val->getType());
  if (val_int_ty == nullptr) {
    throw common::InternalError(
        "BuildDpiBitVecStorage",
        "expected integer LLVM value for svBitVecVal transport");
  }
  if (val_int_ty->getBitWidth() < semantic_width) {
    throw common::InternalError(
        "BuildDpiBitVecStorage",
        std::format(
            "integer backing width {} is smaller than semantic width {}",
            val_int_ty->getBitWidth(), semantic_width));
  }

  if (val_int_ty->getBitWidth() > semantic_width) {
    llvm::Value* mask = LowBitsMask(two_state_val->getType(), semantic_width);
    two_state_val = b.CreateAnd(two_state_val, mask, "dpi.bv.mask");
  }

  uint32_t word_count = arr_ty->getNumElements();
  llvm::Value* arr = llvm::UndefValue::get(arr_ty);
  for (uint32_t i = 0; i < word_count; ++i) {
    llvm::Value* word = ExtractWord32(b, two_state_val, i);
    arr = b.CreateInsertValue(arr, word, {i});
  }
  return arr;
}

// Decode svBitVecVal array [N x i32] back to a Lyra 2-state integer.
auto DecodeDpiBitVecStorage(
    llvm::IRBuilder<>& b, llvm::Value* raw_arr, uint32_t semantic_width,
    llvm::Type* backing_ty) -> llvm::Value* {
  auto* backing_int_ty = llvm::dyn_cast<llvm::IntegerType>(backing_ty);
  if (backing_int_ty == nullptr) {
    throw common::InternalError(
        "DecodeDpiBitVecStorage",
        "expected integer backing type for svBitVecVal transport");
  }
  if (backing_int_ty->getBitWidth() < semantic_width) {
    throw common::InternalError(
        "DecodeDpiBitVecStorage",
        std::format(
            "integer backing width {} is smaller than semantic width {}",
            backing_int_ty->getBitWidth(), semantic_width));
  }

  auto* arr_ty = llvm::dyn_cast<llvm::ArrayType>(raw_arr->getType());
  if (arr_ty == nullptr) {
    throw common::InternalError(
        "DecodeDpiBitVecStorage",
        "expected array storage for svBitVecVal transport");
  }
  if (arr_ty->getElementType() != llvm::Type::getInt32Ty(b.getContext())) {
    throw common::InternalError(
        "DecodeDpiBitVecStorage",
        "expected [N x i32] storage for svBitVecVal transport");
  }

  uint32_t word_count = arr_ty->getNumElements();
  llvm::Value* accum = llvm::ConstantInt::get(backing_ty, 0);
  for (uint32_t i = 0; i < word_count; ++i) {
    llvm::Value* word = b.CreateExtractValue(raw_arr, {i});
    accum = InsertWord32(b, accum, word, i);
  }
  if (backing_int_ty->getBitWidth() > semantic_width) {
    llvm::Value* mask = LowBitsMask(backing_ty, semantic_width);
    accum = b.CreateAnd(accum, mask, "dpi.bv.dec.mask");
  }
  return accum;
}

// ---------------------------------------------------------------------------
// Entry-block alloca for staged temps
// ---------------------------------------------------------------------------

auto CreateDpiStagedAlloca(
    llvm::IRBuilder<>& builder, llvm::Type* ty, const char* name)
    -> llvm::AllocaInst* {
  auto* func = builder.GetInsertBlock()->getParent();
  auto& entry_bb = func->getEntryBlock();
  auto insert_pt = entry_bb.begin();
  while (insert_pt != entry_bb.end() &&
         llvm::isa<llvm::AllocaInst>(&*insert_pt)) {
    ++insert_pt;
  }
  llvm::IRBuilder<> entry_builder(&entry_bb, insert_pt);
  return entry_builder.CreateAlloca(ty, nullptr, name);
}

// ---------------------------------------------------------------------------
// Per-argument lowering state
// ---------------------------------------------------------------------------

struct DpiArgLoweringState {
  const mir::DpiParamDesc* desc = nullptr;
  llvm::AllocaInst* staged_tmp = nullptr;
};

// ---------------------------------------------------------------------------
// Phase A helpers
// ---------------------------------------------------------------------------

// Marshal a scalar input value to its DPI C ABI representation.
// Handles string, kLogicScalar, and 2-state scalar types.
// Rejects packed-vector, kVoid, and kInvalid -- those are not scalar paths.
auto MarshalDpiScalarInput(
    Context& context, const mir::Operand& operand,
    const mir::DpiParamDesc& param) -> Result<llvm::Value*> {
  if (IsPackedVecDpiType(param.abi_type) ||
      param.abi_type == DpiAbiTypeClass::kVoid ||
      param.abi_type == DpiAbiTypeClass::kInvalid) {
    throw common::InternalError(
        "MarshalDpiScalarInput",
        std::format(
            "non-scalar DPI ABI class {}", static_cast<int>(param.abi_type)));
  }
  if (param.abi_type == DpiAbiTypeClass::kString) {
    auto val = LowerOperand(context, operand);
    if (!val) return std::unexpected(val.error());
    return MarshalInputString(context, *val);
  }
  if (param.abi_type == DpiAbiTypeClass::kLogicScalar) {
    auto fsv = LowerDpiFourStateOperand(context, operand, param.sv_type);
    if (!fsv) return std::unexpected(fsv.error());
    auto& b = context.GetBuilder();
    auto* i1 = llvm::Type::getInt1Ty(b.getContext());
    auto* val1 = b.CreateTrunc(fsv->val, i1, "dpi.ls.val");
    auto* unk1 = b.CreateTrunc(fsv->unk, i1, "dpi.ls.unk");
    return EncodeSvLogic(b, val1, unk1);
  }
  auto val = LowerOperand(context, operand);
  if (!val) return std::unexpected(val.error());
  return CoerceToDpiAbiType(context, *val, param.abi_type);
}

// Marshal a 4-state packed vector input to svLogicVecVal storage.
// Accepts kLogicVecNarrow and kLogicVecWide.
auto MarshalDpiLogicVecInput(
    Context& context, const mir::Operand& operand,
    const mir::DpiParamDesc& param) -> Result<llvm::Value*> {
  if (!IsFourStateVecDpiType(param.abi_type)) {
    throw common::InternalError(
        "MarshalDpiLogicVecInput",
        std::format(
            "expected 4-state vector ABI class, got {}",
            static_cast<int>(param.abi_type)));
  }
  auto fsv = LowerDpiFourStateOperand(context, operand, param.sv_type);
  if (!fsv) return std::unexpected(fsv.error());
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  uint32_t wc = GetDpiLogicWordCount(fsv->semantic_width);
  auto* arr_ty = GetDpiLogicVecStorageType(llvm_ctx, wc);
  auto* word_ty = GetDpiLogicVecWordType(llvm_ctx);
  return BuildDpiLogicVecStorage(b, *fsv, arr_ty, word_ty);
}

// Marshal a 2-state wide packed vector input to svBitVecVal storage.
// Only accepts kBitVecWide.
auto MarshalDpiBitVecInput(
    Context& context, const mir::Operand& operand,
    const mir::DpiParamDesc& param) -> Result<llvm::Value*> {
  if (param.abi_type != DpiAbiTypeClass::kBitVecWide) {
    throw common::InternalError(
        "MarshalDpiBitVecInput", std::format(
                                     "expected kBitVecWide, got ABI class {}",
                                     static_cast<int>(param.abi_type)));
  }
  auto val = LowerOperand(context, operand);
  if (!val) return std::unexpected(val.error());
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  uint32_t width = GetSemanticWidth(param.sv_type, context.GetTypeArena());
  uint32_t wc = GetDpiLogicWordCount(width);
  auto* arr_ty = GetDpiBitVecStorageType(llvm_ctx, wc);
  return BuildDpiBitVecStorage(b, *val, width, arr_ty);
}

// Populate a by-pointer staged temp with the marshaled input value.
// Only valid for by-pointer arguments (output, inout, or packed-vector input).
auto InitializeDpiByPointerArgStorage(
    Context& context, const mir::Operand& operand,
    const mir::DpiParamDesc& param, llvm::AllocaInst* alloca) -> Result<void> {
  if (param.passing != mir::DpiPassingMode::kByPointer) {
    throw common::InternalError(
        "InitializeDpiByPointerArgStorage", "called for by-value parameter");
  }
  llvm::Value* storage = nullptr;
  if (IsFourStateVecDpiType(param.abi_type)) {
    auto s = MarshalDpiLogicVecInput(context, operand, param);
    if (!s) return std::unexpected(s.error());
    storage = *s;
  } else if (param.abi_type == DpiAbiTypeClass::kBitVecWide) {
    auto s = MarshalDpiBitVecInput(context, operand, param);
    if (!s) return std::unexpected(s.error());
    storage = *s;
  } else {
    auto s = MarshalDpiScalarInput(context, operand, param);
    if (!s) return std::unexpected(s.error());
    storage = *s;
  }
  context.GetBuilder().CreateStore(storage, alloca);
  return {};
}

// ---------------------------------------------------------------------------
// Phase A: Prepare arguments
// ---------------------------------------------------------------------------

auto PrepareDpiCallArgs(
    Context& context, const mir::DpiCall& call,
    std::vector<llvm::Value*>& abi_args,
    std::vector<DpiArgLoweringState>& arg_states) -> Result<void> {
  const auto& sig = call.callee.signature;
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  for (size_t i = 0; i < call.args.size(); ++i) {
    const auto& binding = call.args[i];
    const auto& param = sig.params[i];
    arg_states[i].desc = &param;

    if (param.passing == mir::DpiPassingMode::kByValue) {
      auto marshaled =
          MarshalDpiScalarInput(context, *binding.input_value, param);
      if (!marshaled) return std::unexpected(marshaled.error());
      abi_args.push_back(*marshaled);
    } else {
      llvm::Type* storage_ty =
          GetLlvmDpiStorageType(llvm_ctx, param.abi_type, param.sv_type, types);
      auto* alloca = CreateDpiStagedAlloca(builder, storage_ty, "dpi.staged");
      arg_states[i].staged_tmp = alloca;

      if (binding.input_value) {
        auto result = InitializeDpiByPointerArgStorage(
            context, *binding.input_value, param, alloca);
        if (!result) return std::unexpected(result.error());
      }

      abi_args.push_back(alloca);
    }
  }
  return {};
}

// ---------------------------------------------------------------------------
// Phase C.1: Writeback output/inout params
// ---------------------------------------------------------------------------

auto CommitDpiOutputWritebacks(
    Context& context, const mir::DpiCall& call,
    const std::vector<DpiArgLoweringState>& arg_states) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  for (size_t i = 0; i < call.args.size(); ++i) {
    if (arg_states[i].staged_tmp == nullptr) {
      continue;
    }
    const auto& binding = call.args[i];
    const auto& param = *arg_states[i].desc;

    // Input-only by-pointer (packed-vector input): no writeback.
    if (param.direction == ParameterDirection::kInput) {
      continue;
    }

    if (!binding.writeback_dest.has_value()) {
      throw common::InternalError(
          "CommitDpiOutputWritebacks",
          std::format("missing writeback destination for DPI param {}", i));
    }

    llvm::Type* storage_ty =
        GetLlvmDpiStorageType(llvm_ctx, param.abi_type, param.sv_type, types);
    llvm::Value* raw =
        builder.CreateLoad(storage_ty, arg_states[i].staged_tmp, "dpi.wb.raw");

    llvm::Value* decoded = nullptr;
    if (param.abi_type == DpiAbiTypeClass::kLogicScalar) {
      uint32_t width = GetSemanticWidth(param.sv_type, types);
      decoded = BuildLyraLogicScalarValue(builder, llvm_ctx, raw, width);
    } else if (IsFourStateVecDpiType(param.abi_type)) {
      uint32_t width = GetSemanticWidth(param.sv_type, types);
      auto* backing_ty = GetBackingLlvmType(llvm_ctx, width);
      decoded = DecodeDpiLogicVecStorage(builder, raw, width, backing_ty);
    } else if (param.abi_type == DpiAbiTypeClass::kBitVecWide) {
      uint32_t width = GetSemanticWidth(param.sv_type, types);
      auto* backing_ty = GetBackingLlvmType(llvm_ctx, width);
      decoded = DecodeDpiBitVecStorage(builder, raw, width, backing_ty);
    } else if (param.abi_type == DpiAbiTypeClass::kString) {
      decoded = MaterializeStringWriteback(context, raw);
    } else {
      auto dest_type = context.GetPlaceLlvmType(*binding.writeback_dest);
      if (!dest_type) return std::unexpected(dest_type.error());
      decoded = CoerceFromDpiAbiType(context, raw, param.abi_type, *dest_type);
    }
    auto result = CommitValue(
        context, *binding.writeback_dest, decoded, param.sv_type,
        OwnershipPolicy::kMove);
    if (!result) return std::unexpected(result.error());
  }
  return {};
}

// ---------------------------------------------------------------------------
// Phase C.2: Handle return value
// ---------------------------------------------------------------------------

auto DecodeIndirectDpiReturn(
    Context& context, const mir::DpiReturnDesc& result, llvm::Value* raw)
    -> llvm::Value* {
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  uint32_t width = GetSemanticWidth(result.sv_type, context.GetTypeArena());
  auto* backing_ty = GetBackingLlvmType(llvm_ctx, width);

  switch (result.abi_type) {
    case DpiAbiTypeClass::kLogicVecNarrow:
    case DpiAbiTypeClass::kLogicVecWide:
      return DecodeDpiLogicVecStorage(b, raw, width, backing_ty);
    case DpiAbiTypeClass::kBitVecWide:
      return DecodeDpiBitVecStorage(b, raw, width, backing_ty);
    default:
      throw common::InternalError(
          "DecodeIndirectDpiReturn", "unexpected indirect DPI return type");
  }
}

// Normalize an internal callable return value into a logical four-state pair.
// In 4-state mode the return is a {val, unk} struct. In --two-state mode
// the return is a plain integer (no X/Z tracking). Both shapes must produce
// a DpiFourStateValue for svLogicVecVal encoding at the DPI boundary.
auto NormalizeToDpiFourStateReturn(
    Context& context, llvm::Value* ret_val, uint32_t semantic_width)
    -> DpiFourStateValue {
  auto& b = context.GetBuilder();

  if (ret_val->getType()->isStructTy()) {
    auto* val = b.CreateExtractValue(ret_val, {0}, "dpi.ret.val");
    auto* unk = b.CreateExtractValue(ret_val, {1}, "dpi.ret.unk");
    return DpiFourStateValue{
        .val = val, .unk = unk, .semantic_width = semantic_width};
  }

  if (!ret_val->getType()->isIntegerTy()) {
    throw common::InternalError(
        "NormalizeToDpiFourStateReturn",
        "expected struct or integer return value for logical DPI indirect "
        "return");
  }

  auto* unk = llvm::ConstantInt::get(ret_val->getType(), 0);
  return DpiFourStateValue{
      .val = ret_val, .unk = unk, .semantic_width = semantic_width};
}

auto EncodeIndirectDpiReturn(
    Context& context, const mir::DpiReturnDesc& result, llvm::Value* ret_val)
    -> llvm::Value* {
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  uint32_t width = GetSemanticWidth(result.sv_type, context.GetTypeArena());

  switch (result.abi_type) {
    case DpiAbiTypeClass::kLogicVecNarrow:
    case DpiAbiTypeClass::kLogicVecWide: {
      DpiFourStateValue fsv =
          NormalizeToDpiFourStateReturn(context, ret_val, width);
      uint32_t wc = GetDpiLogicWordCount(width);
      auto* arr_ty = GetDpiLogicVecStorageType(llvm_ctx, wc);
      auto* word_ty = GetDpiLogicVecWordType(llvm_ctx);
      return BuildDpiLogicVecStorage(b, fsv, arr_ty, word_ty);
    }
    case DpiAbiTypeClass::kBitVecWide: {
      uint32_t wc = GetDpiLogicWordCount(width);
      auto* arr_ty = GetDpiBitVecStorageType(llvm_ctx, wc);
      return BuildDpiBitVecStorage(b, ret_val, width, arr_ty);
    }
    default:
      throw common::InternalError(
          "EncodeIndirectDpiReturn", "unexpected indirect DPI return type");
  }
}

auto CommitDpiReturnValue(
    Context& context, const mir::DpiCall& call, llvm::CallInst* call_inst,
    llvm::Value* indirect_ret_ptr) -> Result<void> {
  const auto& sig = call.callee.signature;
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  if (sig.result.kind == mir::DpiReturnKind::kVoid) {
    return {};
  }
  if (!call.ret) {
    throw common::InternalError(
        "CommitDpiReturnValue", "non-void DPI call missing CallReturn");
  }

  auto tmp_ptr = context.GetPlacePointer(call.ret->tmp);
  if (!tmp_ptr) return std::unexpected(tmp_ptr.error());
  auto tmp_type = context.GetPlaceLlvmType(call.ret->tmp);
  if (!tmp_type) return std::unexpected(tmp_type.error());

  if (sig.result.kind == mir::DpiReturnKind::kIndirect) {
    if (indirect_ret_ptr == nullptr) {
      throw common::InternalError(
          "CommitDpiReturnValue",
          "kIndirect DPI return missing staged result pointer");
    }
    llvm::Type* storage_ty = GetLlvmDpiStorageType(
        llvm_ctx, sig.result.abi_type, sig.result.sv_type, types);
    llvm::Value* raw =
        builder.CreateLoad(storage_ty, indirect_ret_ptr, "dpi.ret.raw");
    llvm::Value* decoded = DecodeIndirectDpiReturn(context, sig.result, raw);
    builder.CreateStore(decoded, *tmp_ptr);
  } else if (sig.result.abi_type == DpiAbiTypeClass::kLogicScalar) {
    uint32_t width = GetSemanticWidth(sig.result.sv_type, types);
    auto* packed =
        BuildLyraLogicScalarValue(builder, llvm_ctx, call_inst, width);
    builder.CreateStore(packed, *tmp_ptr);
  } else if (sig.result.abi_type == DpiAbiTypeClass::kString) {
    llvm::Value* handle = MaterializeStringWriteback(context, call_inst);
    builder.CreateStore(handle, *tmp_ptr);
  } else {
    llvm::Value* internal_result = CoerceFromDpiAbiType(
        context, call_inst, sig.result.abi_type, *tmp_type);
    builder.CreateStore(internal_result, *tmp_ptr);
  }

  if (call.ret->dest.has_value()) {
    llvm::Value* ret_val = builder.CreateLoad(*tmp_type, *tmp_ptr);
    return CommitValue(
        context, *call.ret->dest, ret_val, call.ret->type,
        OwnershipPolicy::kMove);
  }

  return {};
}

}  // namespace

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

auto GetLlvmScalarDpiType(llvm::LLVMContext& ctx, DpiAbiTypeClass t)
    -> llvm::Type* {
  switch (t) {
    case DpiAbiTypeClass::kBit:
      return llvm::Type::getInt8Ty(ctx);
    case DpiAbiTypeClass::kByte:
      return llvm::Type::getInt8Ty(ctx);
    case DpiAbiTypeClass::kShortInt:
      return llvm::Type::getInt16Ty(ctx);
    case DpiAbiTypeClass::kInt:
      return llvm::Type::getInt32Ty(ctx);
    case DpiAbiTypeClass::kLongInt:
      return llvm::Type::getInt64Ty(ctx);
    case DpiAbiTypeClass::kReal:
      return llvm::Type::getDoubleTy(ctx);
    case DpiAbiTypeClass::kShortReal:
      return llvm::Type::getFloatTy(ctx);
    case DpiAbiTypeClass::kString:
    case DpiAbiTypeClass::kChandle:
      return llvm::PointerType::getUnqual(ctx);
    case DpiAbiTypeClass::kVoid:
      return llvm::Type::getVoidTy(ctx);
    case DpiAbiTypeClass::kLogicScalar:
      return llvm::Type::getInt8Ty(ctx);
    case DpiAbiTypeClass::kLogicVecNarrow:
    case DpiAbiTypeClass::kLogicVecWide:
    case DpiAbiTypeClass::kBitVecWide:
      throw common::InternalError(
          "GetLlvmScalarDpiType",
          "packed-vector ABI class is not a scalar direct type");
    case DpiAbiTypeClass::kInvalid:
      break;
  }
  throw common::InternalError(
      "GetLlvmScalarDpiType", "invalid DPI ABI type class");
}

auto GetLlvmDpiStorageType(
    llvm::LLVMContext& ctx, DpiAbiTypeClass abi_type, TypeId sv_type,
    const TypeArena& types) -> llvm::Type* {
  if (abi_type == DpiAbiTypeClass::kLogicVecNarrow ||
      abi_type == DpiAbiTypeClass::kLogicVecWide) {
    uint32_t width = GetSemanticWidth(sv_type, types);
    return GetDpiLogicVecStorageType(ctx, GetDpiLogicWordCount(width));
  }
  if (abi_type == DpiAbiTypeClass::kBitVecWide) {
    uint32_t width = GetSemanticWidth(sv_type, types);
    return GetDpiBitVecStorageType(ctx, GetDpiLogicWordCount(width));
  }
  return GetLlvmScalarDpiType(ctx, abi_type);
}

auto BuildDpiImportFunctionType(
    Context& context, const mir::DpiSignature& sig, bool is_context)
    -> llvm::FunctionType* {
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  bool indirect_ret = sig.result.kind == mir::DpiReturnKind::kIndirect;

  std::vector<llvm::Type*> param_types;
  param_types.reserve(
      sig.params.size() + (is_context ? 1 : 0) + (indirect_ret ? 1 : 0));
  if (is_context) {
    param_types.push_back(llvm::PointerType::getUnqual(llvm_ctx));
  }
  if (indirect_ret) {
    llvm::Type* storage_ty = GetLlvmDpiStorageType(
        llvm_ctx, sig.result.abi_type, sig.result.sv_type, types);
    param_types.push_back(llvm::PointerType::getUnqual(storage_ty));
  }
  for (const auto& p : sig.params) {
    if (p.passing == mir::DpiPassingMode::kByPointer) {
      param_types.push_back(llvm::PointerType::getUnqual(llvm_ctx));
    } else {
      param_types.push_back(
          GetLlvmDpiStorageType(llvm_ctx, p.abi_type, p.sv_type, types));
    }
  }

  llvm::Type* ret_type =
      indirect_ret ? llvm::Type::getVoidTy(llvm_ctx)
                   : GetLlvmScalarDpiType(llvm_ctx, sig.result.abi_type);
  return llvm::FunctionType::get(ret_type, param_types, false);
}

auto GetOrDeclareDpiImport(
    Context& context, const std::string& c_name, const mir::DpiSignature& sig,
    bool is_context) -> llvm::Function* {
  mir::ValidateDpiSignatureContract(sig, "GetOrDeclareDpiImport");

  auto& module = context.GetModule();
  auto* fn_type = BuildDpiImportFunctionType(context, sig, is_context);

  if (auto* existing = module.getFunction(c_name)) {
    if (existing->getFunctionType() != fn_type) {
      throw common::InternalError(
          "GetOrDeclareDpiImport",
          std::format(
              "existing declaration for '{}' has mismatched DPI ABI", c_name));
    }
    if (existing->getCallingConv() != llvm::CallingConv::C) {
      throw common::InternalError(
          "GetOrDeclareDpiImport",
          std::format(
              "existing declaration for '{}' has non-C calling convention",
              c_name));
    }
    return existing;
  }

  auto* fn = llvm::Function::Create(
      fn_type, llvm::Function::ExternalLinkage, c_name, &module);
  fn->setCallingConv(llvm::CallingConv::C);
  return fn;
}

// Single source of truth for the caller's DPI scope at a context import
// call site.
// In Lyra runtime, RuntimeInstance* is the canonical svScope handle.
// Module-scoped lowering already carries that exact pointer in instance_ptr.
// All other contexts (package scope, compilation-unit scope) get null.
auto ResolveDpiCallerScope(Context& context) -> llvm::Value* {
  if (llvm::Value* inst = context.GetInstancePointer()) {
    return inst;
  }
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  return llvm::ConstantPointerNull::get(ptr_ty);
}

// State for the push/call/pop region of a context import call.
struct DpiContextCallState {
  llvm::Value* caller_scope = nullptr;
  llvm::Value* prev_scope = nullptr;
};

// Emit push: resolve caller scope, push it into runtime context,
// prepend it to abi_args. Returns state for EndContextImportCall.
//
// Must remain infallible.
// No type validation, no ABI classification, no temporary allocation logic.
// Only: resolve caller scope, emit push call, prepend ABI arg.
auto BeginContextImportCall(
    Context& context, const mir::DpiImportRef& ref,
    std::vector<llvm::Value*>& abi_args) -> DpiContextCallState {
  DpiContextCallState st;
  if (!ref.is_context) return st;

  auto& builder = context.GetBuilder();

  // Single source of truth: both ABI arg and runtime push use this value.
  st.caller_scope = ResolveDpiCallerScope(context);

  // Runtime query obligation: push caller scope into active context.
  auto* push_fn = context.GetLyraPushCurrentDpiScope();
  auto* push_call =
      builder.CreateCall(push_fn, {st.caller_scope}, "dpi.prev.scope");
  static_cast<llvm::CallInst*>(push_call)->setCallingConv(llvm::CallingConv::C);
  st.prev_scope = push_call;

  // C ABI obligation: prepend caller scope as hidden first parameter.
  abi_args.push_back(st.caller_scope);

  return st;
}

// Emit pop: restore previous scope after foreign call returns.
void EndContextImportCall(Context& context, const DpiContextCallState& st) {
  if (st.prev_scope == nullptr) return;

  auto& builder = context.GetBuilder();
  auto* pop_fn = context.GetLyraPopCurrentDpiScope();
  auto* pop_call = builder.CreateCall(pop_fn, {st.prev_scope});
  pop_call->setCallingConv(llvm::CallingConv::C);
}

auto LowerDpiImportCall(Context& context, const mir::DpiCall& call)
    -> Result<void> {
  const auto& ref = call.callee;
  const auto& sig = ref.signature;
  mir::ValidateDpiSignatureContract(sig, "LowerDpiImportCall");
  mir::ValidateDpiCallContract(sig, call.args, "LowerDpiImportCall");

  // Phase A: all fallible work completes before scope push.
  llvm::Function* fn =
      GetOrDeclareDpiImport(context, ref.c_name, sig, ref.is_context);
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  std::vector<llvm::Value*> user_abi_args;
  std::vector<DpiArgLoweringState> arg_states(call.args.size());
  user_abi_args.reserve(call.args.size());

  auto prep_result =
      PrepareDpiCallArgs(context, call, user_abi_args, arg_states);
  if (!prep_result) return std::unexpected(prep_result.error());

  // Indirect return: allocate staged result storage.
  llvm::Value* indirect_ret_ptr = nullptr;
  if (sig.result.kind == mir::DpiReturnKind::kIndirect) {
    llvm::Type* storage_ty = GetLlvmDpiStorageType(
        llvm_ctx, sig.result.abi_type, sig.result.sv_type, types);
    indirect_ret_ptr =
        CreateDpiStagedAlloca(builder, storage_ty, "dpi.ret.staged");
  }

  // --- Non-fallible region: push -> call -> pop ---

  // Context import prologue: push scope, prepend ABI arg.
  std::vector<llvm::Value*> abi_args;
  auto ctx_state = BeginContextImportCall(context, ref, abi_args);

  // Indirect result pointer in canonical slot (after scope, before user args).
  if (indirect_ret_ptr != nullptr) {
    abi_args.push_back(indirect_ret_ptr);
  }

  // Append user args.
  abi_args.insert(abi_args.end(), user_abi_args.begin(), user_abi_args.end());

  // Phase B: emit foreign call.
  auto* call_inst = builder.CreateCall(fn, abi_args);
  call_inst->setCallingConv(llvm::CallingConv::C);

  // Context import epilogue: pop scope.
  EndContextImportCall(context, ctx_state);

  // --- End non-fallible region ---

  // Phase C.1: writeback output/inout params.
  auto wb_result = CommitDpiOutputWritebacks(context, call, arg_states);
  if (!wb_result) return std::unexpected(wb_result.error());

  // Phase C.2: handle return value.
  return CommitDpiReturnValue(context, call, call_inst, indirect_ret_ptr);
}

namespace {

// Build the LLVM function type for a DPI export wrapper from its DpiSignature.
// By-value params use their scalar LLVM type, by-pointer params use ptr.
// Only the hidden indirect-return pointer is newly typed via
// GetLlvmDpiStorageType(). Existing user parameter ABI formation
// remains unchanged.
auto BuildExportWrapperType(
    llvm::LLVMContext& ctx, const mir::DpiSignature& sig,
    const TypeArena& types) -> llvm::FunctionType* {
  bool indirect_ret = sig.result.kind == mir::DpiReturnKind::kIndirect;

  llvm::Type* ret_ty =
      (indirect_ret || sig.result.kind == mir::DpiReturnKind::kVoid)
          ? llvm::Type::getVoidTy(ctx)
          : GetLlvmScalarDpiType(ctx, sig.result.abi_type);

  std::vector<llvm::Type*> param_types;
  param_types.reserve(sig.params.size() + (indirect_ret ? 1 : 0));
  if (indirect_ret) {
    llvm::Type* storage_ty = GetLlvmDpiStorageType(
        ctx, sig.result.abi_type, sig.result.sv_type, types);
    param_types.push_back(llvm::PointerType::getUnqual(storage_ty));
  }
  for (const auto& p : sig.params) {
    if (p.passing == mir::DpiPassingMode::kByPointer) {
      param_types.push_back(llvm::PointerType::getUnqual(ctx));
    } else {
      param_types.push_back(GetLlvmScalarDpiType(ctx, p.abi_type));
    }
  }
  return llvm::FunctionType::get(ret_ty, param_types, false);
}

// Export parameter lowering classification.
// kDirectValue: 2-state scalar by-value (CoerceFromDpiAbiType).
// kLogicScalar: 4-state scalar by-value (DecodeSvLogic path).
// kByPointer: all by-pointer params -- packed vectors and scalar
//   output/inout. Uses staged decode/writeback.
enum class ExportParamLoweringKind : uint8_t {
  kDirectValue,
  kLogicScalar,
  kByPointer,
};

struct ExportParamPlan {
  ExportParamLoweringKind kind = ExportParamLoweringKind::kDirectValue;
  ParameterDirection dir = ParameterDirection::kInput;
  DpiAbiTypeClass abi_type = DpiAbiTypeClass::kInvalid;
  TypeId sv_type;
};

auto PlanExportParam(const mir::DpiParamDesc& param) -> ExportParamPlan {
  ExportParamLoweringKind kind = ExportParamLoweringKind::kDirectValue;
  if (param.passing == mir::DpiPassingMode::kByPointer) {
    kind = ExportParamLoweringKind::kByPointer;
  } else if (param.abi_type == DpiAbiTypeClass::kLogicScalar) {
    kind = ExportParamLoweringKind::kLogicScalar;
  } else {
    kind = ExportParamLoweringKind::kDirectValue;
  }
  return {
      .kind = kind,
      .dir = param.direction,
      .abi_type = param.abi_type,
      .sv_type = param.sv_type,
  };
}

struct ExportByPointerState {
  const mir::DpiParamDesc* param = nullptr;
  llvm::Value* foreign_ptr = nullptr;
  llvm::AllocaInst* staged_alloca = nullptr;
};

auto LoadForeignDpiStorage(
    Context& context, llvm::Value* foreign_ptr, llvm::Type* storage_ty)
    -> llvm::Value* {
  auto& b = context.GetBuilder();
  if (!foreign_ptr->getType()->isPointerTy()) {
    throw common::InternalError(
        "LoadForeignDpiStorage",
        "expected pointer type for foreign DPI storage");
  }
  llvm::Value* typed_ptr = foreign_ptr;
  auto* wanted_ptr_ty = storage_ty->getPointerTo();
  if (foreign_ptr->getType() != wanted_ptr_ty) {
    typed_ptr = b.CreateBitCast(foreign_ptr, wanted_ptr_ty, "exp.foreign.cast");
  }
  return b.CreateLoad(storage_ty, typed_ptr, "exp.foreign.load");
}

void StoreForeignDpiStorage(
    Context& context, llvm::Value* foreign_ptr, llvm::Value* encoded) {
  auto& b = context.GetBuilder();
  if (!foreign_ptr->getType()->isPointerTy()) {
    throw common::InternalError(
        "StoreForeignDpiStorage",
        "expected pointer type for foreign DPI storage");
  }
  llvm::Value* typed_ptr = foreign_ptr;
  auto* wanted_ptr_ty = encoded->getType()->getPointerTo();
  if (foreign_ptr->getType() != wanted_ptr_ty) {
    typed_ptr =
        b.CreateBitCast(foreign_ptr, wanted_ptr_ty, "exp.foreign.store.cast");
  }
  b.CreateStore(encoded, typed_ptr);
}

auto DecodeExportByPointerArg(
    Context& context, llvm::Value* foreign_ptr, const mir::DpiParamDesc& param)
    -> llvm::Value* {
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  llvm::Type* storage_ty = GetLlvmDpiStorageType(
      llvm_ctx, param.abi_type, param.sv_type, context.GetTypeArena());
  llvm::Value* raw = LoadForeignDpiStorage(context, foreign_ptr, storage_ty);
  if (IsFourStateVecDpiType(param.abi_type)) {
    uint32_t width = GetSemanticWidth(param.sv_type, context.GetTypeArena());
    auto* backing_ty = GetBackingLlvmType(llvm_ctx, width);
    return DecodeDpiLogicVecStorage(b, raw, width, backing_ty);
  }
  if (param.abi_type == DpiAbiTypeClass::kBitVecWide) {
    uint32_t width = GetSemanticWidth(param.sv_type, context.GetTypeArena());
    auto* backing_ty = GetBackingLlvmType(llvm_ctx, width);
    return DecodeDpiBitVecStorage(b, raw, width, backing_ty);
  }
  throw common::InternalError(
      "DecodeExportByPointerArg",
      std::format(
          "non-packed ABI class {} routed to packed export decoder",
          static_cast<int>(param.abi_type)));
}

auto ToI1(llvm::IRBuilder<>& b, llvm::Value* v) -> llvm::Value* {
  auto* i1 = llvm::Type::getInt1Ty(b.getContext());
  if (v->getType() == i1) return v;
  return b.CreateTrunc(v, i1);
}

auto EncodeExportByPointerValue(
    Context& context, llvm::Value* canonical, const mir::DpiParamDesc& param)
    -> llvm::Value* {
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  if (IsFourStateVecDpiType(param.abi_type)) {
    uint32_t width = GetSemanticWidth(param.sv_type, context.GetTypeArena());
    auto* val = b.CreateExtractValue(canonical, {0}, "exp.enc.val");
    auto* unk = b.CreateExtractValue(canonical, {1}, "exp.enc.unk");
    uint32_t wc = GetDpiLogicWordCount(width);
    auto* arr_ty = GetDpiLogicVecStorageType(llvm_ctx, wc);
    auto* word_ty = GetDpiLogicVecWordType(llvm_ctx);
    DpiFourStateValue fsv{.val = val, .unk = unk, .semantic_width = width};
    return BuildDpiLogicVecStorage(b, fsv, arr_ty, word_ty);
  }
  if (param.abi_type == DpiAbiTypeClass::kBitVecWide) {
    uint32_t width = GetSemanticWidth(param.sv_type, context.GetTypeArena());
    uint32_t wc = GetDpiLogicWordCount(width);
    auto* arr_ty = GetDpiBitVecStorageType(llvm_ctx, wc);
    return BuildDpiBitVecStorage(b, canonical, width, arr_ty);
  }
  throw common::InternalError(
      "EncodeExportByPointerValue",
      std::format(
          "non-packed ABI class {} routed to packed export encoder",
          static_cast<int>(param.abi_type)));
}

}  // namespace

void PrepareExportUserArgs(
    Context& context, const mir::DpiSignature& sig, llvm::Function* wrapper,
    llvm::Function* internal_fn, unsigned user_arg_base,
    unsigned wrapper_arg_base, std::vector<llvm::Value*>& internal_args,
    std::vector<ExportByPointerState>& by_pointer_states) {
  auto& b = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  for (size_t i = 0; i < sig.params.size(); ++i) {
    llvm::Value* wrapper_arg =
        wrapper->getArg(wrapper_arg_base + static_cast<unsigned>(i));
    const auto& param = sig.params[i];
    auto plan = PlanExportParam(param);
    unsigned internal_idx = static_cast<unsigned>(i) + user_arg_base;
    llvm::Type* internal_ty =
        internal_fn->getFunctionType()->getParamType(internal_idx);

    switch (plan.kind) {
      case ExportParamLoweringKind::kDirectValue: {
        llvm::Value* coerced = CoerceFromDpiAbiType(
            context, wrapper_arg, param.abi_type, internal_ty);
        internal_args.push_back(coerced);
        break;
      }

      case ExportParamLoweringKind::kLogicScalar: {
        if (internal_ty->isPointerTy()) {
          throw common::InternalError(
              "PrepareExportUserArgs",
              "logic-scalar export param unexpectedly lowered as pointer");
        }
        if (internal_ty->isStructTy()) {
          uint32_t width =
              GetSemanticWidth(param.sv_type, context.GetTypeArena());
          llvm::Value* decoded =
              BuildLyraLogicScalarValue(b, llvm_ctx, wrapper_arg, width);
          internal_args.push_back(decoded);
        } else {
          auto [val, unk] = DecodeSvLogic(b, wrapper_arg);
          llvm::Value* coerced = CoerceFromDpiAbiType(
              context, val, DpiAbiTypeClass::kBit, internal_ty);
          internal_args.push_back(coerced);
        }
        break;
      }

      case ExportParamLoweringKind::kByPointer: {
        bool internal_expects_ptr = internal_ty->isPointerTy();
        bool dir_has_output = param.direction == ParameterDirection::kOutput ||
                              param.direction == ParameterDirection::kInOut;
        bool dir_input_only = param.direction == ParameterDirection::kInput;

        if (dir_has_output && !internal_expects_ptr) {
          throw common::InternalError(
              "PrepareExportUserArgs",
              "output/inout export param unexpectedly lowered as value");
        }
        if (dir_input_only && internal_expects_ptr) {
          throw common::InternalError(
              "PrepareExportUserArgs",
              "input-only export param unexpectedly lowered as pointer");
        }

        // Canonical value type for this DPI parameter, derived from the same
        // callable-ABI builder that defined the internal callable signature.
        // The wrapper must reuse that ABI source of truth instead of deriving
        // an independent local type model.
        auto* expected_value_ty = GetCallableAbiLlvmType(
            llvm_ctx, param.sv_type, context.GetTypeArena(),
            context.IsForceTwoState());

        bool is_packed = IsPackedVecDpiType(param.abi_type);

        llvm::Value* canonical = nullptr;
        if (!dir_has_output || param.direction == ParameterDirection::kInOut) {
          if (is_packed) {
            canonical = DecodeExportByPointerArg(context, wrapper_arg, param);
          } else {
            llvm::Type* storage_ty = GetLlvmDpiStorageType(
                llvm_ctx, param.abi_type, param.sv_type,
                context.GetTypeArena());
            llvm::Value* raw =
                LoadForeignDpiStorage(context, wrapper_arg, storage_ty);
            if (param.abi_type == DpiAbiTypeClass::kLogicScalar) {
              uint32_t width =
                  GetSemanticWidth(param.sv_type, context.GetTypeArena());
              canonical = BuildLyraLogicScalarValue(b, llvm_ctx, raw, width);
            } else if (param.abi_type == DpiAbiTypeClass::kString) {
              canonical = MaterializeStringWriteback(context, raw);
            } else {
              canonical = CoerceFromDpiAbiType(
                  context, raw, param.abi_type, expected_value_ty);
            }
          }
        }
        if (canonical == nullptr) {
          canonical = llvm::Constant::getNullValue(expected_value_ty);
        }

        if (canonical->getType() != expected_value_ty) {
          throw common::InternalError(
              "PrepareExportUserArgs",
              "decoded export argument type does not match internal "
              "callable ABI");
        }

        if (internal_expects_ptr) {
          auto* alloca =
              CreateDpiStagedAlloca(b, expected_value_ty, "exp.bp.staged");
          b.CreateStore(canonical, alloca);
          internal_args.push_back(alloca);
          if (dir_has_output) {
            by_pointer_states.push_back({
                .param = &sig.params[i],
                .foreign_ptr = wrapper_arg,
                .staged_alloca = alloca,
            });
          }
        } else {
          internal_args.push_back(canonical);
        }
        break;
      }
    }
  }
}

void CommitExportByPointerWritebacks(
    Context& context, const std::vector<ExportByPointerState>& states) {
  auto& b = context.GetBuilder();

  for (const auto& state : states) {
    const auto& param = *state.param;
    llvm::Type* canonical_ty = state.staged_alloca->getAllocatedType();
    llvm::Value* canonical =
        b.CreateLoad(canonical_ty, state.staged_alloca, "exp.wb.canon");

    if (IsPackedVecDpiType(param.abi_type)) {
      llvm::Value* encoded =
          EncodeExportByPointerValue(context, canonical, param);
      StoreForeignDpiStorage(context, state.foreign_ptr, encoded);
    } else if (param.abi_type == DpiAbiTypeClass::kLogicScalar) {
      auto* val = b.CreateExtractValue(canonical, {0}, "exp.wb.ls.val");
      auto* unk = b.CreateExtractValue(canonical, {1}, "exp.wb.ls.unk");
      llvm::Value* encoded = EncodeSvLogic(b, ToI1(b, val), ToI1(b, unk));
      StoreForeignDpiStorage(context, state.foreign_ptr, encoded);
    } else if (param.abi_type == DpiAbiTypeClass::kString) {
      llvm::Value* encoded = MarshalInputString(context, canonical);
      StoreForeignDpiStorage(context, state.foreign_ptr, encoded);
    } else {
      llvm::Value* encoded =
          CoerceToDpiAbiType(context, canonical, param.abi_type);
      StoreForeignDpiStorage(context, state.foreign_ptr, encoded);
    }
  }
}

void EmitExportReturn(
    Context& context, const mir::DpiSignature& sig, llvm::Value* ret_val,
    llvm::Value* indirect_ret_ptr) {
  auto& b = context.GetBuilder();
  if (sig.result.kind == mir::DpiReturnKind::kVoid) {
    b.CreateRetVoid();
    return;
  }
  if (sig.result.kind == mir::DpiReturnKind::kIndirect) {
    if (indirect_ret_ptr == nullptr) {
      throw common::InternalError(
          "EmitExportReturn",
          "kIndirect export return missing wrapper result pointer");
    }
    llvm::Value* encoded =
        EncodeIndirectDpiReturn(context, sig.result, ret_val);
    StoreForeignDpiStorage(context, indirect_ret_ptr, encoded);
    b.CreateRetVoid();
    return;
  }
  if (sig.result.abi_type == DpiAbiTypeClass::kLogicScalar) {
    if (ret_val->getType()->isStructTy()) {
      auto* val = b.CreateExtractValue(ret_val, {0}, "exp.ret.val");
      auto* unk = b.CreateExtractValue(ret_val, {1}, "exp.ret.unk");
      llvm::Value* encoded = EncodeSvLogic(b, ToI1(b, val), ToI1(b, unk));
      b.CreateRet(encoded);
    } else {
      llvm::Value* encoded =
          CoerceToDpiAbiType(context, ret_val, DpiAbiTypeClass::kBit);
      b.CreateRet(encoded);
    }
    return;
  }
  llvm::Value* encoded =
      CoerceToDpiAbiType(context, ret_val, sig.result.abi_type);
  b.CreateRet(encoded);
}

// Emit package-scoped export wrapper binding resolution.
// Calls LyraResolvePackageExportBinding and extracts design_state + engine.
struct PackageBindingValues {
  llvm::Value* design_ptr;
  llvm::Value* engine_ptr;
};

auto EmitResolvePackageBinding(Context& context) -> PackageBindingValues {
  auto& llvm_ctx = context.GetLlvmContext();
  auto& b = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  // DpiResolvedPackageBinding: { ptr design_state, ptr engine }
  auto* binding_ty = llvm::StructType::get(ptr_ty, ptr_ty);
  auto* alloca = b.CreateAlloca(binding_ty, nullptr, "pkg.binding");
  auto* resolve_fn = context.GetLyraResolvePackageExportBinding();
  auto* call = b.CreateCall(resolve_fn, {alloca});
  call->setCallingConv(llvm::CallingConv::C);

  return {
      .design_ptr = b.CreateLoad(
          ptr_ty, b.CreateStructGEP(binding_ty, alloca, 0), "pkg.design"),
      .engine_ptr = b.CreateLoad(
          ptr_ty, b.CreateStructGEP(binding_ty, alloca, 1), "pkg.engine"),
  };
}

// Emit module-scoped export wrapper binding resolution.
// Calls LyraResolveModuleInstanceBinding(out) and loads fields from out.
struct ModuleBindingValues {
  llvm::Value* design_ptr;
  llvm::Value* engine_ptr;
  llvm::Value* this_ptr;
  llvm::Value* instance_ptr;
  llvm::Value* instance_id;
};

auto EmitResolveModuleBinding(Context& context) -> ModuleBindingValues {
  auto& llvm_ctx = context.GetLlvmContext();
  auto& b = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  // DpiResolvedModuleBinding: { ptr, ptr, ptr, ptr, i32 }
  auto* binding_ty =
      llvm::StructType::get(ptr_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty);
  auto* alloca = b.CreateAlloca(binding_ty, nullptr, "mod.binding");
  auto* resolve_fn = context.GetLyraResolveModuleInstanceBinding();
  auto* call = b.CreateCall(resolve_fn, {alloca});
  call->setCallingConv(llvm::CallingConv::C);

  return {
      .design_ptr = b.CreateLoad(
          ptr_ty, b.CreateStructGEP(binding_ty, alloca, 0), "mod.design"),
      .engine_ptr = b.CreateLoad(
          ptr_ty, b.CreateStructGEP(binding_ty, alloca, 1), "mod.engine"),
      .this_ptr = b.CreateLoad(
          ptr_ty, b.CreateStructGEP(binding_ty, alloca, 2), "mod.this"),
      .instance_ptr = b.CreateLoad(
          ptr_ty, b.CreateStructGEP(binding_ty, alloca, 3), "mod.instance"),
      .instance_id = b.CreateLoad(
          i32_ty, b.CreateStructGEP(binding_ty, alloca, 4), "mod.id"),
  };
}

auto EmitDpiExportWrappers(
    Context& context, const std::vector<mir::DpiExportWrapperDesc>& exports,
    const std::unordered_map<
        mir::ModuleExportCalleeKey, ModuleExportCalleeInfo,
        mir::ModuleExportCalleeKeyHash>& module_export_callees)
    -> Result<void> {
  if (exports.empty()) return {};

  auto& llvm_ctx = context.GetLlvmContext();
  auto& module = context.GetModule();

  const auto& types = context.GetTypeArena();

  for (const auto& desc : exports) {
    // Build wrapper function (same C ABI shape for both package and module).
    auto* wrapper_ty = BuildExportWrapperType(llvm_ctx, desc.signature, types);
    auto* wrapper = llvm::Function::Create(
        wrapper_ty, llvm::Function::ExternalLinkage, desc.c_name, &module);
    wrapper->setCallingConv(llvm::CallingConv::C);

    auto* entry_bb = llvm::BasicBlock::Create(llvm_ctx, "entry", wrapper);
    context.GetBuilder().SetInsertPoint(entry_bb);

    // Consume hidden wrapper args in canonical order.
    unsigned wrapper_arg_idx = 0;
    llvm::Value* indirect_ret_ptr = nullptr;
    if (desc.signature.result.kind == mir::DpiReturnKind::kIndirect) {
      indirect_ret_ptr = wrapper->getArg(wrapper_arg_idx++);
    }

    // Determine if this is a task export (D7a suspension guard).
    bool is_task = desc.routine_kind == mir::DpiRoutineKind::kTask;

    if (desc.target.scope_kind == mir::DpiExportScopeKind::kPackage) {
      // Package path: resolve binding + direct call to design-global callee.
      auto binding = EmitResolvePackageBinding(context);
      const auto& entry = context.GetDesignFunction(desc.target.package_symbol);

      // Reject if callee accepts process ownership (either directly from
      // process-owned effects or transitively via call graph propagation).
      // DPI wrappers have no process context to provide.
      const auto& callee_func = context.GetDesignArena()[entry.func_id];
      if (callee_func.abi_contract.accepts_process_ownership) {
        return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
            callee_func.origin,
            std::format(
                "DPI export '{}' targets a function that requires process "
                "execution context (e.g. contains unique/priority case); "
                "this is not supported from DPI call paths",
                desc.c_name),
            UnsupportedCategory::kFeature));
      }

      llvm::Function* internal_fn = entry.llvm_func;
      if (internal_fn == nullptr) {
        throw common::InternalError(
            "EmitDpiExportWrappers",
            std::format(
                "internal function for package DPI export '{}' is null",
                desc.c_name));
      }

      // Push per-wrapper-call context (D7a: suspension_disallowed for tasks).
      // Inherits design/engine/scope from the current simulation-lifetime head.
      auto& builder = context.GetBuilder();
      builder.CreateCall(
          context.GetLyraPushDpiExportCallContext(),
          {builder.getInt1(is_task)});

      std::vector<llvm::Value*> args;
      args.push_back(binding.design_ptr);
      args.push_back(binding.engine_ptr);
      std::vector<ExportByPointerState> bp_states;
      PrepareExportUserArgs(
          context, desc.signature, wrapper, internal_fn, 2, wrapper_arg_idx,
          args, bp_states);
      auto* ret_val = builder.CreateCall(internal_fn, args);
      CommitExportByPointerWritebacks(context, bp_states);

      builder.CreateCall(context.GetLyraPopDpiExportCallContext());
      EmitExportReturn(context, desc.signature, ret_val, indirect_ret_ptr);

    } else {
      // Module path: resolve instance binding + direct call to module callee.
      auto binding = EmitResolveModuleBinding(context);
      auto callee_it = module_export_callees.find(desc.target.module_target);
      if (callee_it == module_export_callees.end()) {
        throw common::InternalError(
            "EmitDpiExportWrappers",
            std::format(
                "module callee for DPI export '{}' not found in accumulator",
                desc.c_name));
      }
      const auto& callee_info = callee_it->second;

      // Reject if callee accepts process ownership (DPI wrappers have none).
      if (callee_info.accepts_process_ownership) {
        return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
            common::OriginId::Invalid(),
            std::format(
                "DPI export '{}' targets a function that requires process "
                "execution context (e.g. contains unique/priority case); "
                "this is not supported from DPI call paths",
                desc.c_name),
            UnsupportedCategory::kFeature));
      }

      llvm::Function* internal_fn = callee_info.llvm_func;

      // Push per-wrapper-call context (D7a: suspension_disallowed for tasks).
      // Inherits design/engine/scope from the current simulation-lifetime head.
      auto& builder = context.GetBuilder();
      builder.CreateCall(
          context.GetLyraPushDpiExportCallContext(),
          {builder.getInt1(is_task)});

      std::vector<llvm::Value*> args;
      args.push_back(binding.design_ptr);
      args.push_back(binding.engine_ptr);
      args.push_back(binding.this_ptr);
      args.push_back(binding.instance_ptr);
      args.push_back(binding.instance_id);
      std::vector<ExportByPointerState> bp_states;
      PrepareExportUserArgs(
          context, desc.signature, wrapper, internal_fn, 5, wrapper_arg_idx,
          args, bp_states);
      auto* ret_val = builder.CreateCall(internal_fn, args);
      CommitExportByPointerWritebacks(context, bp_states);

      builder.CreateCall(context.GetLyraPopDpiExportCallContext());
      EmitExportReturn(context, desc.signature, ret_val, indirect_ret_ptr);
    }
  }

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm::dpi
