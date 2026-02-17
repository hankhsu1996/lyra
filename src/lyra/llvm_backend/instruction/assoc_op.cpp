#include "lyra/llvm_backend/instruction/assoc_op.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/type_query.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/runtime/assoc_map.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct KeyArgs {
  llvm::Value* key_a;
  llvm::Value* key_b;
  llvm::Value* key_c;
};

auto PlaceType(Context& ctx, mir::PlaceId place_id) -> TypeId {
  return mir::TypeOfPlace(ctx.GetTypeArena(), ctx.GetMirArena()[place_id]);
}

auto GetAATypeInfo(Context& ctx, mir::PlaceId receiver)
    -> std::pair<TypeId, const AssociativeArrayInfo&> {
  const auto& types = ctx.GetTypeArena();
  TypeId receiver_type = PlaceType(ctx, receiver);
  const auto& aa_info = types[receiver_type].AsAssociativeArray();
  return {receiver_type, aa_info};
}

auto GetKeyType(Context& ctx, mir::PlaceId receiver) -> TypeId {
  auto [_, aa_info] = GetAATypeInfo(ctx, receiver);
  return aa_info.key_type;
}

auto GetElemType(Context& ctx, mir::PlaceId receiver) -> TypeId {
  auto [_, aa_info] = GetAATypeInfo(ctx, receiver);
  return aa_info.element_type;
}

// Determine LyraAssocElemKind for a type
auto ClassifyElemKind(
    TypeId elem_type, const TypeArena& types, bool force_two_state)
    -> uint32_t {
  const auto& type = types[elem_type];
  switch (type.Kind()) {
    case TypeKind::kString:
      return kLyraAssocElemString;
    case TypeKind::kDynamicArray:
      return kLyraAssocElemDynArray;
    case TypeKind::kQueue:
      return kLyraAssocElemQueue;
    case TypeKind::kAssociativeArray:
      return kLyraAssocElemAssocArray;
    default:
      return IsFourState(elem_type, types, force_two_state)
                 ? kLyraAssocElemPod4State
                 : kLyraAssocElemPod;
  }
}

// Compute element byte size using DataLayout
auto GetElemByteSize(Context& ctx, TypeId elem_type) -> uint32_t {
  const auto& types = ctx.GetTypeArena();
  const auto& type = types[elem_type];
  // Handle types are pointer-sized
  if (type.Kind() == TypeKind::kString ||
      type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue ||
      type.Kind() == TypeKind::kAssociativeArray) {
    return static_cast<uint32_t>(
        ctx.GetModule().getDataLayout().getPointerSize());
  }
  auto llvm_type = BuildLlvmTypeForTypeId(ctx, elem_type);
  if (!llvm_type) {
    throw common::InternalError("GetElemByteSize", "failed to build LLVM type");
  }
  return static_cast<uint32_t>(
      ctx.GetModule().getDataLayout().getTypeAllocSize(*llvm_type));
}

// Lower a key operand to the (key_a, key_b, key_c) ABI triple.
auto LowerKey(Context& ctx, const mir::Operand& key_op, TypeId key_type)
    -> Result<KeyArgs> {
  auto& builder = ctx.GetBuilder();
  const auto& types = ctx.GetTypeArena();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  // Determine if string or integral key
  bool is_string_key = false;
  if (key_type) {
    const auto& kt = types[key_type];
    is_string_key = (kt.Kind() == TypeKind::kString);
  }

  if (is_string_key) {
    // String key: load handle, get view
    auto raw = LowerOperandRaw(ctx, key_op);
    if (!raw) return std::unexpected(raw.error());

    // Alloca for ptr + len output from LyraStringGetView
    auto* str_ptr_alloca =
        builder.CreateAlloca(ptr_ty, nullptr, "aa.key.str.ptr");
    auto* i64_ty = llvm::Type::getInt64Ty(ctx.GetLlvmContext());
    auto* str_len_alloca =
        builder.CreateAlloca(i64_ty, nullptr, "aa.key.str.len");
    builder.CreateCall(
        ctx.GetLyraStringGetView(), {*raw, str_ptr_alloca, str_len_alloca});
    auto* str_ptr = builder.CreateLoad(ptr_ty, str_ptr_alloca, "aa.key.ptr");
    auto* str_len = builder.CreateLoad(i64_ty, str_len_alloca, "aa.key.len");
    auto* str_len32 = builder.CreateTrunc(str_len, i32_ty, "aa.key.len32");

    return KeyArgs{
        .key_a = str_ptr,
        .key_b = llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptr_ty)),
        .key_c = str_len32,
    };
  }

  // Integral key: store iN to alloca, pass pointer
  auto raw = LowerOperandRaw(ctx, key_op);
  if (!raw) return std::unexpected(raw.error());

  auto* raw_val = *raw;
  auto* raw_ty = raw_val->getType();

  // Determine bit width and whether it's 4-state
  bool is_four_state = raw_ty->isStructTy();
  llvm::Value* value_part = nullptr;
  llvm::Value* xz_part = nullptr;

  if (is_four_state) {
    value_part = builder.CreateExtractValue(raw_val, 0, "aa.key.val");
    xz_part = builder.CreateExtractValue(raw_val, 1, "aa.key.xz");
  } else {
    value_part = raw_val;
  }

  uint32_t bit_width = value_part->getType()->getIntegerBitWidth();
  uint32_t num_words = (bit_width + 63) / 64;
  uint32_t alloca_bytes = num_words * 8;

  // Create alloca, zero it, store value.
  // Use [N x i64] to guarantee 8-byte alignment for runtime word reads.
  auto* i64_ty = llvm::Type::getInt64Ty(ctx.GetLlvmContext());
  auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
  auto* val_alloca = builder.CreateAlloca(
      llvm::ArrayType::get(i64_ty, num_words), nullptr, "aa.key.words");
  builder.CreateMemSet(
      val_alloca, llvm::ConstantInt::get(i8_ty, 0), alloca_bytes,
      llvm::MaybeAlign(8));
  builder.CreateStore(value_part, val_alloca);

  llvm::Value* xz_ptr = nullptr;
  if (is_four_state) {
    auto* xz_alloca = builder.CreateAlloca(
        llvm::ArrayType::get(i64_ty, num_words), nullptr, "aa.key.xz.words");
    builder.CreateMemSet(
        xz_alloca, llvm::ConstantInt::get(i8_ty, 0), alloca_bytes,
        llvm::MaybeAlign(8));
    builder.CreateStore(xz_part, xz_alloca);
    xz_ptr = xz_alloca;
  } else {
    xz_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
  }

  return KeyArgs{
      .key_a = val_alloca,
      .key_b = xz_ptr,
      .key_c = llvm::ConstantInt::get(i32_ty, num_words),
  };
}

// Get the key spec constants for LyraAssocNew
struct KeySpecConstants {
  uint32_t kind;
  uint32_t bit_width;
  uint32_t is_signed;
};

auto GetKeySpecConstants(TypeId key_type, const TypeArena& types)
    -> KeySpecConstants {
  if (!key_type) {
    // Wildcard [*] -> int (32-bit signed)
    return {.kind = 0, .bit_width = 32, .is_signed = 1};
  }
  const auto& type = types[key_type];
  if (type.Kind() == TypeKind::kString) {
    return {.kind = 1, .bit_width = 0, .is_signed = 0};
  }
  if (type.Kind() == TypeKind::kEnum) {
    const auto& info = type.AsEnum();
    const auto& base = types[info.base_type];
    const auto& integral = base.AsIntegral();
    return {
        .kind = 0,
        .bit_width = integral.bit_width,
        .is_signed = integral.is_signed ? 1u : 0u};
  }
  if (type.Kind() == TypeKind::kIntegral) {
    const auto& integral = type.AsIntegral();
    return {
        .kind = 0,
        .bit_width = integral.bit_width,
        .is_signed = integral.is_signed ? 1u : 0u};
  }
  throw common::InternalError(
      "GetKeySpecConstants", "unsupported key type kind");
}

// Load AA handle from receiver, lazy-allocate if null.
auto LoadOrAllocHandle(Context& ctx, mir::PlaceId receiver)
    -> Result<llvm::Value*> {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  auto receiver_ptr = ctx.GetPlacePointer(receiver);
  if (!receiver_ptr) return std::unexpected(receiver_ptr.error());

  auto* handle = builder.CreateLoad(ptr_ty, *receiver_ptr, "aa.handle");

  // Null check: if null, allocate new AssocMap
  auto* is_null = builder.CreateICmpEQ(
      handle,
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty)),
      "aa.isnull");

  auto* orig_bb = builder.GetInsertBlock();
  auto* current_fn = orig_bb->getParent();
  auto* alloc_bb =
      llvm::BasicBlock::Create(ctx.GetLlvmContext(), "aa.alloc", current_fn);
  auto* cont_bb =
      llvm::BasicBlock::Create(ctx.GetLlvmContext(), "aa.cont", current_fn);

  builder.CreateCondBr(is_null, alloc_bb, cont_bb);

  // Alloc block: call LyraAssocNew, store back to receiver
  builder.SetInsertPoint(alloc_bb);
  auto [receiver_type, aa_info] = GetAATypeInfo(ctx, receiver);
  auto key_consts = GetKeySpecConstants(aa_info.key_type, ctx.GetTypeArena());
  uint32_t elem_kind = ClassifyElemKind(
      aa_info.element_type, ctx.GetTypeArena(), ctx.IsForceTwoState());
  uint32_t elem_size = GetElemByteSize(ctx, aa_info.element_type);

  auto* new_handle = builder.CreateCall(
      ctx.GetLyraAssocNew(),
      {llvm::ConstantInt::get(i32_ty, key_consts.kind),
       llvm::ConstantInt::get(i32_ty, key_consts.bit_width),
       llvm::ConstantInt::get(i32_ty, key_consts.is_signed),
       llvm::ConstantInt::get(i32_ty, elem_kind),
       llvm::ConstantInt::get(i32_ty, elem_size)},
      "aa.new");
  builder.CreateStore(new_handle, *receiver_ptr);
  builder.CreateBr(cont_bb);

  // Continue block: PHI to merge handle
  builder.SetInsertPoint(cont_bb);
  auto* phi = builder.CreatePHI(ptr_ty, 2, "aa.h");
  phi->addIncoming(handle, orig_bb);
  phi->addIncoming(new_handle, alloc_bb);

  return phi;
}

// Load AA handle (no allocation - for read-only ops)
auto LoadHandle(Context& ctx, mir::PlaceId receiver) -> Result<llvm::Value*> {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());
  auto receiver_ptr = ctx.GetPlacePointer(receiver);
  if (!receiver_ptr) return std::unexpected(receiver_ptr.error());
  return builder.CreateLoad(ptr_ty, *receiver_ptr, "aa.handle");
}

// Compute key alloca size for iteration output
auto GetKeyAllocaSize(Context& ctx, TypeId key_type) -> uint32_t {
  const auto& types = ctx.GetTypeArena();
  if (!key_type) {
    return 8;  // wildcard = 32-bit int, but we need at least a word
  }
  const auto& type = types[key_type];
  if (type.Kind() == TypeKind::kString) {
    return static_cast<uint32_t>(
        ctx.GetModule().getDataLayout().getPointerSize());
  }
  // Integral: ceil(bw/64) * 8
  uint32_t bw = 32;
  if (type.Kind() == TypeKind::kIntegral) {
    bw = type.AsIntegral().bit_width;
  } else if (type.Kind() == TypeKind::kEnum) {
    const auto& info = type.AsEnum();
    bw = types[info.base_type].AsIntegral().bit_width;
  }
  return ((bw + 63) / 64) * 8;
}

}  // namespace

auto LowerAssocOp(Context& ctx, const mir::AssocOp& op) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(ctx.GetLlvmContext());
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  return std::visit(
      common::Overloaded{
          [&](const mir::AssocGet& get) -> Result<void> {
            auto handle = LoadOrAllocHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            auto key = LowerKey(ctx, get.key, key_type);
            if (!key) return std::unexpected(key.error());

            auto elem_type = GetElemType(ctx, op.receiver);
            uint32_t elem_size = GetElemByteSize(ctx, elem_type);

            // Alloca for output value
            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* out_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, elem_size), nullptr, "aa.get.out");

            builder.CreateCall(
                ctx.GetLyraAssocGet(), {*handle, key->key_a, key->key_b,
                                        key->key_c, out_alloca, null_ptr});

            // Load from alloca and commit to dest
            auto dest_llvm_type = BuildLlvmTypeForTypeId(ctx, elem_type);
            if (!dest_llvm_type) {
              return std::unexpected(dest_llvm_type.error());
            }
            auto* loaded =
                builder.CreateLoad(*dest_llvm_type, out_alloca, "aa.get.val");

            auto result = CommitValue(
                ctx, get.dest, loaded, elem_type, OwnershipPolicy::kMove);
            if (!result) return std::unexpected(result.error());

            // No Destroy here: kMove transfers ownership from the alloca
            // to the destination. The alloca is dead after the load.
            return {};
          },
          [&](const mir::AssocSet& set) -> Result<void> {
            auto handle = LoadOrAllocHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            auto key = LowerKey(ctx, set.key, key_type);
            if (!key) return std::unexpected(key.error());

            // Lower value operand
            auto raw_val = LowerOperandRaw(ctx, set.value);
            if (!raw_val) return std::unexpected(raw_val.error());

            auto elem_type = GetElemType(ctx, op.receiver);
            uint32_t elem_size = GetElemByteSize(ctx, elem_type);

            // Store value to alloca for passing to runtime
            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* val_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, elem_size), nullptr, "aa.set.val");
            builder.CreateStore(*raw_val, val_alloca);

            builder.CreateCall(
                ctx.GetLyraAssocSet(), {*handle, key->key_a, key->key_b,
                                        key->key_c, val_alloca, null_ptr});
            return {};
          },
          [&](const mir::AssocExists& exists) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            auto key = LowerKey(ctx, exists.key, key_type);
            if (!key) return std::unexpected(key.error());

            auto* result = builder.CreateCall(
                ctx.GetLyraAssocExists(),
                {*handle, key->key_a, key->key_b, key->key_c, null_ptr},
                "aa.exists");
            return CommitValue(
                ctx, exists.dest, result, PlaceType(ctx, exists.dest),
                OwnershipPolicy::kMove);
          },
          [&](const mir::AssocDelete& /*del*/) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());
            builder.CreateCall(ctx.GetLyraAssocDeleteAll(), {*handle});
            return {};
          },
          [&](const mir::AssocDeleteKey& del) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            auto key = LowerKey(ctx, del.key, key_type);
            if (!key) return std::unexpected(key.error());

            builder.CreateCall(
                ctx.GetLyraAssocDeleteKey(),
                {*handle, key->key_a, key->key_b, key->key_c, null_ptr});
            return {};
          },
          [&](const mir::AssocNum& num) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto* size = builder.CreateCall(
                ctx.GetLyraAssocSize(), {*handle}, "aa.size");
            auto* size32 = builder.CreateTrunc(size, i32_ty, "aa.size32");
            return CommitValue(
                ctx, num.dest, size32, PlaceType(ctx, num.dest),
                OwnershipPolicy::kMove);
          },
          [&](const mir::AssocIterFirst& iter) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            uint32_t key_size = GetKeyAllocaSize(ctx, key_type);

            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* key_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, key_size), nullptr, "aa.iter.key");
            builder.CreateMemSet(
                key_alloca, llvm::ConstantInt::get(i8_ty, 0), key_size,
                llvm::MaybeAlign(8));

            auto* found = builder.CreateCall(
                ctx.GetLyraAssocFirst(), {*handle, key_alloca}, "aa.first");

            // Only commit key when found (avoid uninitialized/dangling handle)
            auto* is_found = builder.CreateICmpNE(
                found, llvm::ConstantInt::get(found->getType(), 0),
                "aa.first.ok");
            auto* current_fn = builder.GetInsertBlock()->getParent();
            auto* commit_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.first.commit", current_fn);
            auto* done_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.first.done", current_fn);
            builder.CreateCondBr(is_found, commit_bb, done_bb);

            builder.SetInsertPoint(commit_bb);
            auto key_llvm = BuildLlvmTypeForTypeId(ctx, key_type);
            if (!key_llvm) return std::unexpected(key_llvm.error());
            auto* key_val =
                builder.CreateLoad(*key_llvm, key_alloca, "aa.iter.kv");
            auto result = CommitValue(
                ctx, iter.out_key, key_val, key_type, OwnershipPolicy::kMove);
            if (!result) return std::unexpected(result.error());
            builder.CreateBr(done_bb);

            builder.SetInsertPoint(done_bb);
            return CommitValue(
                ctx, iter.dest_found, found, PlaceType(ctx, iter.dest_found),
                OwnershipPolicy::kMove);
          },
          [&](const mir::AssocIterLast& iter) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            uint32_t key_size = GetKeyAllocaSize(ctx, key_type);

            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* key_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, key_size), nullptr, "aa.iter.key");
            builder.CreateMemSet(
                key_alloca, llvm::ConstantInt::get(i8_ty, 0), key_size,
                llvm::MaybeAlign(8));

            auto* found = builder.CreateCall(
                ctx.GetLyraAssocLast(), {*handle, key_alloca}, "aa.last");

            // Only commit key when found (avoid uninitialized/dangling handle)
            auto* is_found = builder.CreateICmpNE(
                found, llvm::ConstantInt::get(found->getType(), 0),
                "aa.last.ok");
            auto* current_fn = builder.GetInsertBlock()->getParent();
            auto* commit_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.last.commit", current_fn);
            auto* done_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.last.done", current_fn);
            builder.CreateCondBr(is_found, commit_bb, done_bb);

            builder.SetInsertPoint(commit_bb);
            auto key_llvm = BuildLlvmTypeForTypeId(ctx, key_type);
            if (!key_llvm) return std::unexpected(key_llvm.error());
            auto* key_val =
                builder.CreateLoad(*key_llvm, key_alloca, "aa.iter.kv");
            auto result = CommitValue(
                ctx, iter.out_key, key_val, key_type, OwnershipPolicy::kMove);
            if (!result) return std::unexpected(result.error());
            builder.CreateBr(done_bb);

            builder.SetInsertPoint(done_bb);
            return CommitValue(
                ctx, iter.dest_found, found, PlaceType(ctx, iter.dest_found),
                OwnershipPolicy::kMove);
          },
          [&](const mir::AssocIterNext& iter) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            uint32_t key_size = GetKeyAllocaSize(ctx, key_type);

            // Load current key into alloca
            auto key_ptr = ctx.GetPlacePointer(iter.key_place);
            if (!key_ptr) return std::unexpected(key_ptr.error());

            auto key_llvm = BuildLlvmTypeForTypeId(ctx, key_type);
            if (!key_llvm) return std::unexpected(key_llvm.error());

            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* key_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, key_size), nullptr, "aa.next.key");
            builder.CreateMemSet(
                key_alloca, llvm::ConstantInt::get(i8_ty, 0), key_size,
                llvm::MaybeAlign(8));
            auto* current_key =
                builder.CreateLoad(*key_llvm, *key_ptr, "aa.next.cur");
            builder.CreateStore(current_key, key_alloca);

            auto* found = builder.CreateCall(
                ctx.GetLyraAssocNext(), {*handle, key_alloca}, "aa.next");

            // Only update key_place when found (avoid dangling handle on miss)
            auto* is_found = builder.CreateICmpNE(
                found, llvm::ConstantInt::get(found->getType(), 0),
                "aa.next.ok");
            auto* current_fn = builder.GetInsertBlock()->getParent();
            auto* commit_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.next.commit", current_fn);
            auto* done_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.next.done", current_fn);
            builder.CreateCondBr(is_found, commit_bb, done_bb);

            builder.SetInsertPoint(commit_bb);
            auto* new_key =
                builder.CreateLoad(*key_llvm, key_alloca, "aa.next.kv");
            auto result = CommitValue(
                ctx, iter.key_place, new_key, key_type, OwnershipPolicy::kMove);
            if (!result) return std::unexpected(result.error());
            builder.CreateBr(done_bb);

            builder.SetInsertPoint(done_bb);
            return CommitValue(
                ctx, iter.dest_found, found, PlaceType(ctx, iter.dest_found),
                OwnershipPolicy::kMove);
          },
          [&](const mir::AssocIterPrev& iter) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            uint32_t key_size = GetKeyAllocaSize(ctx, key_type);

            auto key_ptr = ctx.GetPlacePointer(iter.key_place);
            if (!key_ptr) return std::unexpected(key_ptr.error());

            auto key_llvm = BuildLlvmTypeForTypeId(ctx, key_type);
            if (!key_llvm) return std::unexpected(key_llvm.error());

            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* key_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, key_size), nullptr, "aa.prev.key");
            builder.CreateMemSet(
                key_alloca, llvm::ConstantInt::get(i8_ty, 0), key_size,
                llvm::MaybeAlign(8));
            auto* current_key =
                builder.CreateLoad(*key_llvm, *key_ptr, "aa.prev.cur");
            builder.CreateStore(current_key, key_alloca);

            auto* found = builder.CreateCall(
                ctx.GetLyraAssocPrev(), {*handle, key_alloca}, "aa.prev");

            // Only update key_place when found (avoid dangling handle on miss)
            auto* is_found = builder.CreateICmpNE(
                found, llvm::ConstantInt::get(found->getType(), 0),
                "aa.prev.ok");
            auto* current_fn = builder.GetInsertBlock()->getParent();
            auto* commit_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.prev.commit", current_fn);
            auto* done_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.prev.done", current_fn);
            builder.CreateCondBr(is_found, commit_bb, done_bb);

            builder.SetInsertPoint(commit_bb);
            auto* new_key =
                builder.CreateLoad(*key_llvm, key_alloca, "aa.prev.kv");
            auto result = CommitValue(
                ctx, iter.key_place, new_key, key_type, OwnershipPolicy::kMove);
            if (!result) return std::unexpected(result.error());
            builder.CreateBr(done_bb);

            builder.SetInsertPoint(done_bb);
            return CommitValue(
                ctx, iter.dest_found, found, PlaceType(ctx, iter.dest_found),
                OwnershipPolicy::kMove);
          },
          [&](const mir::AssocSnapshot& snap) -> Result<void> {
            auto handle = LoadHandle(ctx, op.receiver);
            if (!handle) return std::unexpected(handle.error());

            auto key_type = GetKeyType(ctx, op.receiver);
            uint32_t key_size = GetKeyAllocaSize(ctx, key_type);

            // Create snapshot
            auto* snapshot = builder.CreateCall(
                ctx.GetLyraAssocSnapshotCreate(), {*handle}, "aa.snap");
            auto* snap_size = builder.CreateCall(
                ctx.GetLyraAssocSnapshotSize(), {snapshot}, "aa.snap.sz");

            // Allocate dynamic array for keys
            auto dest_type = PlaceType(ctx, snap.dest_keys);
            auto elem_ops = ctx.GetElemOpsForType(key_type);
            if (!elem_ops) return std::unexpected(elem_ops.error());

            auto* new_arr = builder.CreateCall(
                ctx.GetLyraDynArrayNew(),
                {snap_size, llvm::ConstantInt::get(i32_ty, elem_ops->elem_size),
                 elem_ops->clone_fn, elem_ops->destroy_fn},
                "aa.snap.arr");

            // Loop: copy keys from snapshot to array
            auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
            auto* key_alloca = builder.CreateAlloca(
                llvm::ArrayType::get(i8_ty, key_size), nullptr, "aa.snap.key");

            auto* pre_loop_bb = builder.GetInsertBlock();
            auto* current_fn = pre_loop_bb->getParent();
            auto* loop_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.snap.loop", current_fn);
            auto* body_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.snap.body", current_fn);
            auto* done_bb = llvm::BasicBlock::Create(
                ctx.GetLlvmContext(), "aa.snap.done", current_fn);

            builder.CreateBr(loop_bb);

            builder.SetInsertPoint(loop_bb);
            auto* idx = builder.CreatePHI(i64_ty, 2, "aa.snap.idx");
            idx->addIncoming(llvm::ConstantInt::get(i64_ty, 0), pre_loop_bb);
            auto* cmp = builder.CreateICmpSLT(idx, snap_size, "aa.snap.cmp");
            builder.CreateCondBr(cmp, body_bb, done_bb);

            builder.SetInsertPoint(body_bb);
            builder.CreateCall(
                ctx.GetLyraAssocSnapshotKeyAt(), {snapshot, idx, key_alloca});
            auto* elem_ptr = builder.CreateCall(
                ctx.GetLyraDynArrayElementPtr(), {new_arr, idx}, "aa.snap.ep");

            auto key_llvm = BuildLlvmTypeForTypeId(ctx, key_type);
            if (!key_llvm) return std::unexpected(key_llvm.error());
            auto* kv = builder.CreateLoad(*key_llvm, key_alloca, "aa.snap.kv");
            builder.CreateStore(kv, elem_ptr);

            auto* next_idx = builder.CreateAdd(
                idx, llvm::ConstantInt::get(i64_ty, 1), "aa.snap.next");
            idx->addIncoming(next_idx, body_bb);
            builder.CreateBr(loop_bb);

            builder.SetInsertPoint(done_bb);
            builder.CreateCall(ctx.GetLyraAssocSnapshotRelease(), {snapshot});

            return CommitValue(
                ctx, snap.dest_keys, new_arr, dest_type,
                OwnershipPolicy::kMove);
          },
      },
      op.data);
}

}  // namespace lyra::lowering::mir_to_llvm
