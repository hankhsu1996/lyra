#include "lyra/llvm_backend/type_ops/four_state_init.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <utility>
#include <vector>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// FNV-1a hash for patch content (deterministic, stable across runs).
// NOTE: absl::Hash is NOT deterministic across process invocations (random
// seed), so we use FNV-1a which produces reproducible hashes for identical
// content.
auto HashPatchContent(
    const uint64_t* offsets, const void* masks, size_t count, size_t mask_size)
    -> uint64_t {
  constexpr uint64_t kFnvPrime = 0x00000100000001B3ULL;
  constexpr uint64_t kFnvBasis = 0xcbf29ce484222325ULL;
  uint64_t hash = kFnvBasis;

  // Hash offsets
  auto* offset_bytes = reinterpret_cast<const uint8_t*>(offsets);
  for (size_t i = 0; i < count * sizeof(uint64_t); ++i) {
    hash ^= offset_bytes[i];
    hash *= kFnvPrime;
  }

  // Hash masks
  auto* mask_bytes = static_cast<const uint8_t*>(masks);
  for (size_t i = 0; i < count * mask_size; ++i) {
    hash ^= mask_bytes[i];
    hash *= kFnvPrime;
  }

  return hash;
}

// Get or create the runtime helper function with proper attributes
auto GetOrCreatePatchHelper(
    llvm::Module& module, llvm::LLVMContext& llvm_ctx, const char* name)
    -> llvm::FunctionCallee {
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* void_ty = llvm::Type::getVoidTy(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* func_ty =
      llvm::FunctionType::get(void_ty, {ptr_ty, ptr_ty, ptr_ty, i64_ty}, false);

  auto callee = module.getOrInsertFunction(name, func_ty);

  // Set attributes if this is a new function declaration
  if (auto* func = llvm::dyn_cast<llvm::Function>(callee.getCallee())) {
    if (func->empty()) {
      // Set function attributes
      func->setDoesNotThrow();
      func->setWillReturn();
      // Arg 0 (base): nocapture
      func->addParamAttr(0, llvm::Attribute::NoCapture);
      // Arg 1 (offsets): nocapture, readonly
      func->addParamAttr(1, llvm::Attribute::NoCapture);
      func->addParamAttr(1, llvm::Attribute::ReadOnly);
      // Arg 2 (masks): nocapture, readonly
      func->addParamAttr(2, llvm::Attribute::NoCapture);
      func->addParamAttr(2, llvm::Attribute::ReadOnly);
    }
  }

  return callee;
}

// Get or create a global constant array, reusing existing if content matches.
// Returns the global variable (caller should GEP to get element pointer).
auto GetOrCreatePatchGlobal(
    llvm::Module& module, const std::string& name, llvm::Constant* init)
    -> llvm::GlobalVariable* {
  // Check if global already exists
  if (auto* existing = module.getNamedGlobal(name)) {
    return existing;
  }

  // Create new global
  auto* global = new llvm::GlobalVariable(
      module, init->getType(), true, llvm::GlobalValue::InternalLinkage, init,
      name);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return global;
}

// Create global constant arrays for offsets and masks, then emit a call to
// the runtime helper. Returns without emitting anything if patches is empty.
// Uses content-based hashing for deterministic naming and deduplication.
template <typename MaskT>
void EmitPatchCall(
    Context& ctx, llvm::Value* base_ptr,
    const std::vector<std::pair<uint64_t, MaskT>>& patches,
    const std::string& name_prefix, const char* helper_name) {
  if (patches.empty()) return;

  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto& module = ctx.GetModule();

  auto count = patches.size();
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  // Build offset and mask data arrays
  std::vector<uint64_t> offset_data;
  offset_data.reserve(count);
  std::vector<MaskT> mask_data;
  mask_data.reserve(count);
  for (const auto& [offset, mask] : patches) {
    offset_data.push_back(offset);
    mask_data.push_back(mask);
  }

  // Compute content hash for deterministic naming and deduplication.
  // NOTE: We don't include name_prefix in the global name - this allows
  // identical patch tables across different frames to share the same globals.
  uint64_t content_hash = HashPatchContent(
      offset_data.data(), mask_data.data(), count, sizeof(MaskT));

  // Generate deterministic global names from content hash only.
  // Width is encoded in the name to avoid type collisions.
  std::string offset_name =
      std::format("lyra.4s.off{}.{:016x}", sizeof(MaskT) * 8, content_hash);
  std::string mask_name =
      std::format("lyra.4s.mask{}.{:016x}", sizeof(MaskT) * 8, content_hash);

  // Build offset array using ConstantDataArray
  auto* offset_init =
      llvm::ConstantDataArray::get(llvm_ctx, llvm::ArrayRef(offset_data));
  auto* offset_global =
      GetOrCreatePatchGlobal(module, offset_name, offset_init);
  auto* offset_arr_ty = offset_global->getValueType();

  // Build mask array using ConstantDataArray
  llvm::Constant* mask_init = nullptr;
  if constexpr (sizeof(MaskT) == 1) {
    mask_init =
        llvm::ConstantDataArray::get(llvm_ctx, llvm::ArrayRef(mask_data));
  } else if constexpr (sizeof(MaskT) == 2) {
    mask_init =
        llvm::ConstantDataArray::get(llvm_ctx, llvm::ArrayRef(mask_data));
  } else if constexpr (sizeof(MaskT) == 4) {
    mask_init =
        llvm::ConstantDataArray::get(llvm_ctx, llvm::ArrayRef(mask_data));
  } else {
    mask_init =
        llvm::ConstantDataArray::get(llvm_ctx, llvm::ArrayRef(mask_data));
  }
  auto* mask_global = GetOrCreatePatchGlobal(module, mask_name, mask_init);
  auto* mask_arr_ty = mask_global->getValueType();

  // Get or create the runtime helper with proper attributes
  auto callee = GetOrCreatePatchHelper(module, llvm_ctx, helper_name);

  // GEP (0,0) to get pointer to first element, not pointer to array
  auto* zero = builder.getInt32(0);
  auto* offset_ptr =
      builder.CreateGEP(offset_arr_ty, offset_global, {zero, zero});
  auto* mask_ptr = builder.CreateGEP(mask_arr_ty, mask_global, {zero, zero});

  // Emit call
  builder.CreateCall(
      callee,
      {base_ptr, offset_ptr, mask_ptr, llvm::ConstantInt::get(i64_ty, count)});
}

}  // namespace

void EmitApply4StatePatches(
    Context& ctx, llvm::Value* base_ptr, const FourStatePatchTable& patches,
    const std::string& name_prefix) {
  EmitPatchCall(
      ctx, base_ptr, patches.patches_8, name_prefix + ".8",
      "LyraApply4StatePatches8");
  EmitPatchCall(
      ctx, base_ptr, patches.patches_16, name_prefix + ".16",
      "LyraApply4StatePatches16");
  EmitPatchCall(
      ctx, base_ptr, patches.patches_32, name_prefix + ".32",
      "LyraApply4StatePatches32");
  EmitPatchCall(
      ctx, base_ptr, patches.patches_64, name_prefix + ".64",
      "LyraApply4StatePatches64");
}

}  // namespace lyra::lowering::mir_to_llvm
