#include "lyra/llvm_backend/emit_descriptor_utils.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Alignment.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

void ValidateOwnedMetaTemplate(
    const OwnedProcessMetaTemplate& tmpl, const char* caller) {
  // Pool sentinel must hold regardless of entry count.
  if (!tmpl.pool.empty() && tmpl.pool[0] != '\0') {
    throw common::InternalError(
        caller, "meta pool missing '\\0' sentinel at offset 0");
  }
  if (tmpl.entries.empty()) return;
  if (tmpl.pool.empty()) {
    throw common::InternalError(
        caller, "non-empty meta template has empty pool");
  }
  auto pool_size = static_cast<uint32_t>(tmpl.pool.size());
  for (uint32_t i = 0; i < tmpl.entries.size(); ++i) {
    uint32_t off = tmpl.entries[i].file_pool_off;
    if (off == 0) continue;
    if (off >= pool_size) {
      throw common::InternalError(
          caller,
          std::format(
              "entry {} file_pool_off {} >= pool_size {}", i, off, pool_size));
    }
    bool found_nul = false;
    for (uint32_t j = off; j < pool_size; ++j) {
      if (tmpl.pool[j] == '\0') {
        found_nul = true;
        break;
      }
    }
    if (!found_nul) {
      throw common::InternalError(
          caller,
          std::format(
              "entry {} file string at offset {} not NUL-terminated", i, off));
    }
  }
}

// Validate an owned trigger template before emission.
// Either all containers are empty, or they satisfy domain shape invariants.
void ValidateOwnedTriggerTemplate(
    const OwnedTriggerTemplate& tmpl, const char* caller) {
  bool has_entries = !tmpl.entries.empty();
  bool has_ranges = !tmpl.proc_ranges.empty();
  bool has_shapes = !tmpl.proc_shapes.empty();
  bool has_groupable = !tmpl.proc_groupable.empty();
  if (!has_ranges && (has_entries || has_shapes || has_groupable)) {
    throw common::InternalError(
        caller, "trigger template partially populated: ranges empty");
  }
  if (has_ranges && (!has_shapes || !has_groupable)) {
    throw common::InternalError(
        caller,
        "trigger template partially populated: ranges non-empty "
        "but shapes or groupable empty");
  }
  if (!has_ranges) return;
  if (tmpl.proc_shapes.size() != tmpl.proc_ranges.size()) {
    throw common::InternalError(
        caller, std::format(
                    "trigger shapes size {} != ranges size {}",
                    tmpl.proc_shapes.size(), tmpl.proc_ranges.size()));
  }
  if (tmpl.proc_groupable.size() != tmpl.proc_ranges.size()) {
    throw common::InternalError(
        caller, std::format(
                    "trigger groupable size {} != ranges size {}",
                    tmpl.proc_groupable.size(), tmpl.proc_ranges.size()));
  }
  for (uint32_t i = 0; i < tmpl.proc_ranges.size(); ++i) {
    const auto& r = tmpl.proc_ranges[i];
    if (r.start > tmpl.entries.size() ||
        r.count > tmpl.entries.size() - r.start) {
      throw common::InternalError(
          caller, std::format(
                      "trigger range[{}] ({},{}) exceeds entries size {}", i,
                      r.start, r.count, tmpl.entries.size()));
    }
  }
}

// Validate an owned comb template before emission.
void ValidateOwnedCombTemplate(
    const OwnedCombTemplate& tmpl, const char* caller) {
  bool has_entries = !tmpl.entries.empty();
  bool has_kernels = !tmpl.kernels.empty();
  if (has_entries && !has_kernels) {
    throw common::InternalError(
        caller, "comb template has entries without kernels");
  }
  if (has_kernels && !has_entries) {
    throw common::InternalError(
        caller, "comb template has kernels without entries");
  }
  for (uint32_t i = 0; i < tmpl.kernels.size(); ++i) {
    const auto& k = tmpl.kernels[i];
    if (k.trigger_start > tmpl.entries.size() ||
        k.trigger_count > tmpl.entries.size() - k.trigger_start) {
      throw common::InternalError(
          caller, std::format(
                      "comb kernel[{}] ({},{}) exceeds entries size {}", i,
                      k.trigger_start, k.trigger_count, tmpl.entries.size()));
    }
  }
}

auto EmitObservableDescriptorTemplate(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    const OwnedObservableDescriptorTemplate& tmpl,
    const std::string& name_prefix) -> ObservableDescriptorEmission {
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  ObservableDescriptorEmission result{};
  result.entries_ptr = null_ptr;
  result.num_entries = 0;
  result.pool_ptr = null_ptr;
  result.pool_size = 0;

  auto num_entries = static_cast<uint32_t>(tmpl.entries.size());
  if (num_entries > 0) {
    auto* entry_type = llvm::ArrayType::get(i32_ty, 15);
    std::vector<llvm::Constant*> entry_constants;
    entry_constants.reserve(num_entries);
    for (const auto& e : tmpl.entries) {
      std::array<llvm::Constant*, 15> fields = {
          llvm::ConstantInt::get(i32_ty, e.storage_byte_offset),
          llvm::ConstantInt::get(i32_ty, e.total_bytes),
          llvm::ConstantInt::get(i32_ty, e.storage_kind),
          llvm::ConstantInt::get(i32_ty, e.value_lane_offset),
          llvm::ConstantInt::get(i32_ty, e.value_lane_bytes),
          llvm::ConstantInt::get(i32_ty, e.unk_lane_offset),
          llvm::ConstantInt::get(i32_ty, e.unk_lane_bytes),
          llvm::ConstantInt::get(i32_ty, e.bit_width),
          llvm::ConstantInt::get(i32_ty, e.local_name_pool_off),
          llvm::ConstantInt::get(i32_ty, e.trace_kind),
          llvm::ConstantInt::get(i32_ty, e.storage_owner_ref),
          llvm::ConstantInt::get(i32_ty, e.flags),
          llvm::ConstantInt::get(i32_ty, e.storage_domain),
          llvm::ConstantInt::get(i32_ty, e.local_signal_id),
          llvm::ConstantInt::get(i32_ty, e.backing_rel_off),
      };
      entry_constants.push_back(llvm::ConstantArray::get(entry_type, fields));
    }
    auto entries_name = name_prefix + "_obs_entries";
    auto* entries_array_type = llvm::ArrayType::get(entry_type, num_entries);
    auto* entries_global = new llvm::GlobalVariable(
        mod, entries_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(entries_array_type, entry_constants),
        entries_name);
    entries_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    result.entries_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        entries_array_type, entries_global,
        llvm::ArrayRef<llvm::Constant*>{zero, zero});
    result.num_entries = num_entries;
  }

  auto pool_size = static_cast<uint32_t>(tmpl.pool.size());
  if (pool_size > 0) {
    auto pool_name = name_prefix + "_obs_pool";
    std::vector<llvm::Constant*> pool_bytes;
    pool_bytes.reserve(pool_size);
    for (char c : tmpl.pool) {
      pool_bytes.push_back(
          llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(c)));
    }
    auto* pool_array_type = llvm::ArrayType::get(i8_ty, pool_size);
    auto* pool_global = new llvm::GlobalVariable(
        mod, pool_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(pool_array_type, pool_bytes), pool_name);
    pool_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    result.pool_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        pool_array_type, pool_global,
        llvm::ArrayRef<llvm::Constant*>{zero, zero});
    result.pool_size = pool_size;
  }

  return result;
}

auto EmitInitDescriptor(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    std::span<const runtime::StorageConstructionOp> recipe,
    std::span<const uint32_t> recipe_roots,
    std::span<const uint32_t> recipe_child_indices,
    std::span<const runtime::ParamInitSlotEntry> param_slots,
    const std::string& name_prefix) -> InitDescriptorEmission {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  InitDescriptorEmission result{};
  result.recipe_ptr = null_ptr;
  result.num_recipe_ops = 0;
  result.recipe_roots_ptr = null_ptr;
  result.num_recipe_roots = 0;
  result.recipe_child_indices_ptr = null_ptr;
  result.num_recipe_child_indices = 0;
  result.param_slots_ptr = null_ptr;
  result.num_param_slots = 0;

  // Emit storage construction recipe ops as a raw byte blob.
  //
  // StorageConstructionOp is a POD tagged union (trivially copyable,
  // standard layout). We emit the entire op array as a single
  // [N * sizeof(Op) x i8] constant with the correct alignment.
  // The runtime reads via const StorageConstructionOp* pointer,
  // which is valid because the alignment and size contract is
  // enforced by static_assert in the recipe header.
  //
  // This approach is preferred over per-field LLVM struct emission
  // because the union variant makes typed emission complex, and the
  // POD contract guarantees identical binary representation.
  if (!recipe.empty()) {
    constexpr auto kOpSize = sizeof(runtime::StorageConstructionOp);
    constexpr auto kOpAlign = alignof(runtime::StorageConstructionOp);
    auto num = static_cast<uint32_t>(recipe.size());
    auto total_bytes = num * kOpSize;

    // Build raw byte data from the POD op array. reinterpret_cast is
    // required: trivially-copyable POD array to LLVM byte blob has no
    // type-safe alternative.
    // NOLINTBEGIN(cppcoreguidelines-pro-type-reinterpret-cast)
    auto blob = llvm::StringRef(
        reinterpret_cast<const char*>(recipe.data()), total_bytes);
    // NOLINTEND(cppcoreguidelines-pro-type-reinterpret-cast)
    auto* data = llvm::ConstantDataArray::getRaw(blob, total_bytes, i8_ty);

    auto* global = new llvm::GlobalVariable(
        mod, data->getType(), true, llvm::GlobalValue::InternalLinkage, data,
        name_prefix + "_init_recipe");
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    global->setAlignment(llvm::Align(kOpAlign));
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    result.recipe_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        data->getType(), global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
    result.num_recipe_ops = num;
  }

  // Emit recipe root indices: [N x i32]
  if (!recipe_roots.empty()) {
    std::vector<llvm::Constant*> constants;
    constants.reserve(recipe_roots.size());
    for (uint32_t root : recipe_roots) {
      constants.push_back(llvm::ConstantInt::get(i32_ty, root));
    }
    auto num = static_cast<uint32_t>(recipe_roots.size());
    auto* arr_ty = llvm::ArrayType::get(i32_ty, num);
    auto* global = new llvm::GlobalVariable(
        mod, arr_ty, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(arr_ty, constants),
        name_prefix + "_init_recipe_roots");
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    result.recipe_roots_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        arr_ty, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
    result.num_recipe_roots = num;
  }

  // Emit recipe child indices: [N x i32]
  if (!recipe_child_indices.empty()) {
    std::vector<llvm::Constant*> constants;
    constants.reserve(recipe_child_indices.size());
    for (uint32_t idx : recipe_child_indices) {
      constants.push_back(llvm::ConstantInt::get(i32_ty, idx));
    }
    auto num = static_cast<uint32_t>(recipe_child_indices.size());
    auto* arr_ty = llvm::ArrayType::get(i32_ty, num);
    auto* global = new llvm::GlobalVariable(
        mod, arr_ty, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(arr_ty, constants),
        name_prefix + "_init_recipe_child_indices");
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    result.recipe_child_indices_ptr =
        llvm::ConstantExpr::getInBoundsGetElementPtr(
            arr_ty, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
    result.num_recipe_child_indices = num;
  }

  // Emit param slots: {i32 rel_byte_offset, i32 byte_size}
  if (!param_slots.empty()) {
    auto* entry_ty = llvm::StructType::get(ctx, {i32_ty, i32_ty});
    std::vector<llvm::Constant*> constants;
    constants.reserve(param_slots.size());
    for (const auto& s : param_slots) {
      constants.push_back(
          llvm::ConstantStruct::get(
              entry_ty, {llvm::ConstantInt::get(i32_ty, s.rel_byte_offset),
                         llvm::ConstantInt::get(i32_ty, s.byte_size)}));
    }
    auto num = static_cast<uint32_t>(param_slots.size());
    auto* arr_ty = llvm::ArrayType::get(entry_ty, num);
    auto* global = new llvm::GlobalVariable(
        mod, arr_ty, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(arr_ty, constants),
        name_prefix + "_init_param_slots");
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    auto* zero = llvm::ConstantInt::get(i32_ty, 0);
    result.param_slots_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        arr_ty, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
    result.num_param_slots = num;
  }

  return result;
}

// Emit a byte pool as a single [N x i8] constant global.
// Returns pointer to first element, or null if empty.
auto EmitBytePoolGlobal(
    llvm::LLVMContext& ctx, llvm::Module& mod, llvm::ArrayRef<uint8_t> data,
    const std::string& name) -> llvm::Constant* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  if (data.empty()) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }
  auto* init = llvm::ConstantDataArray::get(ctx, data);
  auto* global = new llvm::GlobalVariable(
      mod, init->getType(), true, llvm::GlobalValue::InternalLinkage, init,
      name);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      init->getType(), global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

}  // namespace lyra::lowering::mir_to_llvm
