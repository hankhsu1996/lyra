#include "lyra/llvm_backend/design_metadata_lowering.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <string>
#include <unordered_map>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/type_query.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/instance.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/runtime/loop_site_meta.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto ClassifySlotStorageKind(
    TypeId type_id, const TypeArena& types, bool force_two_state)
    -> runtime::SlotStorageKind {
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      return runtime::SlotStorageKind::kString;
    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
      return runtime::SlotStorageKind::kHandle;
    case TypeKind::kUnpackedArray:
    case TypeKind::kUnpackedStruct:
    case TypeKind::kUnpackedUnion:
      return runtime::SlotStorageKind::kAggregate;
    case TypeKind::kAssociativeArray:
      return runtime::SlotStorageKind::kHandle;
    default:
      break;
  }
  if (IsPacked(type) && IsFourState(type_id, types, force_two_state)) {
    return runtime::SlotStorageKind::kPacked4;
  }
  return runtime::SlotStorageKind::kPacked2;
}

auto MapProcessKind(mir::ProcessKind kind) -> runtime::ProcessKind {
  switch (kind) {
    case mir::ProcessKind::kOnce:
      return runtime::ProcessKind::kInitial;
    case mir::ProcessKind::kFinal:
      return runtime::ProcessKind::kFinal;
    case mir::ProcessKind::kLooping:
      return runtime::ProcessKind::kAlways;
  }
  return runtime::ProcessKind::kAlways;
}

struct ResolvedLoc {
  std::string file;
  uint32_t line = 0;
  uint32_t col = 0;
};

auto ResolveProcessOrigin(
    common::OriginId origin, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager) -> ResolvedLoc {
  if (diag_ctx == nullptr || source_manager == nullptr || !origin.IsValid()) {
    return {};
  }

  // TODO(hankhsu): Thread ResolveToSpan access for full source location.
  return {};
}

auto EmitWordArrayGlobal(
    llvm::Module& mod, llvm::LLVMContext& ctx,
    const std::vector<uint32_t>& words, const char* name) -> llvm::Constant* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  if (words.empty()) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  std::vector<llvm::Constant*> word_constants;
  word_constants.reserve(words.size());
  for (uint32_t w : words) {
    word_constants.push_back(llvm::ConstantInt::get(i32_ty, w));
  }

  auto* array_type = llvm::ArrayType::get(i32_ty, words.size());
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, word_constants), name);
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

auto EmitPoolGlobal(
    llvm::Module& mod, llvm::LLVMContext& ctx, const std::vector<char>& pool,
    const char* name) -> llvm::Constant* {
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  if (pool.empty()) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  std::vector<llvm::Constant*> pool_bytes;
  pool_bytes.reserve(pool.size());
  for (char c : pool) {
    pool_bytes.push_back(
        llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(c)));
  }

  auto* pool_array_type = llvm::ArrayType::get(i8_ty, pool.size());
  auto* pool_init = llvm::ConstantArray::get(pool_array_type, pool_bytes);
  auto* pool_global = new llvm::GlobalVariable(
      mod, pool_array_type, true, llvm::GlobalValue::InternalLinkage, pool_init,
      name);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      pool_array_type, pool_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

auto GetConnectionDescriptorLlvmType(llvm::LLVMContext& ctx)
    -> llvm::StructType* {
  auto* i8 = llvm::Type::getInt8Ty(ctx);
  auto* i16 = llvm::Type::getInt16Ty(ctx);
  auto* i32 = llvm::Type::getInt32Ty(ctx);
  return llvm::StructType::create(
      ctx, {i32, i32, i32, i32, i32, i8, i8, i16, i32, i32},
      "ConnectionDescriptor");
}

}  // namespace

auto ExtractSlotMetaInputs(
    Context& context, const std::vector<SlotInfo>& slots,
    const DesignLayout& design_layout, const llvm::DataLayout& dl,
    const TypeArena& types) -> std::vector<realization::SlotMetaInput> {
  std::vector<realization::SlotMetaInput> entries;
  entries.reserve(slots.size());

  auto* design_struct = design_layout.llvm_type;
  const llvm::StructLayout* sl = dl.getStructLayout(design_struct);

  for (const auto& slot : slots) {
    auto it = design_layout.slot_to_field.find(slot.slot_id);
    if (it == design_layout.slot_to_field.end()) {
      throw common::InternalError(
          "ExtractSlotMetaInputs",
          std::format("slot_id {} not in design layout", slot.slot_id.value));
    }
    uint32_t field_idx = it->second;
    auto* field_type = design_struct->getElementType(field_idx);

    uint64_t raw_base_off = sl->getElementOffset(field_idx);
    uint64_t raw_total_bytes = dl.getTypeAllocSize(field_type);
    if (raw_base_off > UINT32_MAX || raw_total_bytes > UINT32_MAX) {
      throw common::InternalError(
          "ExtractSlotMetaInputs", "DesignState exceeds 4GB (not supported)");
    }

    auto kind =
        ClassifySlotStorageKind(slot.type_id, types, context.IsForceTwoState());

    realization::SlotMetaInput entry{
        .byte_offset = static_cast<uint32_t>(raw_base_off),
        .total_bytes = static_cast<uint32_t>(raw_total_bytes),
        .storage_kind = static_cast<uint32_t>(kind),
    };

    if (kind == runtime::SlotStorageKind::kPacked4) {
      auto* four_state = llvm::dyn_cast<llvm::StructType>(field_type);
      if (four_state == nullptr) {
        throw common::InternalError(
            "ExtractSlotMetaInputs",
            std::format(
                "expected StructType for kPacked4 slot {}",
                slot.slot_id.value));
      }
      const llvm::StructLayout* fs_layout = dl.getStructLayout(four_state);
      uint64_t raw_value_off = fs_layout->getElementOffset(0);
      uint64_t raw_value_bytes =
          dl.getTypeAllocSize(four_state->getElementType(0));
      uint64_t raw_unk_off = fs_layout->getElementOffset(1);
      uint64_t raw_unk_bytes =
          dl.getTypeAllocSize(four_state->getElementType(1));

      if (raw_value_off > UINT32_MAX || raw_value_bytes > UINT32_MAX ||
          raw_unk_off > UINT32_MAX || raw_unk_bytes > UINT32_MAX) {
        throw common::InternalError(
            "ExtractSlotMetaInputs",
            "4-state plane layout exceeds 4GB (not supported)");
      }
      entry.value_offset = static_cast<uint32_t>(raw_value_off);
      entry.value_bytes = static_cast<uint32_t>(raw_value_bytes);
      entry.unk_offset = static_cast<uint32_t>(raw_unk_off);
      entry.unk_bytes = static_cast<uint32_t>(raw_unk_bytes);
    }

    entries.push_back(entry);
  }

  return entries;
}

auto PrepareScheduledProcessInputs(
    const std::vector<std::string>& instance_paths, const mir::Arena& mir_arena,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager,
    const std::vector<ScheduledProcess>& scheduled_processes, size_t num_init)
    -> std::vector<realization::ScheduledProcessInput> {
  if (scheduled_processes.size() < num_init) {
    throw common::InternalError(
        "PrepareScheduledProcessInputs",
        "scheduled_processes.size() < num_init");
  }

  std::vector<realization::ScheduledProcessInput> entries;
  auto num_module = scheduled_processes.size() - num_init;
  entries.reserve(num_module);

  for (size_t i = num_init; i < scheduled_processes.size(); ++i) {
    const auto& bp = scheduled_processes[i];
    const auto& proc = mir_arena[bp.process_id];

    std::string inst_path;
    if (bp.module_index) {
      if (bp.module_index.value >= instance_paths.size()) {
        throw common::InternalError(
            "PrepareScheduledProcessInputs",
            std::format(
                "module_index {} out of range (instance_paths size {})",
                bp.module_index.value, instance_paths.size()));
      }
      inst_path = instance_paths[bp.module_index.value];
    }

    auto kind = MapProcessKind(proc.kind);
    auto loc = ResolveProcessOrigin(proc.origin, diag_ctx, source_manager);

    auto scheduled_index = static_cast<uint32_t>(i - num_init);

    entries.push_back({
        .scheduled_process_index = scheduled_index,
        .module_index = bp.module_index
                            ? bp.module_index.value
                            : static_cast<uint32_t>(ModuleIndex::kNone),
        .kind_packed = static_cast<uint32_t>(kind),
        .instance_path = std::move(inst_path),
        .file = std::move(loc.file),
        .line = loc.line,
        .col = loc.col,
    });
  }

  return entries;
}

auto PrepareLoopSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<realization::LoopSiteInput> {
  const auto& origins = context.GetLoopSiteOrigins();
  std::vector<realization::LoopSiteInput> entries;
  entries.reserve(origins.size());

  for (size_t i = 0; i < origins.size(); ++i) {
    auto loc = ResolveProcessOrigin(origins[i], diag_ctx, source_manager);
    entries.push_back({
        .loop_site_index = static_cast<uint32_t>(i),
        .file = std::move(loc.file),
        .line = loc.line,
        .col = loc.col,
    });
  }

  return entries;
}

auto ExtractConnectionDescriptorEntries(
    const std::vector<TypeId>& slot_types, const mir::Arena& mir_arena,
    const TypeArena& type_arena, const Layout& layout,
    const llvm::DataLayout& dl, llvm::LLVMContext& ctx, bool force_two_state)
    -> std::vector<realization::ConnectionDescriptorEntry> {
  const auto& kernel_entries = layout.connection_kernel_entries;
  std::vector<realization::ConnectionDescriptorEntry> entries;
  entries.reserve(kernel_entries.size());

  auto* design_struct = layout.design.llvm_type;
  const llvm::StructLayout* design_sl = dl.getStructLayout(design_struct);

  for (const auto& entry : kernel_entries) {
    auto src_it = layout.design.slot_to_field.find(entry.src_slot);
    auto dst_it = layout.design.slot_to_field.find(entry.dst_slot);
    if (src_it == layout.design.slot_to_field.end() ||
        dst_it == layout.design.slot_to_field.end()) {
      throw common::InternalError(
          "ExtractConnectionDescriptorEntries",
          "kernelized connection slot not in design layout");
    }

    uint32_t src_field = src_it->second;
    uint32_t dst_field = dst_it->second;

    uint64_t raw_src_offset = design_sl->getElementOffset(src_field);
    uint64_t raw_dst_offset = design_sl->getElementOffset(dst_field);
    auto* dst_field_type = design_struct->getElementType(dst_field);
    uint64_t raw_byte_size = dl.getTypeAllocSize(dst_field_type);

    if (raw_src_offset > UINT32_MAX || raw_dst_offset > UINT32_MAX ||
        raw_byte_size > UINT32_MAX) {
      throw common::InternalError(
          "ExtractConnectionDescriptorEntries",
          "connection descriptor offset/size exceeds 4GB (not supported)");
    }

    auto src_offset = static_cast<uint32_t>(raw_src_offset);
    auto dst_offset = static_cast<uint32_t>(raw_dst_offset);
    auto byte_size = static_cast<uint32_t>(raw_byte_size);

    uint32_t trigger_byte_offset = 0;
    uint32_t trigger_byte_size = 0;
    uint8_t trigger_bit_index = 0;
    if (entry.trigger_observed_place) {
      auto trigger_slot_it =
          layout.design.slot_to_field.find(entry.trigger_slot);
      if (trigger_slot_it != layout.design.slot_to_field.end()) {
        if (trigger_slot_it->first.value >= slot_types.size()) {
          throw common::InternalError(
              "ExtractConnectionDescriptorEntries",
              std::format(
                  "trigger slot_id {} out of range (slot_types size {})",
                  trigger_slot_it->first.value, slot_types.size()));
        }
        TypeId trigger_root_type = slot_types[trigger_slot_it->first.value];
        const auto& trigger_place = mir_arena[*entry.trigger_observed_place];
        auto range = ResolveByteRange(
            ctx, dl, type_arena, trigger_place, trigger_root_type, nullptr,
            force_two_state);
        if (range.kind == RangeKind::kPrecise) {
          trigger_byte_offset = range.byte_offset;
          trigger_byte_size = range.byte_size;
          trigger_bit_index = range.bit_index;
        }
      }
    }

    entries.push_back({
        .src_byte_offset = src_offset,
        .dst_byte_offset = dst_offset,
        .byte_size = byte_size,
        .dst_slot_id = entry.dst_slot.value,
        .trigger_slot_id = entry.trigger_slot.value,
        .trigger_edge = static_cast<uint8_t>(entry.trigger_edge),
        .trigger_bit_index = trigger_bit_index,
        .trigger_byte_offset = trigger_byte_offset,
        .trigger_byte_size = trigger_byte_size,
    });
  }

  return entries;
}

auto PrepareCombKernelInputs(
    const std::vector<TypeId>& slot_types, const mir::Arena& mir_arena,
    const TypeArena& types, const Layout& layout, const llvm::DataLayout& dl,
    llvm::LLVMContext& ctx, bool force_two_state, size_t num_init)
    -> std::vector<realization::CombKernelInput> {
  const auto& comb_entries = layout.comb_kernel_entries;
  if (comb_entries.empty()) {
    return {};
  }

  if (layout.scheduled_processes.size() < num_init) {
    throw common::InternalError(
        "PrepareCombKernelInputs", "scheduled_processes.size() < num_init");
  }

  auto num_module = layout.scheduled_processes.size() - num_init;

  // Build ProcessId -> scheduled_process_index mapping.
  // scheduled_process_index is 0-based from the first module process
  // (after init processes), matching the process meta table row order.
  std::unordered_map<uint32_t, uint32_t> proc_id_to_scheduled_index;
  for (uint32_t pi = 0; pi < num_module; ++pi) {
    auto proc_id = layout.scheduled_processes[num_init + pi].process_id;
    proc_id_to_scheduled_index[proc_id.value] = pi;
  }

  std::vector<realization::CombKernelInput> inputs;
  inputs.reserve(comb_entries.size());

  for (const auto& ck : comb_entries) {
    auto it = proc_id_to_scheduled_index.find(ck.process_id.value);
    if (it == proc_id_to_scheduled_index.end()) {
      throw common::InternalError(
          "PrepareCombKernelInputs",
          "comb kernel process not found in scheduled process list");
    }

    // Resolve each symbolic trigger to a concrete byte range, then group
    // by slot_id and merge into one trigger per (kernel, slot).
    struct SlotAccum {
      uint32_t byte_offset = 0;
      uint32_t byte_size = 0;
      bool is_full_slot = false;
    };
    std::unordered_map<uint32_t, SlotAccum> per_slot;

    for (const auto& trigger : ck.triggers) {
      uint32_t slot_id = trigger.slot.value;
      auto& accum = per_slot[slot_id];

      if (accum.is_full_slot) continue;

      if (!trigger.observed_place) {
        accum.is_full_slot = true;
        accum.byte_offset = 0;
        accum.byte_size = 0;
        continue;
      }

      // Verify slot exists in design layout (byte-range resolution requires
      // it).
      if (!layout.design.slot_to_field.contains(trigger.slot)) {
        accum.is_full_slot = true;
        accum.byte_offset = 0;
        accum.byte_size = 0;
        continue;
      }

      if (trigger.slot.value >= slot_types.size()) {
        throw common::InternalError(
            "PrepareCombKernelInputs",
            std::format(
                "trigger slot_id {} out of range (slot_types size {})",
                trigger.slot.value, slot_types.size()));
      }
      TypeId root_type = slot_types[trigger.slot.value];
      const auto& place = mir_arena[*trigger.observed_place];
      auto range = ResolveByteRange(
          ctx, dl, types, place, root_type, nullptr, force_two_state);

      if (range.kind != RangeKind::kPrecise) {
        accum.is_full_slot = true;
        accum.byte_offset = 0;
        accum.byte_size = 0;
        continue;
      }

      if (accum.byte_size == 0 && !accum.is_full_slot) {
        // First precise range on this slot
        accum.byte_offset = range.byte_offset;
        accum.byte_size = range.byte_size;
      } else {
        // Merge: bounding range [min_offset, max_end)
        uint32_t existing_end = accum.byte_offset + accum.byte_size;
        uint32_t new_end = range.byte_offset + range.byte_size;
        accum.byte_offset = std::min(accum.byte_offset, range.byte_offset);
        accum.byte_size = std::max(existing_end, new_end) - accum.byte_offset;
      }
    }

    std::vector<realization::CombTriggerInput> merged_triggers;
    merged_triggers.reserve(per_slot.size());
    for (const auto& [slot_id, accum] : per_slot) {
      if (accum.is_full_slot) {
        merged_triggers.push_back({.slot_id = slot_id});
      } else {
        merged_triggers.push_back({
            .slot_id = slot_id,
            .byte_offset = accum.byte_offset,
            .byte_size = accum.byte_size,
        });
      }
    }

    // Deterministic output: sort by slot_id (unordered_map iteration is
    // random).
    std::ranges::sort(
        merged_triggers, {}, &realization::CombTriggerInput::slot_id);

    inputs.push_back({
        .scheduled_process_index = it->second,
        .triggers = std::move(merged_triggers),
        .has_self_edge = ck.has_self_edge,
    });
  }

  return inputs;
}

auto EmitDesignMetadataGlobals(
    Context& context, const realization::DesignMetadata& metadata,
    llvm::IRBuilder<>& builder) -> MetadataGlobals {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  MetadataGlobals result;

  // Verify serialized table shapes before deriving counts.
  if (!metadata.slot_meta_words.empty() &&
      metadata.slot_meta_words.size() % runtime::slot_meta_abi::kStride != 0) {
    throw common::InternalError(
        "EmitDesignMetadataGlobals",
        "slot_meta_words size not divisible by kStride");
  }
  if (!metadata.process_meta.words.empty() &&
      metadata.process_meta.words.size() % runtime::process_meta_abi::kStride !=
          0) {
    throw common::InternalError(
        "EmitDesignMetadataGlobals",
        "process_meta words size not divisible by kStride");
  }
  if (!metadata.loop_site_meta.words.empty() &&
      metadata.loop_site_meta.words.size() %
              runtime::loop_site_meta_abi::kStride !=
          0) {
    throw common::InternalError(
        "EmitDesignMetadataGlobals",
        "loop_site_meta words size not divisible by kStride");
  }

  // Slot meta: already serialized as word array by link
  result.slot_meta_words = EmitWordArrayGlobal(
      mod, ctx, metadata.slot_meta_words, "__lyra_slot_meta_table");
  result.slot_meta_count = static_cast<uint32_t>(
      metadata.slot_meta_words.size() / runtime::slot_meta_abi::kStride);

  // Process meta
  result.process_meta_words = EmitWordArrayGlobal(
      mod, ctx, metadata.process_meta.words, "__lyra_process_meta_table");
  result.process_meta_count = static_cast<uint32_t>(
      metadata.process_meta.words.size() / runtime::process_meta_abi::kStride);
  result.process_meta_pool = EmitPoolGlobal(
      mod, ctx, metadata.process_meta.pool, "__lyra_process_meta_pool");
  result.process_meta_pool_size =
      static_cast<uint32_t>(metadata.process_meta.pool.size());

  // Loop site meta
  result.loop_site_meta_words = EmitWordArrayGlobal(
      mod, ctx, metadata.loop_site_meta.words, "__lyra_loop_site_meta_table");
  result.loop_site_meta_count = static_cast<uint32_t>(
      metadata.loop_site_meta.words.size() /
      runtime::loop_site_meta_abi::kStride);
  result.loop_site_meta_pool = EmitPoolGlobal(
      mod, ctx, metadata.loop_site_meta.pool, "__lyra_loop_site_meta_pool");
  result.loop_site_meta_pool_size =
      static_cast<uint32_t>(metadata.loop_site_meta.pool.size());

  // Connection descriptors
  auto num_conn = static_cast<uint32_t>(metadata.connection_descriptors.size());
  result.conn_desc_count = num_conn;
  if (num_conn > 0) {
    auto* i8_llvm = llvm::Type::getInt8Ty(ctx);
    auto* i16_llvm = llvm::Type::getInt16Ty(ctx);
    auto* i32_llvm = llvm::Type::getInt32Ty(ctx);

    auto* conn_desc_llvm_type = GetConnectionDescriptorLlvmType(ctx);

    std::vector<llvm::Constant*> desc_constants;
    desc_constants.reserve(num_conn);

    for (const auto& desc : metadata.connection_descriptors) {
      desc_constants.push_back(
          llvm::ConstantStruct::get(
              conn_desc_llvm_type,
              {llvm::ConstantInt::get(i32_llvm, desc.src_byte_offset),
               llvm::ConstantInt::get(i32_llvm, desc.dst_byte_offset),
               llvm::ConstantInt::get(i32_llvm, desc.byte_size),
               llvm::ConstantInt::get(i32_llvm, desc.dst_slot_id),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_slot_id),
               llvm::ConstantInt::get(i8_llvm, desc.trigger_edge),
               llvm::ConstantInt::get(i8_llvm, desc.trigger_bit_index),
               llvm::ConstantInt::get(i16_llvm, 0),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_byte_offset),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_byte_size)}));
    }

    auto* desc_array_type = llvm::ArrayType::get(conn_desc_llvm_type, num_conn);
    auto* desc_table = new llvm::GlobalVariable(
        mod, desc_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(desc_array_type, desc_constants),
        "__lyra_conn_descs");
    desc_table->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    result.conn_desc_table = llvm::ConstantExpr::getInBoundsGetElementPtr(
        desc_array_type, desc_table,
        llvm::ArrayRef<llvm::Constant*>{
            llvm::ConstantInt::get(i32_ty, 0),
            llvm::ConstantInt::get(i32_ty, 0)});
  } else {
    result.conn_desc_table = null_ptr;
  }

  // Comb kernel words
  result.comb_kernel_words = EmitWordArrayGlobal(
      mod, ctx, metadata.comb_kernel_words, "__lyra_comb_kernel_words");
  result.comb_kernel_word_count =
      static_cast<uint32_t>(metadata.comb_kernel_words.size());

  // Instance paths
  auto num_paths = static_cast<uint32_t>(metadata.instance_paths.size());
  result.instance_path_count = num_paths;
  if (num_paths > 0) {
    std::vector<llvm::Constant*> path_constants;
    path_constants.reserve(num_paths);
    for (uint32_t i = 0; i < num_paths; ++i) {
      auto* path_str = builder.CreateGlobalStringPtr(
          metadata.instance_paths[i], std::format("inst_path_{}", i));
      path_constants.push_back(llvm::cast<llvm::Constant>(path_str));
    }
    auto* path_array_type = llvm::ArrayType::get(ptr_ty, num_paths);
    auto* paths_global = new llvm::GlobalVariable(
        mod, path_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(path_array_type, path_constants),
        "__lyra_instance_paths");
    paths_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    result.instance_paths_array = llvm::ConstantExpr::getInBoundsGetElementPtr(
        path_array_type, paths_global,
        llvm::ArrayRef<llvm::Constant*>{
            llvm::ConstantInt::get(i32_ty, 0),
            llvm::ConstantInt::get(i32_ty, 0)});
  } else {
    result.instance_paths_array = null_ptr;
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
