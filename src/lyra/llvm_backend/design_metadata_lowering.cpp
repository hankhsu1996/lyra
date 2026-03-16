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
#include "lyra/common/overloaded.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/process_meta.hpp"
#include "lyra/runtime/process_meta_abi.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/runtime/trace_signal_meta_abi.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Derive slot storage kind entirely from resolved storage spec.
auto ClassifySlotStorageKind(const SlotStorageSpec& spec)
    -> runtime::SlotStorageKind {
  return std::visit(
      common::Overloaded{
          [](const PackedStorageSpec& s) {
            return s.is_four_state ? runtime::SlotStorageKind::kPacked4
                                   : runtime::SlotStorageKind::kPacked2;
          },
          [](const FloatStorageSpec&) {
            return runtime::SlotStorageKind::kPacked2;
          },
          [](const ArrayStorageSpec&) {
            return runtime::SlotStorageKind::kAggregate;
          },
          [](const StructStorageSpec&) {
            return runtime::SlotStorageKind::kAggregate;
          },
          [](const UnionStorageSpec&) {
            return runtime::SlotStorageKind::kAggregate;
          },
          [](const HandleStorageSpec& s) {
            return s.kind == HandleKind::kString
                       ? runtime::SlotStorageKind::kString
                       : runtime::SlotStorageKind::kHandle;
          },
      },
      spec.data);
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

  auto span = diag_ctx->ResolveToSpan(origin);
  if (!span || !span->file_id) {
    return {};
  }

  const FileInfo* file = source_manager->GetFile(span->file_id);
  if (file == nullptr) {
    return {};
  }

  auto [line, col] =
      source_manager->OffsetToLineCol(span->file_id, span->begin);
  if (line == 0) {
    return {};
  }

  return {.file = file->path, .line = line, .col = col};
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

// Read a NUL-terminated string from a char pool at the given offset.
// Throws InternalError on invalid offset (compile-side invariant).
//
// Pool contract: every stored string is NUL-terminated, and every valid
// offset points to the first byte of such a string. The pool writer
// (StringPoolIntern) maintains this by appending '\0' after each string.
// Offset 0 is the empty-string sentinel (pool[0] == '\0').
auto ReadPoolString(const std::vector<char>& pool, uint32_t offset)
    -> std::string_view {
  if (offset >= pool.size()) {
    throw common::InternalError(
        "ReadPoolString",
        std::format(
            "offset {} out of range (pool size {})", offset, pool.size()));
  }
  return {&pool[offset]};
}

// Map SlotKind to runtime TraceSignalKind. This is the sole conversion point
// between MIR storage semantics and trace-facing signal kinds.
auto MapSlotKindToTraceKind(mir::SlotKind kind) -> runtime::TraceSignalKind {
  switch (kind) {
    case mir::SlotKind::kVariable:
      return runtime::TraceSignalKind::kVariable;
    case mir::SlotKind::kNet:
      return runtime::TraceSignalKind::kNet;
    case mir::SlotKind::kParamConst:
      return runtime::TraceSignalKind::kParam;
  }
  throw common::InternalError(
      "MapSlotKindToTraceKind",
      std::format("unknown SlotKind {}", static_cast<int>(kind)));
}

// Compute trace bit width from a type. Returns the packed bit width for
// integral/packed types, 0 for non-bit-vector types.
auto ComputeTraceBitWidth(TypeId type_id, const TypeArena& types) -> uint32_t {
  const Type& type = types[type_id];
  if (IsPacked(type)) {
    return PackedBitWidth(type, types);
  }
  return 0;
}

}  // namespace

auto PrepareTraceSignalMetaInputs(
    const std::vector<mir::SlotTraceProvenance>& provenance,
    const std::vector<char>& trace_string_pool,
    const std::vector<TypeId>& slot_types,
    const std::vector<mir::SlotKind>& slot_kinds,
    const std::vector<std::string>& instance_paths, const TypeArena& types)
    -> std::vector<realization::TraceSignalMetaInput> {
  if (provenance.empty()) return {};

  if (provenance.size() != slot_types.size()) {
    throw common::InternalError(
        "PrepareTraceSignalMetaInputs",
        std::format(
            "provenance.size() ({}) != slot_types.size() ({})",
            provenance.size(), slot_types.size()));
  }
  if (provenance.size() != slot_kinds.size()) {
    throw common::InternalError(
        "PrepareTraceSignalMetaInputs",
        std::format(
            "provenance.size() ({}) != slot_kinds.size() ({})",
            provenance.size(), slot_kinds.size()));
  }
  if (trace_string_pool.empty() || trace_string_pool[0] != '\0') {
    throw common::InternalError(
        "PrepareTraceSignalMetaInputs",
        "trace_string_pool must be non-empty and start with '\\0'");
  }

  std::vector<realization::TraceSignalMetaInput> entries;
  entries.reserve(provenance.size());

  for (uint32_t slot_id = 0; slot_id < provenance.size(); ++slot_id) {
    const auto& prov = provenance[slot_id];
    auto local_name =
        ReadPoolString(trace_string_pool, prov.local_name_str_off);

    std::string hierarchical_name;
    switch (prov.scope_kind) {
      case mir::SlotScopeKind::kPackage: {
        auto pkg_name = ReadPoolString(trace_string_pool, prov.scope_ref);
        hierarchical_name = std::format("{}.{}", pkg_name, local_name);
        break;
      }
      case mir::SlotScopeKind::kInstance: {
        if (prov.scope_ref >= instance_paths.size()) {
          throw common::InternalError(
              "PrepareTraceSignalMetaInputs",
              std::format(
                  "slot {} instance scope_ref {} out of range "
                  "(instance_paths size {})",
                  slot_id, prov.scope_ref, instance_paths.size()));
        }
        const auto& inst_path = instance_paths[prov.scope_ref];
        hierarchical_name = std::format("{}.{}", inst_path, local_name);
        break;
      }
    }

    entries.push_back(
        {.hierarchical_name = std::move(hierarchical_name),
         .bit_width = ComputeTraceBitWidth(slot_types[slot_id], types),
         .trace_kind = static_cast<uint32_t>(
             MapSlotKindToTraceKind(slot_kinds[slot_id]))});
  }

  return entries;
}

auto ExtractSlotMetaInputs(
    const std::vector<SlotInfo>& slots, const DesignLayout& design_layout)
    -> std::vector<realization::SlotMetaInput> {
  std::vector<realization::SlotMetaInput> entries;
  entries.reserve(slots.size());

  for (const auto& slot : slots) {
    auto it = design_layout.slot_to_index.find(slot.slot_id);
    if (it == design_layout.slot_to_index.end()) {
      throw common::InternalError(
          "ExtractSlotMetaInputs",
          std::format("slot_id {} not in design layout", slot.slot_id.value));
    }
    uint32_t idx = it->second;
    uint64_t byte_offset = design_layout.slot_byte_offsets[idx];
    const auto& spec = design_layout.slot_storage_specs[idx];

    auto kind = ClassifySlotStorageKind(spec);

    realization::SlotMetaInput entry{
        .byte_offset = NarrowToU32(byte_offset, "ExtractSlotMetaInputs"),
        .total_bytes = spec.TotalByteSize(),
        .storage_kind = static_cast<uint32_t>(kind),
    };

    if (kind == runtime::SlotStorageKind::kPacked4) {
      const auto& packed = std::get<PackedStorageSpec>(spec.data);
      uint32_t lane_bytes = packed.LaneByteSize();
      entry.value_offset = 0;
      entry.value_bytes = lane_bytes;
      entry.unk_offset = packed.UnknownLaneOffset();
      entry.unk_bytes = lane_bytes;
    }

    entries.push_back(entry);
  }

  return entries;
}

auto PrepareScheduledProcessInputs(
    const std::vector<std::string>& instance_paths, const mir::Design& design,
    const mir::Arena& design_arena, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager,
    const std::vector<ScheduledProcess>& scheduled_processes, size_t num_init)
    -> std::vector<realization::ScheduledProcessInput> {
  if (scheduled_processes.size() < num_init) {
    throw common::InternalError(
        "PrepareScheduledProcessInputs",
        "scheduled_processes.size() < num_init");
  }

  // Build module_index -> body_id mapping from design elements.
  // module_index corresponds to the i-th Module element (skipping Packages).
  std::vector<mir::ModuleBodyId> module_body_ids;
  for (const auto& elem : design.elements) {
    if (const auto* mod = std::get_if<mir::Module>(&elem)) {
      module_body_ids.push_back(mod->body_id);
    }
  }

  std::vector<realization::ScheduledProcessInput> entries;
  auto num_module = scheduled_processes.size() - num_init;
  entries.reserve(num_module);

  for (size_t i = num_init; i < scheduled_processes.size(); ++i) {
    const auto& bp = scheduled_processes[i];
    // Resolve from the correct arena: body arena for module-bound,
    // design arena for standalone.
    const mir::Arena& proc_arena =
        (bp.module_index && bp.module_index.value < module_body_ids.size())
            ? design.module_bodies
                  .at(module_body_ids[bp.module_index.value].value)
                  .arena
            : design_arena;
    const auto& proc = proc_arena[bp.process_id];

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

auto PrepareBackEdgeSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<realization::BackEdgeSiteInput> {
  const auto& origins = context.GetBackEdgeSiteOrigins();
  std::vector<realization::BackEdgeSiteInput> entries;
  entries.reserve(origins.size());

  for (size_t i = 0; i < origins.size(); ++i) {
    auto loc = ResolveProcessOrigin(origins[i], diag_ctx, source_manager);
    entries.push_back({
        .back_edge_site_index = static_cast<uint32_t>(i),
        .file = std::move(loc.file),
        .line = loc.line,
        .col = loc.col,
    });
  }

  return entries;
}

auto ExtractConnectionDescriptorEntries(
    const mir::Arena& mir_arena, const Layout& layout)
    -> std::vector<realization::ConnectionDescriptorEntry> {
  const auto& kernel_entries = layout.connection_kernel_entries;
  std::vector<realization::ConnectionDescriptorEntry> entries;
  entries.reserve(kernel_entries.size());

  const auto& design = layout.design;

  for (const auto& entry : kernel_entries) {
    auto src_it = design.slot_to_index.find(entry.src_slot);
    auto dst_it = design.slot_to_index.find(entry.dst_slot);
    if (src_it == design.slot_to_index.end() ||
        dst_it == design.slot_to_index.end()) {
      throw common::InternalError(
          "ExtractConnectionDescriptorEntries",
          "kernelized connection slot not in design layout");
    }

    auto src_offset = NarrowToU32(
        design.slot_byte_offsets[src_it->second],
        "ExtractConnectionDescriptorEntries src");
    auto dst_offset = NarrowToU32(
        design.slot_byte_offsets[dst_it->second],
        "ExtractConnectionDescriptorEntries dst");
    auto byte_size = design.slot_storage_specs[dst_it->second].TotalByteSize();

    uint32_t trigger_byte_offset = 0;
    uint32_t trigger_byte_size = 0;
    uint8_t trigger_bit_index = 0;
    if (entry.trigger_observed_place) {
      auto trigger_slot_it = design.slot_to_index.find(entry.trigger_slot);
      if (trigger_slot_it != design.slot_to_index.end()) {
        const auto& trigger_spec =
            design.slot_storage_specs[trigger_slot_it->second];
        const auto& trigger_place = mir_arena[*entry.trigger_observed_place];
        auto range = ResolveByteRange(
            trigger_spec, design.storage_spec_arena, trigger_place, nullptr);
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
    const mir::Arena& mir_arena, const Layout& layout, size_t num_init)
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

      auto trigger_slot_it = layout.design.slot_to_index.find(trigger.slot);
      if (trigger_slot_it == layout.design.slot_to_index.end()) {
        accum.is_full_slot = true;
        accum.byte_offset = 0;
        accum.byte_size = 0;
        continue;
      }
      const auto& trigger_spec =
          layout.design.slot_storage_specs[trigger_slot_it->second];
      const auto& place = mir_arena[*trigger.observed_place];
      auto range = ResolveByteRange(
          trigger_spec, layout.design.storage_spec_arena, place, nullptr);

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
  auto* null_ptr =
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
  if (!metadata.back_edge_site_meta.words.empty() &&
      metadata.back_edge_site_meta.words.size() %
              runtime::back_edge_site_abi::kStride !=
          0) {
    throw common::InternalError(
        "EmitDesignMetadataGlobals",
        "back_edge_site_meta words size not divisible by kStride");
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

  // Back-edge site meta
  result.back_edge_site_meta_words = EmitWordArrayGlobal(
      mod, ctx, metadata.back_edge_site_meta.words,
      "__lyra_back_edge_site_meta_table");
  result.back_edge_site_meta_count = static_cast<uint32_t>(
      metadata.back_edge_site_meta.words.size() /
      runtime::back_edge_site_abi::kStride);
  result.back_edge_site_meta_pool = EmitPoolGlobal(
      mod, ctx, metadata.back_edge_site_meta.pool,
      "__lyra_back_edge_site_meta_pool");
  result.back_edge_site_meta_pool_size =
      static_cast<uint32_t>(metadata.back_edge_site_meta.pool.size());

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

  // Trace signal meta
  if (!metadata.trace_signal_meta.words.empty() &&
      metadata.trace_signal_meta.words.size() %
              runtime::trace_signal_meta_abi::kStride !=
          0) {
    throw common::InternalError(
        "EmitDesignMetadataGlobals",
        "trace_signal_meta words size not divisible by kStride");
  }
  result.trace_signal_meta_words = EmitWordArrayGlobal(
      mod, ctx, metadata.trace_signal_meta.words,
      "__lyra_trace_signal_meta_table");
  result.trace_signal_meta_word_count =
      static_cast<uint32_t>(metadata.trace_signal_meta.words.size());
  result.trace_signal_meta_pool = EmitPoolGlobal(
      mod, ctx, metadata.trace_signal_meta.pool,
      "__lyra_trace_signal_meta_pool");
  result.trace_signal_meta_pool_size =
      static_cast<uint32_t>(metadata.trace_signal_meta.pool.size());

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
