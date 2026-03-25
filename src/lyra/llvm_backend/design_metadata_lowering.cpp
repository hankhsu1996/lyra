#include "lyra/llvm_backend/design_metadata_lowering.hpp"

#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/observable_descriptor_utils.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"
#include "lyra/runtime/trace_signal_meta.hpp"
#include "lyra/runtime/trace_signal_meta_abi.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

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

auto PrepareBackEdgeSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<metadata::BackEdgeSiteInput> {
  const auto& origins = context.GetBackEdgeSiteOrigins();
  std::vector<metadata::BackEdgeSiteInput> entries;
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

auto ExtractConnectionDescriptorEntries(const Layout& layout)
    -> std::vector<metadata::ConnectionDescriptorEntry> {
  const auto& kernel_entries = layout.connection_kernel_entries;
  std::vector<metadata::ConnectionDescriptorEntry> entries;
  entries.reserve(kernel_entries.size());

  const auto& design = layout.design;

  for (const auto& entry : kernel_entries) {
    auto src_offset = NarrowToU32(
        design.GetStorageByteOffset(entry.src_slot),
        "ExtractConnectionDescriptorEntries src");
    auto dst_offset = NarrowToU32(
        design.GetStorageByteOffset(entry.dst_slot),
        "ExtractConnectionDescriptorEntries dst");
    auto byte_size = design.GetStorageSpec(entry.dst_slot).TotalByteSize();

    uint32_t trigger_byte_offset = 0;
    uint32_t trigger_byte_size = 0;
    uint8_t trigger_bit_index = 0;
    if (entry.trigger_observation) {
      trigger_byte_offset = entry.trigger_observation->byte_offset;
      trigger_byte_size = entry.trigger_observation->byte_size;
      trigger_bit_index = entry.trigger_observation->bit_index;
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
        .origin = entry.origin,
    });
  }

  return entries;
}

auto EmitDesignMetadataGlobals(
    Context& context, const metadata::DesignMetadata& metadata,
    llvm::IRBuilder<>& builder) -> MetadataGlobals {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  MetadataGlobals result;

  if (!metadata.back_edge_site_meta.words.empty() &&
      metadata.back_edge_site_meta.words.size() %
              runtime::back_edge_site_abi::kStride !=
          0) {
    throw common::InternalError(
        "EmitDesignMetadataGlobals",
        "back_edge_site_meta words size not divisible by kStride");
  }

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

  return result;
}

auto FindPortBindingForwardingCandidates(
    std::span<const metadata::ConnectionDescriptorEntry> connections,
    const Layout& layout) -> std::vector<PortBindingForwardingCandidate> {
  auto num_slots = static_cast<uint32_t>(layout.design.slots.size());

  // Dense per-slot connection usage counts and indices.
  std::vector<uint32_t> slot_write_count(num_slots, 0);
  std::vector<uint32_t> slot_trigger_count(num_slots, 0);
  std::vector<uint32_t> slot_writer_conn(num_slots, UINT32_MAX);
  std::vector<uint32_t> slot_trigger_conn(num_slots, UINT32_MAX);

  for (uint32_t ci = 0; ci < connections.size(); ++ci) {
    const auto& conn = connections[ci];
    if (conn.dst_slot_id >= num_slots) {
      throw common::InternalError(
          "FindPortBindingForwardingCandidates",
          std::format(
              "dst_slot_id {} >= num_slots {}", conn.dst_slot_id, num_slots));
    }
    if (conn.trigger_slot_id >= num_slots) {
      throw common::InternalError(
          "FindPortBindingForwardingCandidates",
          std::format(
              "trigger_slot_id {} >= num_slots {}", conn.trigger_slot_id,
              num_slots));
    }
    ++slot_write_count[conn.dst_slot_id];
    slot_writer_conn[conn.dst_slot_id] = ci;
    ++slot_trigger_count[conn.trigger_slot_id];
    slot_trigger_conn[conn.trigger_slot_id] = ci;
  }

  // Build exclusion sets from body-shaped trigger/comb templates and
  // connection templates. For body-relative entries (design-global flag
  // not set), we cannot resolve to absolute slot IDs without per-instance
  // base slot data. This is sound: missing exclusions only produce extra
  // candidates, and this analysis is logging-only (not transform-authorizing).
  std::vector<bool> is_process_trigger(num_slots, false);
  std::vector<bool> is_comb_trigger(num_slots, false);

  for (const auto& body_info : layout.body_realization_infos) {
    for (const auto& entry : body_info.triggers.entries) {
      if ((entry.flags & runtime::kTriggerTemplateFlagDesignGlobal) != 0) {
        if (entry.slot_id < num_slots) {
          is_process_trigger[entry.slot_id] = true;
        }
      }
    }
    for (const auto& entry : body_info.comb.entries) {
      if ((entry.flags & runtime::kCombTemplateFlagDesignGlobal) != 0) {
        if (entry.slot_id < num_slots) {
          is_comb_trigger[entry.slot_id] = true;
        }
      }
    }
  }

  // Connection trigger templates are always design-global.
  for (const auto& entry : layout.connection_templates.triggers.entries) {
    if (entry.slot_id < num_slots) {
      is_process_trigger[entry.slot_id] = true;
    }
  }

  // Analyze slots with exactly one writer and one downstream connection.
  // This is a broad candidate scan -- candidates are NOT proven safe for
  // transformation. Unresolved proof gaps (active trace/display references,
  // process-body reads) are flagged but do not exclude.
  std::vector<PortBindingForwardingCandidate> candidates;

  for (uint32_t slot_id = 0; slot_id < num_slots; ++slot_id) {
    if (slot_write_count[slot_id] != 1) continue;
    if (slot_trigger_count[slot_id] != 1) continue;

    uint32_t upstream_ci = slot_writer_conn[slot_id];
    uint32_t downstream_ci = slot_trigger_conn[slot_id];
    const auto& upstream = connections[upstream_ci];
    const auto& downstream = connections[downstream_ci];

    PortBindingForwardingCandidate candidate;
    candidate.intermediate_slot_id = mir::SlotId{slot_id};
    candidate.upstream_connection_index = ConnectionIndex{upstream_ci};
    candidate.downstream_connection_index = ConnectionIndex{downstream_ci};
    candidate.single_writer = true;
    candidate.single_downstream = true;

    // Today all kernelized connections are port-binding origin.
    // This check is currently always true but structurally correct.
    candidate.both_port_binding =
        upstream.origin == metadata::ConnectionKernelOrigin::kPortBinding &&
        downstream.origin == metadata::ConnectionKernelOrigin::kPortBinding;

    candidate.no_process_trigger = !is_process_trigger[slot_id];
    candidate.no_comb_trigger = !is_comb_trigger[slot_id];

    // Upstream full-copy shape: uses full-slot trigger observation
    // (trigger_byte_size == 0) and copies a nonzero byte region.
    // This does NOT prove the copy covers the entire intermediate slot
    // extent -- that would require comparing against the slot's total
    // byte size, which is not available in the connection descriptor.
    candidate.upstream_full_copy_shape = upstream.byte_size > 0 &&
                                         upstream.trigger_byte_size == 0 &&
                                         upstream.trigger_byte_offset == 0;

    // Downstream matching read shape: reads from the same byte offset
    // that upstream writes to, with the same byte count, and uses
    // full-slot trigger observation. This checks shape compatibility
    // but does NOT prove semantic equivalence of the forwarded value.
    candidate.downstream_matching_read_shape =
        downstream.src_byte_offset == upstream.dst_byte_offset &&
        downstream.byte_size == upstream.byte_size &&
        downstream.trigger_byte_size == 0 &&
        downstream.trigger_byte_offset == 0;

    // trace_ref_unresolved stays true (default). Active trace/display
    // references cannot be determined from compile-time metadata alone.

    candidates.push_back(candidate);
  }

  return candidates;
}

}  // namespace lyra::lowering::mir_to_llvm
