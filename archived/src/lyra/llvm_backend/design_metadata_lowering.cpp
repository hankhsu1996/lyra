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
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/runtime/back_edge_site_meta.hpp"
#include "lyra/runtime/body_realization_desc.hpp"

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
  // Object/member-shaped: {src_object_index, src_byte_offset,
  //   dst_object_index, dst_byte_offset, dst_local_signal, byte_size,
  //   trigger_edge, trigger_bit_index, padding, trigger_byte_offset,
  //   trigger_byte_size, trigger_object_index, trigger_local_id}
  return llvm::StructType::create(
      ctx, {i32, i32, i32, i32, i32, i32, i8, i8, i16, i32, i32, i32, i32},
      "SerializedConnectionDescriptor");
}

}  // namespace

auto PrepareBackEdgeSiteInputs(
    std::span<const common::OriginId> origins,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<metadata::BackEdgeSiteInput> {
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

auto ExtractConnectionDescriptorEntries(
    std::span<const ConnectionKernelEntry> kernel_entries,
    std::span<const Layout::BodyRealizationInfo> body_realization_infos,
    std::span<const uint32_t> instance_body_groups)
    -> std::vector<metadata::ConnectionDescriptorEntry> {
  std::vector<metadata::ConnectionDescriptorEntry> entries;
  entries.reserve(kernel_entries.size());

  for (const auto& entry : kernel_entries) {
    // Compute body-relative byte offsets from object/member identity.
    auto src_bg = instance_body_groups[entry.src_object_index.value];
    const auto& src_layout = body_realization_infos[src_bg];
    auto src_byte_offset =
        src_layout.body_layout.inline_offsets[entry.src_local_slot.value].value;

    auto dst_bg = instance_body_groups[entry.dst_object_index.value];
    const auto& dst_layout = body_realization_infos[dst_bg];
    auto dst_byte_offset =
        dst_layout.body_layout.inline_offsets[entry.dst_local_slot.value].value;
    auto byte_size =
        dst_layout.slot_specs[entry.dst_local_slot.value].TotalByteSize();

    uint32_t trigger_byte_offset = 0;
    uint32_t trigger_byte_size = 0;
    uint8_t trigger_bit_index = 0;
    if (entry.trigger_observation) {
      trigger_byte_offset = entry.trigger_observation->byte_offset;
      trigger_byte_size = entry.trigger_observation->byte_size;
      trigger_bit_index = entry.trigger_observation->bit_index;
    }

    entries.push_back({
        .src_object_index = entry.src_object_index.value,
        .src_byte_offset = static_cast<uint32_t>(src_byte_offset),
        .dst_object_index = entry.dst_object_index.value,
        .dst_byte_offset = static_cast<uint32_t>(dst_byte_offset),
        .dst_local_signal = entry.dst_local_slot.value,
        .byte_size = byte_size,
        .trigger_edge = static_cast<uint8_t>(entry.trigger_edge),
        .trigger_bit_index = trigger_bit_index,
        .trigger_byte_offset = trigger_byte_offset,
        .trigger_byte_size = trigger_byte_size,
        .trigger_object_index = entry.trigger_object_index.value,
        .trigger_local_id = entry.trigger_local_slot.value,
        .origin = entry.origin,
    });
  }

  return entries;
}

auto EmitDesignMetadataGlobals(
    llvm::Module& mod, llvm::LLVMContext& ctx,
    const metadata::DesignMetadata& metadata) -> MetadataGlobals {
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
    auto* i32_llvm = llvm::Type::getInt32Ty(ctx);

    auto* conn_desc_llvm_type = GetConnectionDescriptorLlvmType(ctx);

    std::vector<llvm::Constant*> desc_constants;
    desc_constants.reserve(num_conn);

    auto* i16_llvm = llvm::Type::getInt16Ty(ctx);
    for (const auto& desc : metadata.connection_descriptors) {
      desc_constants.push_back(
          llvm::ConstantStruct::get(
              conn_desc_llvm_type,
              {llvm::ConstantInt::get(i32_llvm, desc.src_object_index),
               llvm::ConstantInt::get(i32_llvm, desc.src_byte_offset),
               llvm::ConstantInt::get(i32_llvm, desc.dst_object_index),
               llvm::ConstantInt::get(i32_llvm, desc.dst_byte_offset),
               llvm::ConstantInt::get(i32_llvm, desc.dst_local_signal),
               llvm::ConstantInt::get(i32_llvm, desc.byte_size),
               llvm::ConstantInt::get(i8_llvm, desc.trigger_edge),
               llvm::ConstantInt::get(i8_llvm, desc.trigger_bit_index),
               llvm::ConstantInt::get(i16_llvm, 0),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_byte_offset),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_byte_size),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_object_index),
               llvm::ConstantInt::get(i32_llvm, desc.trigger_local_id)}));
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

}  // namespace lyra::lowering::mir_to_llvm
