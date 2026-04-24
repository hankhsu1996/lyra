#include <cstdint>
#include <format>
#include <span>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/emit_descriptor_utils.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"

namespace lyra::lowering::mir_to_llvm {

// Emit __lyra_conn_realization_descs: connection realization descriptor array.
// One entry per connection process.
// ConnectionRealizationDesc ABI: {ptr fn_ptr, u32 schema_index, u32 pad}
auto EmitConnectionRealizationDescs(
    Context& context, const Layout& layout,
    std::span<llvm::Function* const> process_funcs, size_t num_init)
    -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  const auto& conn_infos = layout.connection_realization_infos;
  auto count = static_cast<uint32_t>(conn_infos.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  if (num_init + count > process_funcs.size()) {
    throw common::InternalError(
        "EmitConnectionRealizationDescs",
        std::format(
            "connection range [{}, {}) exceeds process_funcs size {}", num_init,
            num_init + count, process_funcs.size()));
  }

  auto* entry_type = llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});

  std::vector<llvm::Constant*> entries;
  entries.reserve(count);
  for (uint32_t ci = 0; ci < count; ++ci) {
    auto* fn = process_funcs[num_init + ci];
    if (fn == nullptr) {
      throw common::InternalError(
          "EmitConnectionRealizationDescs",
          std::format("connection {} has null process function", ci));
    }
    entries.push_back(
        llvm::ConstantStruct::get(
            entry_type,
            {fn, llvm::ConstantInt::get(i32_ty, conn_infos[ci].schema_index),
             llvm::ConstantInt::get(i32_ty, 0)}));
  }

  auto* array_type = llvm::ArrayType::get(entry_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, entries),
      "__lyra_conn_realization_descs");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
}

auto EmitConnectionMetaTemplate(Context& context, const Layout& layout)
    -> MetaTemplateEmission {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  const auto& conn_meta = layout.connection_templates.meta;
  auto num_entries = static_cast<uint32_t>(conn_meta.entries.size());
  auto pool_size = static_cast<uint32_t>(conn_meta.pool.size());

  if (num_entries == 0) {
    return {
        .entries_ptr = null_ptr,
        .num_entries = 0,
        .pool_ptr = null_ptr,
        .pool_size = 0};
  }

  ValidateOwnedMetaTemplate(conn_meta, "EmitConnectionMetaTemplate");

  auto* meta_entry_type =
      llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
  std::vector<llvm::Constant*> meta_constants;
  meta_constants.reserve(num_entries);
  for (const auto& me : conn_meta.entries) {
    meta_constants.push_back(
        llvm::ConstantStruct::get(
            meta_entry_type, {llvm::ConstantInt::get(i32_ty, me.kind_packed),
                              llvm::ConstantInt::get(i32_ty, me.file_pool_off),
                              llvm::ConstantInt::get(i32_ty, me.line),
                              llvm::ConstantInt::get(i32_ty, me.col)}));
  }
  auto* entries_array_type = llvm::ArrayType::get(meta_entry_type, num_entries);
  auto* entries_global = new llvm::GlobalVariable(
      mod, entries_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(entries_array_type, meta_constants),
      "__lyra_conn_meta_entries");
  entries_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* entries_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      entries_array_type, entries_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});

  llvm::Constant* pool_ptr = null_ptr;
  if (pool_size > 0) {
    auto* i8_ty = llvm::Type::getInt8Ty(ctx);
    std::vector<llvm::Constant*> pool_bytes;
    pool_bytes.reserve(pool_size);
    for (char c : conn_meta.pool) {
      pool_bytes.push_back(
          llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(c)));
    }
    auto* pool_array_type = llvm::ArrayType::get(i8_ty, pool_size);
    auto* pool_global = new llvm::GlobalVariable(
        mod, pool_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(pool_array_type, pool_bytes),
        "__lyra_conn_meta_pool");
    pool_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    pool_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        pool_array_type, pool_global,
        llvm::ArrayRef<llvm::Constant*>{zero, zero});
  }

  return {
      .entries_ptr = entries_ptr,
      .num_entries = num_entries,
      .pool_ptr = pool_ptr,
      .pool_size = pool_size};
}

auto EmitConnectionTriggerTemplate(Context& context, const Layout& layout)
    -> TriggerTemplateEmission {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  const auto& trig = layout.connection_templates.triggers;
  ValidateOwnedTriggerTemplate(trig, "EmitConnectionTriggerTemplate");
  auto num_entries = static_cast<uint32_t>(trig.entries.size());
  auto num_ranges = static_cast<uint32_t>(trig.proc_ranges.size());

  TriggerTemplateEmission result{
      .entries_ptr = null_ptr,
      .num_entries = 0,
      .ranges_ptr = null_ptr,
      .num_ranges = 0,
      .shapes_ptr = null_ptr,
      .groupable_ptr = null_ptr,
  };

  if (num_ranges == 0) return result;

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);

  // TriggerTemplateEntry = {u32, u32, u32}
  auto* entry_type = llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty});
  if (num_entries > 0) {
    std::vector<llvm::Constant*> entry_constants;
    entry_constants.reserve(num_entries);
    for (const auto& te : trig.entries) {
      entry_constants.push_back(
          llvm::ConstantStruct::get(
              entry_type, {llvm::ConstantInt::get(i32_ty, te.slot_id),
                           llvm::ConstantInt::get(i32_ty, te.edge),
                           llvm::ConstantInt::get(i32_ty, te.flags)}));
    }
    auto* array_type = llvm::ArrayType::get(entry_type, num_entries);
    auto* global = new llvm::GlobalVariable(
        mod, array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(array_type, entry_constants),
        "__lyra_conn_trigger_entries");
    global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    result.entries_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
        array_type, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
  }
  result.num_entries = num_entries;

  // TriggerRange = {u32, u32}
  auto* range_type = llvm::StructType::get(ctx, {i32_ty, i32_ty});
  std::vector<llvm::Constant*> range_constants;
  range_constants.reserve(num_ranges);
  for (const auto& r : trig.proc_ranges) {
    range_constants.push_back(
        llvm::ConstantStruct::get(
            range_type, {llvm::ConstantInt::get(i32_ty, r.start),
                         llvm::ConstantInt::get(i32_ty, r.count)}));
  }
  auto* ranges_array_type = llvm::ArrayType::get(range_type, num_ranges);
  auto* ranges_global = new llvm::GlobalVariable(
      mod, ranges_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(ranges_array_type, range_constants),
      "__lyra_conn_trigger_ranges");
  ranges_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  result.ranges_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      ranges_array_type, ranges_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});
  result.num_ranges = num_ranges;

  // Shapes: u8 array
  std::vector<llvm::Constant*> shape_constants;
  shape_constants.reserve(num_ranges);
  for (auto s : trig.proc_shapes) {
    shape_constants.push_back(
        llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(s)));
  }
  auto* shapes_array_type = llvm::ArrayType::get(i8_ty, num_ranges);
  auto* shapes_global = new llvm::GlobalVariable(
      mod, shapes_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(shapes_array_type, shape_constants),
      "__lyra_conn_trigger_shapes");
  shapes_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  result.shapes_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      shapes_array_type, shapes_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});

  // Groupable: u8 array
  std::vector<llvm::Constant*> groupable_constants;
  groupable_constants.reserve(num_ranges);
  for (auto g : trig.proc_groupable) {
    groupable_constants.push_back(llvm::ConstantInt::get(i8_ty, g));
  }
  auto* groupable_array_type = llvm::ArrayType::get(i8_ty, num_ranges);
  auto* groupable_global = new llvm::GlobalVariable(
      mod, groupable_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(groupable_array_type, groupable_constants),
      "__lyra_conn_trigger_groupable");
  groupable_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  result.groupable_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      groupable_array_type, groupable_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
