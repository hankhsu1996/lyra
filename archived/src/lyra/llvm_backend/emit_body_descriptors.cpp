#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/emit_descriptor_utils.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/runtime/body_realization_desc.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Body-local emission structs not shared across translation units.
struct CombTemplateEmission {
  llvm::Constant* entries_ptr = nullptr;
  uint32_t num_entries = 0;
  llvm::Constant* kernels_ptr = nullptr;
  uint32_t num_kernels = 0;
};

struct DecisionTableEmission {
  llvm::Constant* tables_ptr = nullptr;
  uint32_t num_tables = 0;
};

}  // namespace

auto EmitBodyRealizationDescs(
    Context& context, const Layout& layout,
    std::span<const lowering::mir_to_llvm::CodegenSession::BodyCompiledFuncs>
        body_compiled_funcs,
    std::span<const std::vector<uint32_t>> /*child_site_to_tree_ordinal*/)
    -> std::vector<BodyDescriptorPackageEmission> {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i16_ty = llvm::Type::getInt16Ty(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  if (body_compiled_funcs.size() != layout.body_realization_infos.size()) {
    throw common::InternalError(
        "EmitBodyRealizationDescs",
        std::format(
            "body_compiled_funcs count {} != body_realization_infos count {}",
            body_compiled_funcs.size(), layout.body_realization_infos.size()));
  }

  // BodyRealizationDesc ABI (96 bytes):
  //   {u32 num_processes, u32 slot_count,
  //    u64 inline_state_size_bytes, u64 appendix_state_size_bytes,
  //    u64 total_state_size_bytes, i8 time_unit_power, i8 time_precision_power,
  //    [2 x i8 pad], u32 event_count,
  //    ptr inline_slot_offsets, u32 num_inline_slot_offsets, [4 x pad],
  //    ptr port_entries, u32 num_port_entries, [4 x pad],
  //    ptr installable_computations, ptr ic_word_pool,
  //    u32 num_ic_word_pool_entries, u32 num_installable_computations}
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* header_type = llvm::StructType::get(
      ctx, {i32_ty, i32_ty, i64_ty, i64_ty, i64_ty, i8_ty, i8_ty, i32_ty,
            ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty, i32_ty, i32_ty});
  // BodyProcessEntry ABI: {ptr shared_body_fn, u32 schema_index, u32 pad}
  auto* entry_type = llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});

  std::vector<BodyDescriptorPackageEmission> result;
  result.reserve(layout.body_realization_infos.size());

  for (size_t bi = 0; bi < layout.body_realization_infos.size(); ++bi) {
    const auto& info = layout.body_realization_infos[bi];
    const auto& rt = layout.body_runtime_descriptors[bi];
    const auto& funcs = body_compiled_funcs[bi];
    auto body_group = static_cast<uint32_t>(bi);
    auto num_procs = static_cast<uint32_t>(rt.process_schema_indices.size());

    if (funcs.functions.size() != num_procs) {
      throw common::InternalError(
          "EmitBodyRealizationDescs",
          std::format(
              "body {} compiled function count {} != process schema count {}",
              body_group, funcs.functions.size(), num_procs));
    }

    // Emit header global.
    auto header_name = std::format("__lyra_body_desc_{}", body_group);

    // Emit per-slot inline byte offsets array.
    auto* null_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    llvm::Constant* inline_slot_offsets_ptr = null_ptr;
    auto num_inline_slot_offsets =
        static_cast<uint32_t>(info.body_layout.inline_offsets.size());
    if (num_inline_slot_offsets != info.slot_count) {
      throw common::InternalError(
          "EmitBodyRealizationDescs",
          std::format(
              "body {} inline_offsets size {} != slot_count {}", body_group,
              num_inline_slot_offsets, info.slot_count));
    }
    if (info.slot_count > 0) {
      std::vector<llvm::Constant*> offset_consts;
      offset_consts.reserve(info.body_layout.inline_offsets.size());
      for (const auto& off : info.body_layout.inline_offsets) {
        offset_consts.push_back(llvm::ConstantInt::get(i32_ty, off.value));
      }
      auto offset_name =
          std::format("__lyra_body_desc_{}_inline_slot_offsets", body_group);
      auto* offset_array_type =
          llvm::ArrayType::get(i32_ty, offset_consts.size());
      auto* offset_global = new llvm::GlobalVariable(
          mod, offset_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(offset_array_type, offset_consts),
          offset_name);
      offset_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      inline_slot_offsets_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          offset_array_type, offset_global,
          llvm::ArrayRef<llvm::Constant*>{
              llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 0)});
    }

    // Emit port entries array.
    // RuntimePortEntry ABI: {u32 sym_value, u32 local_slot, u8 dir, u8, u16}
    auto* port_entry_type =
        llvm::StructType::get(ctx, {i32_ty, i32_ty, i8_ty, i8_ty, i16_ty});
    llvm::Constant* port_entries_ptr = null_ptr;
    auto num_port_entries = static_cast<uint32_t>(info.port_entries.size());
    // Validate port entries before emission.
    for (const auto& pe : info.port_entries) {
      if (pe.sym_value == UINT32_MAX) {
        throw common::InternalError(
            "EmitBodyRealizationDescs",
            std::format(
                "body {} has port entry with invalid sym_value", body_group));
      }
      if (pe.local_slot >= info.slot_count) {
        throw common::InternalError(
            "EmitBodyRealizationDescs",
            std::format(
                "body {} port entry local_slot {} >= slot_count {}", body_group,
                pe.local_slot, info.slot_count));
      }
    }
    if (num_port_entries > 0) {
      std::vector<llvm::Constant*> pe_consts;
      pe_consts.reserve(num_port_entries);
      for (const auto& pe : info.port_entries) {
        pe_consts.push_back(
            llvm::ConstantStruct::get(
                port_entry_type, {llvm::ConstantInt::get(i32_ty, pe.sym_value),
                                  llvm::ConstantInt::get(i32_ty, pe.local_slot),
                                  llvm::ConstantInt::get(i8_ty, pe.dir),
                                  llvm::ConstantInt::get(i8_ty, 0),
                                  llvm::ConstantInt::get(i16_ty, 0)}));
      }
      auto pe_name =
          std::format("__lyra_body_desc_{}_port_entries", body_group);
      auto* pe_array_type =
          llvm::ArrayType::get(port_entry_type, num_port_entries);
      auto* pe_global = new llvm::GlobalVariable(
          mod, pe_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(pe_array_type, pe_consts), pe_name);
      pe_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      port_entries_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          pe_array_type, pe_global,
          llvm::ArrayRef<llvm::Constant*>{
              llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 0)});
    }

    // Installable computation descriptor and word pool emission.
    llvm::Constant* ic_descs_ptr = null_ptr;
    llvm::Constant* ic_word_pool_ptr = null_ptr;
    uint32_t ic_word_pool_count = 0;
    uint32_t num_ic = info.num_installable_computations;

    if (num_ic > 0 && info.body != nullptr) {
      const auto& body = *info.body;
      const auto& ic_fns = funcs.installable_computation_fns;
      if (ic_fns.size() != num_ic) {
        throw common::InternalError(
            "EmitBodyRealizationDescs",
            std::format(
                "body {} ic fn count {} != num_installable_computations {}",
                body_group, ic_fns.size(), num_ic));
      }
      // InstallableComputationDesc ABI (16 bytes):
      //   { ptr eval_fn, u32 dep_pool_offset, u32 dep_count }
      // The callable is a void-returning writeback body that addresses
      // its child target through its owner's ext_ref_bindings; target
      // correlation is not part of this descriptor.
      auto* desc_type = llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});

      // Build shared word pool (dependency slot indices only) and
      // descriptor constants.
      std::vector<uint32_t> word_pool;
      std::vector<llvm::Constant*> desc_consts;
      desc_consts.reserve(num_ic);

      for (uint32_t ci = 0; ci < num_ic; ++ci) {
        const auto& ic = body.installable_computations[ci];
        auto dep_offset = static_cast<uint32_t>(word_pool.size());
        auto dep_count = static_cast<uint32_t>(ic.deps.size());
        for (const auto& slot : ic.deps) {
          word_pool.push_back(slot.value);
        }
        desc_consts.push_back(
            llvm::ConstantStruct::get(
                desc_type,
                {ic_fns[ci], llvm::ConstantInt::get(i32_ty, dep_offset),
                 llvm::ConstantInt::get(i32_ty, dep_count)}));
      }

      // Emit descriptor array.
      auto desc_name = std::format("__lyra_body_desc_{}_ic", body_group);
      auto* desc_array_type = llvm::ArrayType::get(desc_type, num_ic);
      auto* desc_global = new llvm::GlobalVariable(
          mod, desc_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(desc_array_type, desc_consts), desc_name);
      desc_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      ic_descs_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          desc_array_type, desc_global,
          llvm::ArrayRef<llvm::Constant*>{
              llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 0)});

      // Emit word pool.
      ic_word_pool_count = static_cast<uint32_t>(word_pool.size());
      if (!word_pool.empty()) {
        std::vector<llvm::Constant*> pool_consts;
        pool_consts.reserve(word_pool.size());
        for (uint32_t w : word_pool) {
          pool_consts.push_back(llvm::ConstantInt::get(i32_ty, w));
        }
        auto pool_name = std::format("__lyra_body_desc_{}_ic_pool", body_group);
        auto* pool_array_type = llvm::ArrayType::get(i32_ty, word_pool.size());
        auto* pool_global = new llvm::GlobalVariable(
            mod, pool_array_type, true, llvm::GlobalValue::InternalLinkage,
            llvm::ConstantArray::get(pool_array_type, pool_consts), pool_name);
        pool_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        ic_word_pool_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
            pool_array_type, pool_global,
            llvm::ArrayRef<llvm::Constant*>{
                llvm::ConstantInt::get(i32_ty, 0),
                llvm::ConstantInt::get(i32_ty, 0)});
      }
    }

    auto* header_val = llvm::ConstantStruct::get(
        header_type,
        {llvm::ConstantInt::get(i32_ty, num_procs),
         llvm::ConstantInt::get(i32_ty, info.slot_count),
         llvm::ConstantInt::get(i64_ty, info.inline_state_size_bytes),
         llvm::ConstantInt::get(i64_ty, info.appendix_state_size_bytes),
         llvm::ConstantInt::get(i64_ty, info.total_state_size_bytes),
         llvm::ConstantInt::get(i8_ty, info.time_unit_power),
         llvm::ConstantInt::get(i8_ty, info.time_precision_power),
         llvm::ConstantInt::get(i32_ty, info.event_count),
         inline_slot_offsets_ptr,
         llvm::ConstantInt::get(i32_ty, num_inline_slot_offsets),
         port_entries_ptr, llvm::ConstantInt::get(i32_ty, num_port_entries),
         ic_descs_ptr, ic_word_pool_ptr,
         llvm::ConstantInt::get(i32_ty, ic_word_pool_count),
         llvm::ConstantInt::get(i32_ty, num_ic)});
    auto* header_global = new llvm::GlobalVariable(
        mod, header_type, true, llvm::GlobalValue::InternalLinkage, header_val,
        header_name);
    header_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    // Emit entries array global.
    std::vector<llvm::Constant*> entry_constants;
    entry_constants.reserve(num_procs);
    for (uint32_t pi = 0; pi < num_procs; ++pi) {
      auto* fn_ptr = funcs.functions[pi];
      if (fn_ptr == nullptr) {
        throw common::InternalError(
            "EmitBodyRealizationDescs",
            std::format(
                "body {} process {} has null compiled function", body_group,
                pi));
      }
      entry_constants.push_back(
          llvm::ConstantStruct::get(
              entry_type,
              {fn_ptr,
               llvm::ConstantInt::get(i32_ty, rt.process_schema_indices[pi]),
               llvm::ConstantInt::get(i32_ty, 0)}));
    }

    llvm::Constant* entries_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    if (num_procs > 0) {
      auto entries_name =
          std::format("__lyra_body_desc_{}_entries", body_group);
      auto* entries_array_type = llvm::ArrayType::get(entry_type, num_procs);
      auto* entries_global = new llvm::GlobalVariable(
          mod, entries_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(entries_array_type, entry_constants),
          entries_name);
      entries_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      entries_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          entries_array_type, entries_global,
          llvm::ArrayRef<llvm::Constant*>{
              llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 0)});
    }

    // Validate and emit metadata template as part of body descriptor package.
    auto num_meta = static_cast<uint32_t>(rt.meta.entries.size());
    if (num_meta != num_procs) {
      throw common::InternalError(
          "EmitBodyRealizationDescs",
          std::format(
              "body {} meta entry count {} != process count {}", body_group,
              num_meta, num_procs));
    }
    auto meta_caller =
        std::format("EmitBodyRealizationDescs body {}", body_group);
    ValidateOwnedMetaTemplate(rt.meta, meta_caller.c_str());

    auto* meta_entry_type =
        llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
    llvm::Constant* meta_entries_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    if (num_meta > 0) {
      std::vector<llvm::Constant*> meta_constants;
      meta_constants.reserve(num_meta);
      for (const auto& me : rt.meta.entries) {
        meta_constants.push_back(
            llvm::ConstantStruct::get(
                meta_entry_type,
                {llvm::ConstantInt::get(i32_ty, me.kind_packed),
                 llvm::ConstantInt::get(i32_ty, me.file_pool_off),
                 llvm::ConstantInt::get(i32_ty, me.line),
                 llvm::ConstantInt::get(i32_ty, me.col)}));
      }
      auto meta_name =
          std::format("__lyra_body_desc_{}_meta_entries", body_group);
      auto* meta_array_type = llvm::ArrayType::get(meta_entry_type, num_meta);
      auto* meta_global = new llvm::GlobalVariable(
          mod, meta_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(meta_array_type, meta_constants), meta_name);
      meta_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      meta_entries_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          meta_array_type, meta_global,
          llvm::ArrayRef<llvm::Constant*>{
              llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 0)});
    }

    auto meta_pool_size = static_cast<uint32_t>(rt.meta.pool.size());
    llvm::Constant* meta_pool_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    if (meta_pool_size > 0) {
      auto pool_name = std::format("__lyra_body_desc_{}_meta_pool", body_group);
      auto* i8_ty = llvm::Type::getInt8Ty(ctx);
      std::vector<llvm::Constant*> pool_bytes;
      pool_bytes.reserve(meta_pool_size);
      for (char c : rt.meta.pool) {
        pool_bytes.push_back(
            llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(c)));
      }
      auto* pool_array_type = llvm::ArrayType::get(i8_ty, meta_pool_size);
      auto* pool_global = new llvm::GlobalVariable(
          mod, pool_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(pool_array_type, pool_bytes), pool_name);
      pool_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      auto* zero = llvm::ConstantInt::get(i32_ty, 0);
      meta_pool_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          pool_array_type, pool_global,
          llvm::ArrayRef<llvm::Constant*>{zero, zero});
    }

    // Validate and emit trigger/comb template globals.
    auto trig_caller =
        std::format("EmitBodyRealizationDescs body {} triggers", body_group);
    ValidateOwnedTriggerTemplate(rt.triggers, trig_caller.c_str());
    auto comb_caller =
        std::format("EmitBodyRealizationDescs body {} comb", body_group);
    ValidateOwnedCombTemplate(rt.comb, comb_caller.c_str());

    auto* i8_ty = llvm::Type::getInt8Ty(ctx);
    auto* null_ptr_val =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    TriggerTemplateEmission trig_emission{
        .entries_ptr = null_ptr_val,
        .num_entries = 0,
        .ranges_ptr = null_ptr_val,
        .num_ranges = 0,
        .shapes_ptr = null_ptr_val,
        .groupable_ptr = null_ptr_val,
    };
    const auto& trig = rt.triggers;
    auto num_trig_entries = static_cast<uint32_t>(trig.entries.size());
    auto num_trig_ranges = static_cast<uint32_t>(trig.proc_ranges.size());
    if (num_trig_ranges > 0) {
      // TriggerTemplateEntry = {u32, u32, u32}
      auto* trig_entry_type =
          llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty});
      std::vector<llvm::Constant*> trig_constants;
      trig_constants.reserve(num_trig_entries);
      for (const auto& te : trig.entries) {
        trig_constants.push_back(
            llvm::ConstantStruct::get(
                trig_entry_type, {llvm::ConstantInt::get(i32_ty, te.slot_id),
                                  llvm::ConstantInt::get(i32_ty, te.edge),
                                  llvm::ConstantInt::get(i32_ty, te.flags)}));
      }
      if (num_trig_entries > 0) {
        auto trig_name =
            std::format("__lyra_body_desc_{}_trigger_entries", body_group);
        auto* trig_array_type =
            llvm::ArrayType::get(trig_entry_type, num_trig_entries);
        auto* trig_global = new llvm::GlobalVariable(
            mod, trig_array_type, true, llvm::GlobalValue::InternalLinkage,
            llvm::ConstantArray::get(trig_array_type, trig_constants),
            trig_name);
        trig_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        auto* zero_c = llvm::ConstantInt::get(i32_ty, 0);
        trig_emission.entries_ptr =
            llvm::ConstantExpr::getInBoundsGetElementPtr(
                trig_array_type, trig_global,
                llvm::ArrayRef<llvm::Constant*>{zero_c, zero_c});
      }
      trig_emission.num_entries = num_trig_entries;

      // TriggerRange = {u32, u32}
      auto* range_type = llvm::StructType::get(ctx, {i32_ty, i32_ty});
      std::vector<llvm::Constant*> range_constants;
      range_constants.reserve(num_trig_ranges);
      for (const auto& r : trig.proc_ranges) {
        range_constants.push_back(
            llvm::ConstantStruct::get(
                range_type, {llvm::ConstantInt::get(i32_ty, r.start),
                             llvm::ConstantInt::get(i32_ty, r.count)}));
      }
      auto ranges_name =
          std::format("__lyra_body_desc_{}_trigger_ranges", body_group);
      auto* ranges_array_type =
          llvm::ArrayType::get(range_type, num_trig_ranges);
      auto* ranges_global = new llvm::GlobalVariable(
          mod, ranges_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(ranges_array_type, range_constants),
          ranges_name);
      ranges_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      auto* zero_c = llvm::ConstantInt::get(i32_ty, 0);
      trig_emission.ranges_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          ranges_array_type, ranges_global,
          llvm::ArrayRef<llvm::Constant*>{zero_c, zero_c});
      trig_emission.num_ranges = num_trig_ranges;

      // Shapes: u8 array
      std::vector<llvm::Constant*> shape_constants;
      shape_constants.reserve(num_trig_ranges);
      for (auto s : trig.proc_shapes) {
        shape_constants.push_back(
            llvm::ConstantInt::get(i8_ty, static_cast<uint8_t>(s)));
      }
      auto shapes_name =
          std::format("__lyra_body_desc_{}_trigger_shapes", body_group);
      auto* shapes_array_type = llvm::ArrayType::get(i8_ty, num_trig_ranges);
      auto* shapes_global = new llvm::GlobalVariable(
          mod, shapes_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(shapes_array_type, shape_constants),
          shapes_name);
      shapes_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      trig_emission.shapes_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          shapes_array_type, shapes_global,
          llvm::ArrayRef<llvm::Constant*>{zero_c, zero_c});

      // Groupable: u8 array
      std::vector<llvm::Constant*> groupable_constants;
      groupable_constants.reserve(num_trig_ranges);
      for (auto g : trig.proc_groupable) {
        groupable_constants.push_back(llvm::ConstantInt::get(i8_ty, g));
      }
      auto groupable_name =
          std::format("__lyra_body_desc_{}_trigger_groupable", body_group);
      auto* groupable_array_type = llvm::ArrayType::get(i8_ty, num_trig_ranges);
      auto* groupable_global = new llvm::GlobalVariable(
          mod, groupable_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(groupable_array_type, groupable_constants),
          groupable_name);
      groupable_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      trig_emission.groupable_ptr =
          llvm::ConstantExpr::getInBoundsGetElementPtr(
              groupable_array_type, groupable_global,
              llvm::ArrayRef<llvm::Constant*>{zero_c, zero_c});
    }

    // Emit comb template globals.
    CombTemplateEmission comb_emission{
        .entries_ptr = null_ptr_val,
        .num_entries = 0,
        .kernels_ptr = null_ptr_val,
        .num_kernels = 0,
    };
    const auto& comb = rt.comb;
    auto num_comb_entries = static_cast<uint32_t>(comb.entries.size());
    auto num_comb_kernels = static_cast<uint32_t>(comb.kernels.size());
    if (num_comb_kernels > 0) {
      // CombTemplateEntry = {u32, u32, u32, u32, u32, u32}
      auto* comb_entry_type = llvm::StructType::get(
          ctx, {i32_ty, i32_ty, i32_ty, i32_ty, i32_ty, i32_ty});
      std::vector<llvm::Constant*> comb_entry_constants;
      comb_entry_constants.reserve(num_comb_entries);
      for (const auto& ce : comb.entries) {
        comb_entry_constants.push_back(
            llvm::ConstantStruct::get(
                comb_entry_type,
                {llvm::ConstantInt::get(i32_ty, ce.slot_id),
                 llvm::ConstantInt::get(i32_ty, ce.byte_offset),
                 llvm::ConstantInt::get(i32_ty, ce.byte_size),
                 llvm::ConstantInt::get(i32_ty, ce.flags),
                 llvm::ConstantInt::get(i32_ty, ce.owner_instance_id),
                 llvm::ConstantInt::get(i32_ty, ce.local_signal_id)}));
      }
      if (num_comb_entries > 0) {
        auto comb_name =
            std::format("__lyra_body_desc_{}_comb_entries", body_group);
        auto* comb_array_type =
            llvm::ArrayType::get(comb_entry_type, num_comb_entries);
        auto* comb_global = new llvm::GlobalVariable(
            mod, comb_array_type, true, llvm::GlobalValue::InternalLinkage,
            llvm::ConstantArray::get(comb_array_type, comb_entry_constants),
            comb_name);
        comb_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        auto* zero_c = llvm::ConstantInt::get(i32_ty, 0);
        comb_emission.entries_ptr =
            llvm::ConstantExpr::getInBoundsGetElementPtr(
                comb_array_type, comb_global,
                llvm::ArrayRef<llvm::Constant*>{zero_c, zero_c});
      }
      comb_emission.num_entries = num_comb_entries;

      // CombKernelDesc = {u32, u32, u32, u8, u8, u8, u8}
      auto* kernel_type = llvm::StructType::get(
          ctx, {i32_ty, i32_ty, i32_ty, i8_ty, i8_ty, i8_ty, i8_ty});
      std::vector<llvm::Constant*> kernel_constants;
      kernel_constants.reserve(num_comb_kernels);
      for (const auto& k : comb.kernels) {
        kernel_constants.push_back(
            llvm::ConstantStruct::get(
                kernel_type,
                {llvm::ConstantInt::get(i32_ty, k.proc_within_body),
                 llvm::ConstantInt::get(i32_ty, k.trigger_start),
                 llvm::ConstantInt::get(i32_ty, k.trigger_count),
                 llvm::ConstantInt::get(i8_ty, k.has_self_edge),
                 llvm::ConstantInt::get(i8_ty, k.pad0),
                 llvm::ConstantInt::get(i8_ty, k.pad1),
                 llvm::ConstantInt::get(i8_ty, k.pad2)}));
      }
      auto kernels_name =
          std::format("__lyra_body_desc_{}_comb_kernels", body_group);
      auto* kernels_array_type =
          llvm::ArrayType::get(kernel_type, num_comb_kernels);
      auto* kernels_global = new llvm::GlobalVariable(
          mod, kernels_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(kernels_array_type, kernel_constants),
          kernels_name);
      kernels_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      auto* zero_c = llvm::ConstantInt::get(i32_ty, 0);
      comb_emission.kernels_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
          kernels_array_type, kernels_global,
          llvm::ArrayRef<llvm::Constant*>{zero_c, zero_c});
      comb_emission.num_kernels = num_comb_kernels;
    }

    auto obs_emission = EmitObservableDescriptorTemplate(
        ctx, mod, rt.observable_descriptors,
        std::format("__lyra_body_desc_{}", body_group));

    auto init_emission = EmitInitDescriptor(
        ctx, mod, rt.init.storage_recipe, rt.init.recipe_root_indices,
        rt.init.recipe_child_indices, rt.init.param_slots,
        std::format("__lyra_body_desc_{}", body_group));

    // Decision metadata tables: per body-local process, emit a
    // DecisionMetaEntry array and a DecisionTableDescriptor.
    // Layout: { i32 packed, i32 arm_count, ptr file, i32 line, i32 col }
    auto* decision_meta_type =
        llvm::StructType::get(ctx, {i32_ty, i32_ty, ptr_ty, i32_ty, i32_ty});
    auto* decision_table_type = llvm::StructType::get(ctx, {ptr_ty, i32_ty});
    DecisionTableEmission decision_emission{};
    {
      std::vector<llvm::Constant*> table_descs;
      table_descs.reserve(num_procs);
      for (uint32_t p = 0; p < num_procs; ++p) {
        const auto& per_proc = (p < rt.decision_metas.size())
                                   ? rt.decision_metas[p]
                                   : std::vector<runtime::DecisionMetaEntry>{};
        const auto& per_proc_files = (p < rt.decision_meta_files.size())
                                         ? rt.decision_meta_files[p]
                                         : std::vector<std::string>{};
        auto site_count = static_cast<uint32_t>(per_proc.size());
        llvm::Constant* meta_array_ptr = nullptr;
        if (site_count > 0) {
          std::vector<llvm::Constant*> entries_c;
          entries_c.reserve(site_count);
          for (uint32_t s = 0; s < site_count; ++s) {
            const auto& e = per_proc[s];
            // Emit file path as global string constant (or null).
            llvm::Constant* file_ptr = nullptr;
            if (s < per_proc_files.size() && !per_proc_files[s].empty()) {
              auto* str_val =
                  llvm::ConstantDataArray::getString(ctx, per_proc_files[s]);
              auto* str_global = new llvm::GlobalVariable(
                  mod, str_val->getType(), true,
                  llvm::GlobalValue::InternalLinkage, str_val,
                  std::format(
                      "__lyra_decision_file_{}_p{}_s{}", body_group, p, s));
              str_global->setUnnamedAddr(
                  llvm::GlobalValue::UnnamedAddr::Global);
              file_ptr = str_global;
            } else {
              file_ptr = llvm::ConstantPointerNull::get(
                  llvm::cast<llvm::PointerType>(ptr_ty));
            }
            entries_c.push_back(
                llvm::ConstantStruct::get(
                    decision_meta_type,
                    {llvm::ConstantInt::get(i32_ty, e.qualifier_kind_packed),
                     llvm::ConstantInt::get(i32_ty, e.arm_count), file_ptr,
                     llvm::ConstantInt::get(i32_ty, e.line),
                     llvm::ConstantInt::get(i32_ty, e.col)}));
          }
          auto* arr_type = llvm::ArrayType::get(decision_meta_type, site_count);
          auto* arr_global = new llvm::GlobalVariable(
              mod, arr_type, true, llvm::GlobalValue::InternalLinkage,
              llvm::ConstantArray::get(arr_type, entries_c),
              std::format("__lyra_decision_meta_{}_p{}", body_group, p));
          arr_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
          auto* zero = llvm::ConstantInt::get(i32_ty, 0);
          meta_array_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
              arr_type, arr_global,
              llvm::ArrayRef<llvm::Constant*>{zero, zero});
        } else {
          meta_array_ptr = llvm::ConstantPointerNull::get(
              llvm::cast<llvm::PointerType>(ptr_ty));
        }
        table_descs.push_back(
            llvm::ConstantStruct::get(
                decision_table_type,
                {meta_array_ptr, llvm::ConstantInt::get(i32_ty, site_count)}));
      }
      if (!table_descs.empty()) {
        auto* tables_arr_type =
            llvm::ArrayType::get(decision_table_type, num_procs);
        auto* tables_global = new llvm::GlobalVariable(
            mod, tables_arr_type, true, llvm::GlobalValue::InternalLinkage,
            llvm::ConstantArray::get(tables_arr_type, table_descs),
            std::format("__lyra_decision_tables_{}", body_group));
        tables_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
        auto* zero = llvm::ConstantInt::get(i32_ty, 0);
        decision_emission.tables_ptr =
            llvm::ConstantExpr::getInBoundsGetElementPtr(
                tables_arr_type, tables_global,
                llvm::ArrayRef<llvm::Constant*>{zero, zero});
        decision_emission.num_tables = num_procs;
      } else {
        decision_emission.tables_ptr = llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptr_ty));
        decision_emission.num_tables = 0;
      }
    }

    result.push_back(
        BodyDescriptorPackageEmission{
            .header_ptr = header_global,
            .entries_ptr = entries_ptr,
            .num_processes = num_procs,
            .meta =
                MetaTemplateEmission{
                    .entries_ptr = meta_entries_ptr,
                    .num_entries = num_meta,
                    .pool_ptr = meta_pool_ptr,
                    .pool_size = meta_pool_size,
                },
            .triggers = trig_emission,
            .comb_entries_ptr = comb_emission.entries_ptr,
            .num_comb_entries = comb_emission.num_entries,
            .comb_kernels_ptr = comb_emission.kernels_ptr,
            .num_comb_kernels = comb_emission.num_kernels,
            .observable = obs_emission,
            .init = init_emission,
            .decision_tables_ptr = decision_emission.tables_ptr,
            .num_decision_tables = decision_emission.num_tables,
        });
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
