#include "lyra/llvm_backend/emit_design_main.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/design_metadata_lowering.hpp"
#include "lyra/llvm_backend/inspection_plan.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/process_meta_utils.hpp"
#include "lyra/llvm_backend/runtime_abi_codegen.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/four_state_init.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/realization/build_design_metadata.hpp"
#include "lyra/runtime/runtime_abi.hpp"
#include "lyra/runtime/wait_site.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct WaitSiteMetaResult {
  llvm::Constant* words_ptr;
  uint32_t count;
};

auto EmitWaitSiteMetaTable(
    Context& context, std::span<const WaitSiteEntry> entries)
    -> WaitSiteMetaResult {
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
  if (entries.empty()) {
    return {.words_ptr = null_ptr, .count = 0};
  }

  std::vector<llvm::Constant*> words;
  words.reserve(2 + (entries.size() * runtime::wait_site_abi::kStride));

  words.push_back(
      llvm::ConstantInt::get(i32_ty, runtime::wait_site_abi::kVersion));
  words.push_back(
      llvm::ConstantInt::get(i32_ty, static_cast<uint32_t>(entries.size())));

  for (uint32_t i = 0; i < entries.size(); ++i) {
    const auto& entry = entries[i];

    auto shape = runtime::WaitShapeKind::kStatic;
    if (entry.has_late_bound) {
      shape = runtime::WaitShapeKind::kRebindable;
    } else if (entry.has_container) {
      shape = runtime::WaitShapeKind::kDynamic;
    }

    uint32_t shape_and_triggers =
        (static_cast<uint32_t>(shape) & 0xFF) | (entry.num_triggers << 8);
    uint32_t flags = entry.has_late_bound ? 1U : 0U;

    words.push_back(llvm::ConstantInt::get(i32_ty, i));
    words.push_back(llvm::ConstantInt::get(i32_ty, entry.resume_block));
    words.push_back(llvm::ConstantInt::get(i32_ty, shape_and_triggers));
    words.push_back(llvm::ConstantInt::get(i32_ty, flags));
  }

  auto& mod = context.GetModule();
  auto word_count = static_cast<uint32_t>(words.size());

  auto* words_array_type = llvm::ArrayType::get(i32_ty, word_count);
  auto* words_init = llvm::ConstantArray::get(words_array_type, words);
  auto* words_global = new llvm::GlobalVariable(
      mod, words_array_type, true, llvm::GlobalValue::InternalLinkage,
      words_init, "__lyra_wait_site_meta_table");
  words_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* words_gep = llvm::ConstantExpr::getInBoundsGetElementPtr(
      words_array_type, words_global,
      llvm::ArrayRef<llvm::Constant*>{zero, zero});

  return {.words_ptr = words_gep, .count = word_count};
}

void InitializeProcessState(
    Context& context, llvm::Value* process_state, llvm::Value* design_state,
    const ProcessLayout& proc_layout, size_t process_index) {
  auto& builder = context.GetBuilder();
  auto* state_type = proc_layout.state_type;
  bool force_two_state = context.IsForceTwoState();

  EmitMemsetZero(context, process_state, state_type);
  context.EmitStoreDesignPtr(process_state, design_state);

  if (force_two_state) return;

  auto* frame_type = proc_layout.frame.llvm_type;
  auto* frame_ptr = builder.CreateStructGEP(state_type, process_state, 1);
  const auto& frame_layout = proc_layout.frame;

  std::string frame_prefix = std::format("frame.{}", process_index);
  EmitApply4StatePatches(
      context, frame_ptr, frame_layout.four_state_patches, frame_prefix);

  const auto& types = context.GetTypeArena();
  for (uint32_t i = 0; i < frame_layout.root_types.size(); ++i) {
    TypeId type_id = frame_layout.root_types[i];
    if (!context.IsFourState(type_id)) {
      continue;
    }
    if (IsScalarPatchable(type_id, types, force_two_state)) {
      continue;
    }
    auto* field_ptr = builder.CreateStructGEP(frame_type, frame_ptr, i);
    EmitSVDefaultInitAfterZero(context, field_ptr, type_id);
  }
}

struct MainFunctionSetup {
  llvm::Function* main_func;
  llvm::BasicBlock* exit_block;
  llvm::Value* design_state;
};

auto CreateMainFunction(
    Context& context, lowering::mir_to_llvm::MainAbi main_abi)
    -> MainFunctionSetup {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  bool argv_forwarding =
      main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding;

  llvm::FunctionType* main_type = nullptr;
  if (argv_forwarding) {
    auto* i32_ty = llvm::Type::getInt32Ty(ctx);
    auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
    main_type = llvm::FunctionType::get(i32_ty, {i32_ty, ptr_ty}, false);
  } else {
    main_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), false);
  }
  auto* main_func = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, "main", &mod);

  auto* entry = llvm::BasicBlock::Create(ctx, "entry", main_func);
  auto* exit_block = llvm::BasicBlock::Create(ctx, "exit", main_func);
  builder.SetInsertPoint(entry);

  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* arena_ty = llvm::ArrayType::get(i8_ty, context.GetDesignArenaSize());
  auto* design_state = builder.CreateAlloca(arena_ty, nullptr, "design_state");

  return {
      .main_func = main_func,
      .exit_block = exit_block,
      .design_state = design_state,
  };
}

void EmitDesignStateZero(Context& context, llvm::Value* design_state) {
  auto& ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* arena_ty = llvm::ArrayType::get(i8_ty, context.GetDesignArenaSize());
  EmitMemsetZero(context, design_state, arena_ty);
}

void EmitRuntimeInit(
    Context& context, llvm::Function* main_func,
    const EmitDesignMainInput& input) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  bool argv_forwarding =
      input.main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding;

  llvm::Value* fs_base_dir_str = nullptr;
  if (argv_forwarding) {
    auto* argv0 = builder.CreateLoad(
        llvm::PointerType::getUnqual(ctx), main_func->getArg(1), "argv0");
    fs_base_dir_str =
        builder.CreateCall(context.GetLyraResolveBaseDir(), {argv0});
  } else {
    fs_base_dir_str =
        builder.CreateGlobalStringPtr(input.fs_base_dir, "fs_base_dir");
  }
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  builder.CreateCall(
      context.GetLyraInitRuntime(),
      {fs_base_dir_str, llvm::ConstantInt::get(i32_ty, input.iteration_limit)});
}

void EmitInitProcesses(
    Context& context, llvm::Value* design_state, const Layout& layout,
    const std::vector<llvm::Function*>& process_funcs, size_t num_init) {
  auto& builder = context.GetBuilder();

  for (size_t i = 0; i < num_init; ++i) {
    const auto& proc_layout = layout.processes[i];

    auto* process_state = builder.CreateAlloca(
        proc_layout.state_type, nullptr, std::format("init_state_{}", i));

    InitializeProcessState(
        context, process_state, design_state, proc_layout, i);

    builder.CreateCall(
        context.GetLyraRunProcessSync(), {process_funcs[i], process_state});
  }
}

// Canonical LLVM struct type for ProcessStateSchema entries.
// Must match the runtime ProcessStateSchema layout contract:
//   {i64 state_size, i64 state_align, ptr frame_init}
auto GetProcessStateSchemaType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  return llvm::StructType::get(ctx, {i64_ty, i64_ty, ptr_ty});
}

// Canonical LLVM struct type for ProcessConstructorRecord entries.
// Emit one per-schema frame-init function for schemas needing 4-state init.
// Returns a dense vector indexed by schema index (null for schemas not needing
// init). Emission order matches Layout::state_schemas.
auto EmitPerSchemaFrameInitFunctions(Context& context, const Layout& layout)
    -> std::vector<llvm::Function*> {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  bool force_two_state = context.IsForceTwoState();

  std::vector<llvm::Function*> init_fns(layout.state_schemas.size(), nullptr);

  for (size_t si = 0; si < layout.state_schemas.size(); ++si) {
    const auto& schema = layout.state_schemas[si];
    if (!schema.needs_4state_init) continue;

    // Validate schema identity metadata consistency.
    if (schema.body_id.has_value()) {
      if (!schema.proc_within_body.has_value() ||
          schema.conn_index.has_value()) {
        throw common::InternalError(
            "EmitPerSchemaFrameInitFunctions",
            "module schema has inconsistent identity fields");
      }
    } else {
      if (schema.proc_within_body.has_value() ||
          !schema.conn_index.has_value()) {
        throw common::InternalError(
            "EmitPerSchemaFrameInitFunctions",
            "connection schema has inconsistent identity fields");
      }
    }

    if (schema.representative_process_index >= layout.processes.size()) {
      throw common::InternalError(
          "EmitPerSchemaFrameInitFunctions",
          "representative_process_index out of range");
    }

    // Generate function name from schema identity.
    std::string fn_name;
    if (schema.body_id) {
      fn_name = std::format(
          "schema_{}_{}_frame_init", schema.body_id->value,
          *schema.proc_within_body);
    } else {
      fn_name = std::format("conn_{}_frame_init", *schema.conn_index);
    }

    auto* fn_type =
        llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), {ptr_ty}, false);
    auto* fn = llvm::Function::Create(
        fn_type, llvm::Function::InternalLinkage, fn_name, &mod);

    // Save current builder state.
    auto* saved_block = context.GetBuilder().GetInsertBlock();
    auto saved_point = context.GetBuilder().GetInsertPoint();

    auto* entry = llvm::BasicBlock::Create(ctx, "entry", fn);
    context.GetBuilder().SetInsertPoint(entry);

    auto* state_ptr = fn->getArg(0);
    const auto& proc_layout =
        layout.processes[schema.representative_process_index];
    auto* state_type = proc_layout.state_type;
    auto* frame_ptr =
        context.GetBuilder().CreateStructGEP(state_type, state_ptr, 1);

    // Apply scalar 4-state patches.
    if (!proc_layout.frame.four_state_patches.IsEmpty()) {
      std::string prefix = fn_name;
      EmitApply4StatePatches(
          context, frame_ptr, proc_layout.frame.four_state_patches, prefix);
    }

    // Apply composite 4-state initialization.
    auto* frame_type = proc_layout.frame.llvm_type;
    const auto& types = context.GetTypeArena();
    for (uint32_t fi = 0; fi < proc_layout.frame.root_types.size(); ++fi) {
      TypeId type_id = proc_layout.frame.root_types[fi];
      if (!context.IsFourState(type_id)) continue;
      if (IsScalarPatchable(type_id, types, force_two_state)) continue;
      auto* field_ptr =
          context.GetBuilder().CreateStructGEP(frame_type, frame_ptr, fi);
      EmitSVDefaultInitAfterZero(context, field_ptr, type_id);
    }

    context.GetBuilder().CreateRetVoid();

    // Restore builder state.
    if (saved_block != nullptr) {
      context.GetBuilder().SetInsertPoint(saved_block, saved_point);
    }

    init_fns[si] = fn;
  }

  return init_fns;
}

// Emit __lyra_state_schemas: global constant array of ProcessStateSchema.
auto EmitProcessStateSchemas(
    Context& context, const Layout& layout,
    const std::vector<llvm::Function*>& init_fns) -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* schema_type = GetProcessStateSchemaType(ctx);

  auto count = static_cast<uint32_t>(layout.state_schemas.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  std::vector<llvm::Constant*> entries;
  entries.reserve(count);
  for (size_t si = 0; si < layout.state_schemas.size(); ++si) {
    const auto& schema = layout.state_schemas[si];
    llvm::Constant* init_fn = init_fns[si] != nullptr
                                  ? static_cast<llvm::Constant*>(init_fns[si])
                                  : llvm::ConstantPointerNull::get(
                                        llvm::cast<llvm::PointerType>(ptr_ty));
    entries.push_back(
        llvm::ConstantStruct::get(
            schema_type,
            {llvm::ConstantInt::get(i64_ty, schema.state_size),
             llvm::ConstantInt::get(i64_ty, schema.state_align), init_fn}));
  }

  auto* array_type = llvm::ArrayType::get(schema_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, entries), "__lyra_state_schemas");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
}

// Emit __lyra_connection_funcs: dense connection-only function pointer array.
// No null padding. Connection process i has its function at index i.
auto EmitConnectionProcessFunctions(
    Context& context, const std::vector<llvm::Function*>& process_funcs,
    size_t num_init, uint32_t num_connection) -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  if (num_connection == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  std::vector<llvm::Constant*> func_constants;
  func_constants.reserve(num_connection);
  for (uint32_t i = 0; i < num_connection; ++i) {
    auto* fn = process_funcs[num_init + i];
    if (fn == nullptr) {
      throw common::InternalError(
          "EmitConnectionProcessFunctions",
          "connection process has null function pointer");
    }
    func_constants.push_back(static_cast<llvm::Constant*>(fn));
  }

  auto* array_type = llvm::ArrayType::get(ptr_ty, num_connection);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, func_constants),
      "__lyra_connection_funcs");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
}

// Emit __lyra_slot_byte_offsets: compile-owned layout oracle.
// Slot-indexed, not instance-indexed. Used by the runtime constructor for
// byte-offset lookup when binding process frame headers.
auto EmitSlotByteOffsets(Context& context, const Layout& layout)
    -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  auto count = static_cast<uint32_t>(layout.design.slots.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(ctx));
  }

  // Emit owner-resolved byte offsets for all slots (including aliases,
  // which share the canonical owner's offset).
  std::vector<llvm::Constant*> entries;
  entries.reserve(count);
  for (const auto& slot_id : layout.design.slots) {
    entries.push_back(
        llvm::ConstantInt::get(
            i64_ty, layout.design.GetStorageByteOffset(slot_id)));
  }

  auto* array_type = llvm::ArrayType::get(i64_ty, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, entries),
      "__lyra_slot_byte_offsets");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
}

// Emitted metadata template: entries array + string pool as LLVM globals.
struct MetaTemplateEmission {
  llvm::Constant* entries_ptr;
  uint32_t num_entries;
  llvm::Constant* pool_ptr;
  uint32_t pool_size;
};

// Emitted trigger template arrays: entries, per-process
// ranges/shapes/groupable.
struct TriggerTemplateEmission {
  llvm::Constant* entries_ptr;
  uint32_t num_entries;
  llvm::Constant* ranges_ptr;
  uint32_t num_ranges;
  llvm::Constant* shapes_ptr;
  llvm::Constant* groupable_ptr;
};

// Emitted comb template arrays: entries and kernel descriptors.
struct CombTemplateEmission {
  llvm::Constant* entries_ptr;
  uint32_t num_entries;
  llvm::Constant* kernels_ptr;
  uint32_t num_kernels;
};

// Emitted observable descriptor template: entries array and string pool.
struct ObservableDescriptorEmission {
  llvm::Constant* entries_ptr;
  uint32_t num_entries;
  llvm::Constant* pool_ptr;
  uint32_t pool_size;
};

// Emitted body descriptor package: descriptor header, process entries,
// and metadata/trigger/comb/observable templates. One per body.
struct InitDescriptorEmission {
  llvm::Constant* recipe_ptr;
  uint32_t num_recipe_ops;
  llvm::Constant* recipe_roots_ptr;
  uint32_t num_recipe_roots;
  llvm::Constant* recipe_child_indices_ptr;
  uint32_t num_recipe_child_indices;
  llvm::Constant* param_slots_ptr;
  uint32_t num_param_slots;
};

struct DecisionTableEmission {
  llvm::Constant* tables_ptr;
  uint32_t num_tables;
};

struct BodyDescriptorPackageEmission {
  llvm::Constant* header_ptr;
  llvm::Constant* entries_ptr;
  uint32_t num_processes;
  MetaTemplateEmission meta;
  TriggerTemplateEmission triggers;
  CombTemplateEmission comb;
  ObservableDescriptorEmission observable;
  InitDescriptorEmission init;
  DecisionTableEmission decision;
};

// Validate an owned metadata template before emission: pool sentinel,
// all entry file_pool_off values in range and NUL-terminated.
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

// Emit per-body realization descriptors: one BodyRealizationDesc header,
// one BodyProcessEntry array, and one metadata template per body. Built
// from body-shaped sources (layout body_realization_infos + body compiled
// functions), not from per-instance artifacts.
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
    auto* entry_type = llvm::ArrayType::get(i32_ty, 14);
    std::vector<llvm::Constant*> entry_constants;
    entry_constants.reserve(num_entries);
    for (const auto& e : tmpl.entries) {
      std::array<llvm::Constant*, 14> fields = {
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

// Canonical LLVM struct type for runtime::ConstructionProgramEntry.
// Layout: {i32, i32, i32, i32, i64, i64} -- must match the C++ struct
// field order in construction_program_abi.hpp.
auto GetConstructionProgramEntryType(llvm::LLVMContext& ctx)
    -> llvm::StructType* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  return llvm::StructType::get(
      ctx, {i32_ty, i32_ty, i32_ty, i32_ty, i64_ty, i64_ty});
}

// Encode one ConstructionProgramEntry as an LLVM constant.
auto EncodeConstructionProgramEntry(
    llvm::StructType* ty, llvm::LLVMContext& ctx,
    const runtime::ConstructionProgramEntry& e) -> llvm::Constant* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  return llvm::ConstantStruct::get(
      ty, {llvm::ConstantInt::get(i32_ty, e.body_group),
           llvm::ConstantInt::get(i32_ty, e.path_offset),
           llvm::ConstantInt::get(i32_ty, e.param_offset),
           llvm::ConstantInt::get(i32_ty, e.param_size),
           llvm::ConstantInt::get(i64_ty, e.realized_inline_size),
           llvm::ConstantInt::get(i64_ty, e.realized_appendix_size)});
}

// Canonical LLVM struct type for runtime::BodyDescriptorRef.
// Must match the C++ struct field order in construction_program_abi.hpp.
auto GetBodyDescriptorRefType(llvm::LLVMContext& ctx) -> llvm::StructType* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  return llvm::StructType::get(
      ctx,
      {// desc, entries, num_entries
       ptr_ty, ptr_ty, i32_ty,
       // meta: entries, num, pool, pool_size
       ptr_ty, i32_ty, ptr_ty, i32_ty,
       // triggers: entries, num, ranges, num_ranges, shapes, groupable
       ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty,
       // comb: entries, num, kernels, num_kernels
       ptr_ty, i32_ty, ptr_ty, i32_ty,
       // observable: entries, num, pool, pool_size
       ptr_ty, i32_ty, ptr_ty, i32_ty,
       // init: recipe, num_ops, roots, num_roots, child_indices, num,
       //       param_slots, num
       ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
       // decision: tables, num_tables
       ptr_ty, i32_ty});
}

// Encode one BodyDescriptorPackageEmission as a BodyDescriptorRef constant.
auto EncodeBodyDescriptorRef(
    llvm::StructType* ty, llvm::LLVMContext& ctx,
    const BodyDescriptorPackageEmission& pkg) -> llvm::Constant* {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  return llvm::ConstantStruct::get(
      ty, {pkg.header_ptr,
           pkg.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.num_processes),
           pkg.meta.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.meta.num_entries),
           pkg.meta.pool_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.meta.pool_size),
           pkg.triggers.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.triggers.num_entries),
           pkg.triggers.ranges_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.triggers.num_ranges),
           pkg.triggers.shapes_ptr,
           pkg.triggers.groupable_ptr,
           pkg.comb.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.comb.num_entries),
           pkg.comb.kernels_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.comb.num_kernels),
           pkg.observable.entries_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.observable.num_entries),
           pkg.observable.pool_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.observable.pool_size),
           pkg.init.recipe_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_recipe_ops),
           pkg.init.recipe_roots_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_recipe_roots),
           pkg.init.recipe_child_indices_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_recipe_child_indices),
           pkg.init.param_slots_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.init.num_param_slots),
           pkg.decision.tables_ptr,
           llvm::ConstantInt::get(i32_ty, pkg.decision.num_tables)});
}

// Emit the construction program entry table as a single constant global.
// Returns pointer to first element, or null if empty.
auto EmitConstructionProgramGlobal(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    std::span<const runtime::ConstructionProgramEntry> entries)
    -> llvm::Constant* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  auto count = static_cast<uint32_t>(entries.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  auto* entry_type = GetConstructionProgramEntryType(ctx);

  std::vector<llvm::Constant*> constants;
  constants.reserve(count);
  for (const auto& e : entries) {
    constants.push_back(EncodeConstructionProgramEntry(entry_type, ctx, e));
  }

  auto* array_type = llvm::ArrayType::get(entry_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, constants),
      "__lyra_construction_program");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

// Emit the body descriptor ref table: one BodyDescriptorRef per body group.
// Each entry packages all already-emitted body-shaped globals into one POD
// struct that the runtime can use to reconstruct a BodyDescriptorPackage.
auto EmitBodyDescriptorRefTable(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    const std::vector<BodyDescriptorPackageEmission>& body_pkgs)
    -> llvm::Constant* {
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  auto count = static_cast<uint32_t>(body_pkgs.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(
        llvm::cast<llvm::PointerType>(ptr_ty));
  }

  auto* ref_type = GetBodyDescriptorRefType(ctx);

  std::vector<llvm::Constant*> refs;
  refs.reserve(count);
  for (const auto& pkg : body_pkgs) {
    refs.push_back(EncodeBodyDescriptorRef(ref_type, ctx, pkg));
  }

  auto* array_type = llvm::ArrayType::get(ref_type, count);
  auto* global = new llvm::GlobalVariable(
      mod, array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(array_type, refs), "__lyra_body_desc_refs");
  global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global, llvm::ArrayRef<llvm::Constant*>{zero, zero});
}

auto EmitBodyRealizationDescs(
    Context& context, const Layout& layout,
    std::span<const lowering::mir_to_llvm::CodegenSession::BodyCompiledFuncs>
        body_compiled_funcs) -> std::vector<BodyDescriptorPackageEmission> {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
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

  // BodyRealizationDesc ABI:
  //   {u32 num_processes, u32 slot_count,
  //    u64 inline_state_size_bytes, u64 appendix_state_size_bytes,
  //    u64 total_state_size_bytes, i8 time_unit_power, i8 time_precision_power}
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* header_type = llvm::StructType::get(
      ctx, {i32_ty, i32_ty, i64_ty, i64_ty, i64_ty, i8_ty, i8_ty});
  // BodyProcessEntry ABI: {ptr shared_body_fn, u32 schema_index, u32 pad}
  auto* entry_type = llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});

  std::vector<BodyDescriptorPackageEmission> result;
  result.reserve(layout.body_realization_infos.size());

  for (size_t bi = 0; bi < layout.body_realization_infos.size(); ++bi) {
    const auto& info = layout.body_realization_infos[bi];
    const auto& funcs = body_compiled_funcs[bi];
    uint32_t body_id_val = info.body_id.value;
    auto num_procs = static_cast<uint32_t>(info.process_schema_indices.size());

    if (funcs.body_id != info.body_id) {
      throw common::InternalError(
          "EmitBodyRealizationDescs",
          std::format(
              "body_id mismatch at index {}: compiled {} vs info {}", bi,
              funcs.body_id.value, body_id_val));
    }
    if (funcs.functions.size() != num_procs) {
      throw common::InternalError(
          "EmitBodyRealizationDescs",
          std::format(
              "body {} compiled function count {} != process schema count {}",
              body_id_val, funcs.functions.size(), num_procs));
    }

    // Emit header global.
    auto header_name = std::format("__lyra_body_desc_{}", body_id_val);
    auto* header_val = llvm::ConstantStruct::get(
        header_type,
        {llvm::ConstantInt::get(i32_ty, num_procs),
         llvm::ConstantInt::get(i32_ty, info.slot_count),
         llvm::ConstantInt::get(i64_ty, info.inline_state_size_bytes),
         llvm::ConstantInt::get(i64_ty, info.appendix_state_size_bytes),
         llvm::ConstantInt::get(i64_ty, info.total_state_size_bytes),
         llvm::ConstantInt::get(i8_ty, info.time_unit_power),
         llvm::ConstantInt::get(i8_ty, info.time_precision_power)});
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
                "body {} process {} has null compiled function", body_id_val,
                pi));
      }
      entry_constants.push_back(
          llvm::ConstantStruct::get(
              entry_type,
              {fn_ptr,
               llvm::ConstantInt::get(i32_ty, info.process_schema_indices[pi]),
               llvm::ConstantInt::get(i32_ty, 0)}));
    }

    llvm::Constant* entries_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    if (num_procs > 0) {
      auto entries_name =
          std::format("__lyra_body_desc_{}_entries", body_id_val);
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
    auto num_meta = static_cast<uint32_t>(info.meta.entries.size());
    if (num_meta != num_procs) {
      throw common::InternalError(
          "EmitBodyRealizationDescs",
          std::format(
              "body {} meta entry count {} != process count {}", body_id_val,
              num_meta, num_procs));
    }
    auto meta_caller =
        std::format("EmitBodyRealizationDescs body {}", body_id_val);
    ValidateOwnedMetaTemplate(info.meta, meta_caller.c_str());

    auto* meta_entry_type =
        llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
    llvm::Constant* meta_entries_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    if (num_meta > 0) {
      std::vector<llvm::Constant*> meta_constants;
      meta_constants.reserve(num_meta);
      for (const auto& me : info.meta.entries) {
        meta_constants.push_back(
            llvm::ConstantStruct::get(
                meta_entry_type,
                {llvm::ConstantInt::get(i32_ty, me.kind_packed),
                 llvm::ConstantInt::get(i32_ty, me.file_pool_off),
                 llvm::ConstantInt::get(i32_ty, me.line),
                 llvm::ConstantInt::get(i32_ty, me.col)}));
      }
      auto meta_name =
          std::format("__lyra_body_desc_{}_meta_entries", body_id_val);
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

    auto meta_pool_size = static_cast<uint32_t>(info.meta.pool.size());
    llvm::Constant* meta_pool_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    if (meta_pool_size > 0) {
      auto pool_name =
          std::format("__lyra_body_desc_{}_meta_pool", body_id_val);
      auto* i8_ty = llvm::Type::getInt8Ty(ctx);
      std::vector<llvm::Constant*> pool_bytes;
      pool_bytes.reserve(meta_pool_size);
      for (char c : info.meta.pool) {
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
        std::format("EmitBodyRealizationDescs body {} triggers", body_id_val);
    ValidateOwnedTriggerTemplate(info.triggers, trig_caller.c_str());
    auto comb_caller =
        std::format("EmitBodyRealizationDescs body {} comb", body_id_val);
    ValidateOwnedCombTemplate(info.comb, comb_caller.c_str());

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
    const auto& trig = info.triggers;
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
            std::format("__lyra_body_desc_{}_trigger_entries", body_id_val);
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
          std::format("__lyra_body_desc_{}_trigger_ranges", body_id_val);
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
          std::format("__lyra_body_desc_{}_trigger_shapes", body_id_val);
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
          std::format("__lyra_body_desc_{}_trigger_groupable", body_id_val);
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
    const auto& comb = info.comb;
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
            std::format("__lyra_body_desc_{}_comb_entries", body_id_val);
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
          std::format("__lyra_body_desc_{}_comb_kernels", body_id_val);
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
        ctx, mod, info.observable_descriptors,
        std::format("__lyra_body_desc_{}", body_id_val));

    auto init_emission = EmitInitDescriptor(
        ctx, mod, info.init.storage_recipe, info.init.recipe_root_indices,
        info.init.recipe_child_indices, info.init.param_slots,
        std::format("__lyra_body_desc_{}", body_id_val));

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
        const auto& per_proc = (p < info.decision_metas.size())
                                   ? info.decision_metas[p]
                                   : std::vector<runtime::DecisionMetaEntry>{};
        const auto& per_proc_files = (p < info.decision_meta_files.size())
                                         ? info.decision_meta_files[p]
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
                      "__lyra_decision_file_{}_p{}_s{}", body_id_val, p, s));
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
              std::format("__lyra_decision_meta_{}_p{}", body_id_val, p));
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
            std::format("__lyra_decision_tables_{}", body_id_val));
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
            .comb = comb_emission,
            .observable = obs_emission,
            .init = init_emission,
            .decision = decision_emission,
        });
  }

  return result;
}

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

struct PlusargsSetup {
  llvm::Value* array;
  llvm::Value* count;
};

auto BuildPlusargs(
    Context& context, llvm::Function* main_func,
    const EmitDesignMainInput& input) -> PlusargsSetup {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  bool argv_forwarding =
      input.main_abi == lowering::mir_to_llvm::MainAbi::kArgvForwarding;

  if (argv_forwarding) {
    auto* argc_val = main_func->getArg(0);
    auto* argv_val = main_func->getArg(1);
    auto* argc_minus_1 =
        builder.CreateSub(argc_val, llvm::ConstantInt::get(i32_ty, 1));
    auto* is_positive =
        builder.CreateICmpSGT(argc_minus_1, llvm::ConstantInt::get(i32_ty, 0));
    auto* count = builder.CreateSelect(
        is_positive, argc_minus_1, llvm::ConstantInt::get(i32_ty, 0),
        "num_plusargs");
    auto* array = builder.CreateGEP(
        ptr_ty, argv_val, {llvm::ConstantInt::get(i32_ty, 1)}, "plusargs");
    return {.array = array, .count = count};
  }

  auto num_plusargs = static_cast<uint32_t>(input.plusargs.size());
  auto* count = llvm::ConstantInt::get(i32_ty, num_plusargs);
  if (num_plusargs > 0) {
    auto* array = builder.CreateAlloca(
        ptr_ty, llvm::ConstantInt::get(i32_ty, num_plusargs), "plusargs");
    for (uint32_t i = 0; i < num_plusargs; ++i) {
      auto* plusarg_str = builder.CreateGlobalStringPtr(
          input.plusargs[i], std::format("plusarg_{}", i));
      auto* slot = builder.CreateGEP(ptr_ty, array, {builder.getInt32(i)});
      builder.CreateStore(plusarg_str, slot);
    }
    return {.array = array, .count = count};
  }

  auto* null_array =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
  return {.array = null_array, .count = count};
}

// Metadata-building outputs: emitted metadata globals plus structured
// lowering-time analysis reports surfaced to the driver boundary.
struct DesignMetadataOutputs {
  MetadataGlobals globals;
  ForwardingAnalysisReport forwarding_analysis;
};

auto BuildDesignMetadataOutputs(
    Context& context, const Layout& layout, const EmitDesignMainInput& input)
    -> DesignMetadataOutputs {
  auto conn_desc_entries = ExtractConnectionDescriptorEntries(layout);
  auto back_edge_site_inputs =
      PrepareBackEdgeSiteInputs(context, input.diag_ctx, input.source_manager);

  // Port-binding forwarding candidate analysis (analysis only, no transform).
  ForwardingAnalysisReport forwarding_report;
  if (input.collect_forwarding_analysis) {
    forwarding_report = ForwardingAnalysisReport(
        FindPortBindingForwardingCandidates(conn_desc_entries, layout));
  }

  metadata::DesignMetadataInputs metadata_inputs{
      .back_edge_sites = std::move(back_edge_site_inputs),
      .connection_descriptors = std::move(conn_desc_entries),
  };
  auto metadata = realization::BuildDesignMetadata(metadata_inputs);

  return DesignMetadataOutputs{
      .globals = EmitDesignMetadataGlobals(context, metadata),
      .forwarding_analysis = std::move(forwarding_report),
  };
}

// Process metadata values extracted from the constructor result.
// Narrow carrier: only holds the process-metadata portion of the
// constructor result, not other constructor outputs.
struct ConstructorProcessMeta {
  llvm::Value* words = nullptr;
  llvm::Value* word_count = nullptr;
  llvm::Value* pool = nullptr;
  llvm::Value* pool_size = nullptr;
};

// Trigger/comb metadata values extracted from the constructor result.
struct ConstructorTriggerCombMeta {
  llvm::Value* trigger_words = nullptr;
  llvm::Value* trigger_word_count = nullptr;
  llvm::Value* comb_words = nullptr;
  llvm::Value* comb_word_count = nullptr;
};

// Slot/trace metadata values extracted from constructor result.
struct ConstructorSlotTraceMeta {
  llvm::Value* slot_meta_words = nullptr;
  llvm::Value* slot_meta_count = nullptr;
  llvm::Value* trace_meta_words = nullptr;
  llvm::Value* trace_meta_word_count = nullptr;
  llvm::Value* trace_meta_pool = nullptr;
  llvm::Value* trace_meta_pool_size = nullptr;
  llvm::Value* instance_paths = nullptr;
  llvm::Value* instance_path_count = nullptr;
  llvm::Value* instance_ptrs = nullptr;
  llvm::Value* instance_count = nullptr;
  llvm::Value* instance_bundles = nullptr;
  llvm::Value* instance_bundle_count = nullptr;
};

struct ConstructorEmissionResult {
  llvm::Value* states_array = nullptr;
  llvm::Value* num_total = nullptr;
  llvm::Value* result_handle = nullptr;
  llvm::Function* destroy_fn = nullptr;
  ConstructorProcessMeta process_meta;
  ConstructorTriggerCombMeta trigger_comb;
  ConstructorSlotTraceMeta slot_trace;
};

// Emit a global constant array of LyraDeferredAssertionSiteMeta structs.
// Returns the global pointer (or nullptr if no deferred sites).
auto EmitDeferredAssertionSiteMetaGlobal(
    Context& context, const std::vector<mir::DeferredAssertionSiteInfo>& sites)
    -> llvm::Constant* {
  if (sites.empty()) return nullptr;

  auto& llvm_ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i16_ty = llvm::Type::getInt16Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);

  // Struct layout matches LyraDeferredAssertionSiteMeta:
  // { i8 kind, i8 pad0, i16 pad1, i32 cover_site_id,
  //   ptr origin_file, i32 origin_line, i32 origin_col }
  auto* entry_ty = llvm::StructType::get(
      llvm_ctx, {i8_ty, i8_ty, i16_ty, i32_ty, ptr_ty, i32_ty, i32_ty});

  std::vector<llvm::Constant*> entries;
  entries.reserve(sites.size());

  for (const auto& site : sites) {
    uint8_t kind_val = static_cast<uint8_t>(site.kind);
    uint32_t cover_id = site.cover_hit.has_value()
                            ? site.cover_hit->cover_site_id.Index()
                            : UINT32_MAX;

    // Resolve source location from canonical OriginId.
    llvm::Constant* file_ptr = llvm::ConstantPointerNull::get(ptr_ty);
    uint32_t line = 0;
    uint32_t col = 0;
    if (site.origin.IsValid()) {
      auto loc = ResolveProcessOrigin(
          site.origin, &context.GetDiagnosticContext(),
          context.GetSourceManager());
      if (loc.line > 0 && !loc.file.empty()) {
        auto* str_const =
            llvm::ConstantDataArray::getString(llvm_ctx, loc.file, true);
        auto* str_global = new llvm::GlobalVariable(
            context.GetModule(), str_const->getType(), true,
            llvm::GlobalValue::PrivateLinkage, str_const);
        file_ptr = llvm::ConstantExpr::getBitCast(str_global, ptr_ty);
        line = loc.line;
        col = loc.col;
      }
    }

    entries.push_back(
        llvm::ConstantStruct::get(
            entry_ty, {llvm::ConstantInt::get(i8_ty, kind_val),
                       llvm::ConstantInt::get(i8_ty, 0),
                       llvm::ConstantInt::get(i16_ty, 0),
                       llvm::ConstantInt::get(i32_ty, cover_id), file_ptr,
                       llvm::ConstantInt::get(i32_ty, line),
                       llvm::ConstantInt::get(i32_ty, col)}));
  }

  auto* array_ty = llvm::ArrayType::get(entry_ty, entries.size());
  auto* global = new llvm::GlobalVariable(
      context.GetModule(), array_ty, true, llvm::GlobalValue::PrivateLinkage,
      llvm::ConstantArray::get(array_ty, entries),
      "__lyra_deferred_assertion_site_meta");

  return llvm::ConstantExpr::getBitCast(global, ptr_ty);
}

auto BuildRuntimeAbi(
    Context& context, const MetadataGlobals& meta_globals,
    const WaitSiteMetaResult& wait_site_meta, uint32_t num_connection,
    uint32_t feature_flags, const std::string& signal_trace_path,
    llvm::Value* design_state, const ConstructorProcessMeta& process_meta,
    const ConstructorTriggerCombMeta& trigger_comb,
    const ConstructorSlotTraceMeta& slot_trace,
    uint32_t num_immediate_cover_sites, int8_t global_precision_power,
    llvm::Constant* deferred_site_meta_global,
    uint32_t num_deferred_assertion_sites) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  auto* abi_struct_type = GetRuntimeAbiStructType(ctx);

  auto* abi_alloca = builder.CreateAlloca(abi_struct_type, nullptr, "abi");

  auto store_field = [&](unsigned idx, llvm::Value* val) {
    auto* gep = builder.CreateStructGEP(abi_struct_type, abi_alloca, idx);
    builder.CreateStore(val, gep);
  };

  store_field(0, llvm::ConstantInt::get(i32_ty, kRuntimeAbiVersion));
  store_field(1, slot_trace.slot_meta_words);
  store_field(2, slot_trace.slot_meta_count);
  // Process metadata: constructor-produced word table + string pool.
  store_field(3, process_meta.words);
  store_field(4, process_meta.word_count);
  store_field(5, process_meta.pool);
  store_field(6, process_meta.pool_size);
  store_field(7, meta_globals.back_edge_site_meta_words);
  store_field(
      8,
      llvm::ConstantInt::get(i32_ty, meta_globals.back_edge_site_meta_count));
  store_field(9, meta_globals.back_edge_site_meta_pool);
  store_field(
      10, llvm::ConstantInt::get(
              i32_ty, meta_globals.back_edge_site_meta_pool_size));
  store_field(11, meta_globals.conn_desc_table);
  store_field(12, llvm::ConstantInt::get(i32_ty, meta_globals.conn_desc_count));
  // Comb kernel metadata: constructor-produced.
  store_field(13, trigger_comb.comb_words);
  store_field(14, trigger_comb.comb_word_count);
  store_field(15, llvm::ConstantInt::get(i32_ty, feature_flags));
  store_field(16, wait_site_meta.words_ptr);
  store_field(17, llvm::ConstantInt::get(i32_ty, wait_site_meta.count));
  store_field(18, slot_trace.trace_meta_words);
  store_field(19, slot_trace.trace_meta_word_count);
  store_field(20, slot_trace.trace_meta_pool);
  store_field(21, slot_trace.trace_meta_pool_size);

  if (signal_trace_path.empty()) {
    store_field(22, null_ptr);
  } else {
    store_field(
        22,
        builder.CreateGlobalStringPtr(signal_trace_path, "signal_trace_path"));
  }

  // Process trigger metadata: constructor-produced.
  store_field(23, trigger_comb.trigger_words);
  store_field(24, trigger_comb.trigger_word_count);

  // Dispatch partition boundary (connection/module process split).
  // Currently computed from compile-time layout topology; will move
  // to constructor result extraction when remaining dispatch partition
  // ownership migrates.
  store_field(25, llvm::ConstantInt::get(i32_ty, num_connection));

  // Design-state binding.
  store_field(26, design_state);

  // Instance pointer array for slot storage resolution.
  store_field(27, slot_trace.instance_ptrs);
  store_field(28, slot_trace.instance_count);

  // R4: Per-instance metadata bundles for engine-derived registries.
  store_field(29, slot_trace.instance_bundles);
  store_field(30, slot_trace.instance_bundle_count);
  store_field(31, llvm::ConstantInt::get(i32_ty, 0));

  // A1b: Immediate cover site count.
  store_field(32, llvm::ConstantInt::get(i32_ty, num_immediate_cover_sites));
  store_field(33, llvm::ConstantInt::get(i32_ty, 0));

  // D6d: Simulation-global precision power.
  auto* i8_ty = llvm::Type::getInt8Ty(context.GetLlvmContext());
  store_field(34, llvm::ConstantInt::get(i8_ty, global_precision_power));

  // A2: Deferred assertion site metadata table.
  store_field(
      35, deferred_site_meta_global ? deferred_site_meta_global
                                    : llvm::ConstantPointerNull::get(ptr_ty));
  store_field(36, llvm::ConstantInt::get(i32_ty, num_deferred_assertion_sites));
  store_field(37, llvm::ConstantInt::get(i32_ty, 0));

  return abi_alloca;
}

void EmitRunSimulation(
    Context& context, llvm::Constant* funcs_array, llvm::Value* states_array,
    llvm::Value* num_processes, const PlusargsSetup& plusargs,
    const ConstructorSlotTraceMeta& slot_trace, llvm::Value* abi_alloca) {
  auto& builder = context.GetBuilder();

  // Instance paths now come from constructor result, not compile-time globals.
  builder.CreateCall(
      context.GetLyraRunSimulation(),
      {funcs_array, states_array, num_processes, plusargs.array, plusargs.count,
       slot_trace.instance_paths, slot_trace.instance_path_count, abi_alloca});
}

void EmitMainExit(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slot_info, const EmitDesignMainInput& input,
    llvm::BasicBlock* exit_block, llvm::Value* abi_alloca,
    const ConstructorEmissionResult& ctor_result,
    const InspectionPlan& inspection_plan) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();

  builder.CreateBr(exit_block);
  builder.SetInsertPoint(exit_block);

  // String slot release is now handled by Engine::ReleaseStringSlots()
  // inside LyraRunSimulation, before engine destruction.

  // Backend-owned variable inspection from typed placement descriptors.
  if (!inspection_plan.IsEmpty()) {
    EmitVariableInspection(
        context, inspection_plan, slot_info, design_state, abi_alloca);
  }

  if (input.hooks != nullptr) {
    input.hooks->EmitPostSimulationReports(
        context, slot_info, design_state, abi_alloca);
  }

  if (ctor_result.result_handle != nullptr) {
    builder.CreateCall(ctor_result.destroy_fn, {ctor_result.result_handle});
  }

  builder.CreateRet(llvm::ConstantInt::get(ctx, llvm::APInt(32, 0)));
}

}  // namespace

auto BuildEmitDesignMainInput(const lowering::mir_to_llvm::LoweringInput& input)
    -> EmitDesignMainInput {
  return {
      .design = input.design,
      .design_arena = input.mir_arena,
      .diag_ctx = input.diag_ctx,
      .source_manager = input.source_manager,
      .hooks = input.hooks,
      .main_abi = input.main_abi,
      .fs_base_dir = input.fs_base_dir,
      .plusargs = input.plusargs,
      .feature_flags = input.feature_flags,
      .signal_trace_path = input.signal_trace_path,
      .iteration_limit = input.iteration_limit,
      .collect_forwarding_analysis = input.collect_forwarding_analysis,
  };
}

// Runtime constructor C ABI function declarations for LLVM IR emission.
struct ConstructorRuntimeFuncs {
  llvm::Function* create;
  llvm::Function* add_connection;
  llvm::Function* run_program;
  llvm::Function* finalize;
  llvm::Function* result_get_states;
  llvm::Function* result_get_num_total;
  llvm::Function* result_get_meta_words;
  llvm::Function* result_get_meta_word_count;
  llvm::Function* result_get_meta_pool;
  llvm::Function* result_get_meta_pool_size;
  llvm::Function* result_get_trigger_words;
  llvm::Function* result_get_trigger_word_count;
  llvm::Function* result_get_comb_words;
  llvm::Function* result_get_comb_word_count;
  llvm::Function* result_get_slot_meta_words;
  llvm::Function* result_get_slot_meta_count;
  llvm::Function* result_get_trace_meta_words;
  llvm::Function* result_get_trace_meta_word_count;
  llvm::Function* result_get_trace_meta_pool;
  llvm::Function* result_get_trace_meta_pool_size;
  llvm::Function* result_get_instance_paths;
  llvm::Function* result_get_instance_path_count;
  llvm::Function* result_get_instances;
  llvm::Function* result_get_instance_count;
  llvm::Function* result_get_instance_bundles;
  llvm::Function* result_get_instance_bundle_count;
  llvm::Function* result_destroy;
};

auto DeclareConstructorRuntimeFuncs(Context& context)
    -> ConstructorRuntimeFuncs {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* void_ty = llvm::Type::getVoidTy(ctx);

  auto declare =
      [&](const char* name, llvm::Type* ret,
          std::initializer_list<llvm::Type*> args) -> llvm::Function* {
    auto* ft = llvm::FunctionType::get(ret, args, false);
    return llvm::Function::Create(
        ft, llvm::Function::ExternalLinkage, name, &mod);
  };

  return ConstructorRuntimeFuncs{
      .create = declare(
          "LyraConstructorCreate", ptr_ty,
          {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i64_ty,
           ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty,
           i32_ty, ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
           ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty}),
      .add_connection =
          declare("LyraConstructorAddConnection", void_ty, {ptr_ty, ptr_ty}),
      .run_program = declare(
          "LyraConstructorRunProgram", void_ty,
          {ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty,
           i32_ty}),
      .finalize = declare("LyraConstructorFinalize", ptr_ty, {ptr_ty}),
      .result_get_states =
          declare("LyraConstructionResultGetStates", ptr_ty, {ptr_ty}),
      .result_get_num_total =
          declare("LyraConstructionResultGetNumTotal", i32_ty, {ptr_ty}),
      .result_get_meta_words = declare(
          "LyraConstructionResultGetProcessMetaWords", ptr_ty, {ptr_ty}),
      .result_get_meta_word_count = declare(
          "LyraConstructionResultGetProcessMetaWordCount", i32_ty, {ptr_ty}),
      .result_get_meta_pool =
          declare("LyraConstructionResultGetProcessMetaPool", ptr_ty, {ptr_ty}),
      .result_get_meta_pool_size = declare(
          "LyraConstructionResultGetProcessMetaPoolSize", i32_ty, {ptr_ty}),
      .result_get_trigger_words =
          declare("LyraConstructionResultGetTriggerWords", ptr_ty, {ptr_ty}),
      .result_get_trigger_word_count = declare(
          "LyraConstructionResultGetTriggerWordCount", i32_ty, {ptr_ty}),
      .result_get_comb_words =
          declare("LyraConstructionResultGetCombWords", ptr_ty, {ptr_ty}),
      .result_get_comb_word_count =
          declare("LyraConstructionResultGetCombWordCount", i32_ty, {ptr_ty}),
      .result_get_slot_meta_words =
          declare("LyraConstructionResultGetSlotMetaWords", ptr_ty, {ptr_ty}),
      .result_get_slot_meta_count =
          declare("LyraConstructionResultGetSlotMetaCount", i32_ty, {ptr_ty}),
      .result_get_trace_meta_words = declare(
          "LyraConstructionResultGetTraceSignalMetaWords", ptr_ty, {ptr_ty}),
      .result_get_trace_meta_word_count = declare(
          "LyraConstructionResultGetTraceSignalMetaWordCount", i32_ty,
          {ptr_ty}),
      .result_get_trace_meta_pool = declare(
          "LyraConstructionResultGetTraceSignalMetaPool", ptr_ty, {ptr_ty}),
      .result_get_trace_meta_pool_size = declare(
          "LyraConstructionResultGetTraceSignalMetaPoolSize", i32_ty, {ptr_ty}),
      .result_get_instance_paths =
          declare("LyraConstructionResultGetInstancePaths", ptr_ty, {ptr_ty}),
      .result_get_instance_path_count = declare(
          "LyraConstructionResultGetInstancePathCount", i32_ty, {ptr_ty}),
      .result_get_instances =
          declare("LyraConstructionResultGetInstances", ptr_ty, {ptr_ty}),
      .result_get_instance_count =
          declare("LyraConstructionResultGetInstanceCount", i32_ty, {ptr_ty}),
      .result_get_instance_bundles =
          declare("LyraConstructionResultGetInstanceBundles", ptr_ty, {ptr_ty}),
      .result_get_instance_bundle_count = declare(
          "LyraConstructionResultGetInstanceBundleCount", i32_ty, {ptr_ty}),
      .result_destroy =
          declare("LyraConstructionResultDestroy", void_ty, {ptr_ty}),
  };
}

// Emit the constructor function: creates the runtime constructor, adds
// connections, then replays the construction program via a single
// LyraConstructorRunProgram call. The runtime replays BeginBody/AddInstance
// in strict ModuleIndex order from the compact construction program.

auto EmitConstructorFunction(
    Context& context, const Layout& layout, llvm::Value* design_state,
    llvm::Constant* schemas_ptr, uint32_t num_schemas,
    llvm::Constant* slot_byte_offsets_ptr,
    const std::vector<BodyDescriptorPackageEmission>& body_descs,
    llvm::Constant* conn_descs_ptr, const MetaTemplateEmission& conn_meta,
    const TriggerTemplateEmission& conn_triggers,
    const ObservableDescriptorEmission& pkg_observable,
    const InitDescriptorEmission& pkg_init,
    const lowering::mir_to_llvm::ConstructionProgramData& prog)
    -> ConstructorEmissionResult {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  auto rt = DeclareConstructorRuntimeFuncs(context);

  auto num_slots =
      static_cast<uint32_t>(layout.design.slot_byte_offsets.size());
  auto arena_size = layout.design.arena_size;

  // Create the runtime constructor object.
  auto* ctor = builder.CreateCall(
      rt.create,
      {schemas_ptr,
       llvm::ConstantInt::get(i32_ty, num_schemas),
       slot_byte_offsets_ptr,
       llvm::ConstantInt::get(i32_ty, num_slots),
       llvm::ConstantInt::get(i32_ty, layout.num_package_slots),
       design_state,
       llvm::ConstantInt::get(i64_ty, arena_size),
       conn_meta.entries_ptr,
       llvm::ConstantInt::get(i32_ty, conn_meta.num_entries),
       conn_meta.pool_ptr,
       llvm::ConstantInt::get(i32_ty, conn_meta.pool_size),
       conn_triggers.entries_ptr,
       llvm::ConstantInt::get(i32_ty, conn_triggers.num_entries),
       conn_triggers.ranges_ptr,
       llvm::ConstantInt::get(i32_ty, conn_triggers.num_ranges),
       conn_triggers.shapes_ptr,
       conn_triggers.groupable_ptr,
       pkg_observable.entries_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_observable.num_entries),
       pkg_observable.pool_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_observable.pool_size),
       pkg_init.recipe_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_init.num_recipe_ops),
       pkg_init.recipe_roots_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_init.num_recipe_roots),
       pkg_init.recipe_child_indices_ptr,
       llvm::ConstantInt::get(i32_ty, pkg_init.num_recipe_child_indices)},
      "ctor");

  // Add connection processes.
  const auto& conn_infos = layout.connection_realization_infos;
  for (uint32_t ci = 0; ci < conn_infos.size(); ++ci) {
    auto* conn_entry_type =
        llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});
    auto* desc_ptr = builder.CreateConstInBoundsGEP1_32(
        conn_entry_type, conn_descs_ptr, ci, std::format("conn_desc_{}", ci));
    builder.CreateCall(rt.add_connection, {ctor, desc_ptr});
  }

  // Validate: body_descs must match the body-group domain of the
  // construction program entries. This is the emission-side counterpart
  // of the lowering-side topology checks in CompileDesignProcesses.
  if (body_descs.size() != layout.body_realization_infos.size()) {
    throw common::InternalError(
        "EmitConstructorFunction",
        std::format(
            "body_descs size {} != body_realization_infos size {}",
            body_descs.size(), layout.body_realization_infos.size()));
  }
  auto body_desc_count = static_cast<uint32_t>(body_descs.size());
  for (const auto& e : prog.entries) {
    if (e.body_group >= body_desc_count) {
      throw common::InternalError(
          "EmitConstructorFunction",
          std::format(
              "construction entry body_group {} >= body_desc_count {}",
              e.body_group, body_desc_count));
    }
  }

  // Emit pooled construction program globals.
  auto* body_ref_table_ptr =
      EmitBodyDescriptorRefTable(ctx, context.GetModule(), body_descs);

  auto* path_pool_ptr = EmitBytePoolGlobal(
      ctx, context.GetModule(), llvm::ArrayRef<uint8_t>(prog.path_pool),
      "__lyra_path_pool");
  auto* param_pool_ptr = EmitBytePoolGlobal(
      ctx, context.GetModule(), llvm::ArrayRef<uint8_t>(prog.param_pool),
      "__lyra_param_pool");
  auto* program_ptr =
      EmitConstructionProgramGlobal(ctx, context.GetModule(), prog.entries);
  auto entry_count = static_cast<uint32_t>(prog.entries.size());

  // Replay the construction program: the runtime iterates entries in
  // ModuleIndex order, calling BeginBody/AddInstance as needed.
  auto path_pool_size = static_cast<uint32_t>(prog.path_pool.size());
  auto param_pool_size = static_cast<uint32_t>(prog.param_pool.size());
  builder.CreateCall(
      rt.run_program,
      {ctor, body_ref_table_ptr,
       llvm::ConstantInt::get(i32_ty, body_desc_count), path_pool_ptr,
       llvm::ConstantInt::get(i32_ty, path_pool_size), param_pool_ptr,
       llvm::ConstantInt::get(i32_ty, param_pool_size), program_ptr,
       llvm::ConstantInt::get(i32_ty, entry_count)});

  // Finalize: returns a single constructor-owned result handle.
  auto* result = builder.CreateCall(rt.finalize, {ctor}, "ctor_result");

  // Extract states, counts, and process metadata from the result.
  auto* states_array =
      builder.CreateCall(rt.result_get_states, {result}, "states");
  auto* num_total =
      builder.CreateCall(rt.result_get_num_total, {result}, "num_total");

  // Extract constructor-produced process metadata.
  auto* meta_words =
      builder.CreateCall(rt.result_get_meta_words, {result}, "meta_words");
  auto* meta_word_count = builder.CreateCall(
      rt.result_get_meta_word_count, {result}, "meta_word_count");
  auto* meta_pool =
      builder.CreateCall(rt.result_get_meta_pool, {result}, "meta_pool");
  auto* meta_pool_sz = builder.CreateCall(
      rt.result_get_meta_pool_size, {result}, "meta_pool_size");

  // Extract constructor-produced trigger/comb metadata.
  auto* trigger_words = builder.CreateCall(
      rt.result_get_trigger_words, {result}, "trigger_words");
  auto* trigger_word_count = builder.CreateCall(
      rt.result_get_trigger_word_count, {result}, "trigger_word_count");
  auto* comb_words =
      builder.CreateCall(rt.result_get_comb_words, {result}, "comb_words");
  auto* comb_word_count = builder.CreateCall(
      rt.result_get_comb_word_count, {result}, "comb_word_count");

  // Extract constructor-produced slot/trace metadata and instance paths.
  auto* slot_meta_words = builder.CreateCall(
      rt.result_get_slot_meta_words, {result}, "slot_meta_words");
  auto* slot_meta_count = builder.CreateCall(
      rt.result_get_slot_meta_count, {result}, "slot_meta_count");
  auto* trace_meta_words = builder.CreateCall(
      rt.result_get_trace_meta_words, {result}, "trace_meta_words");
  auto* trace_meta_word_count = builder.CreateCall(
      rt.result_get_trace_meta_word_count, {result}, "trace_meta_word_count");
  auto* trace_meta_pool = builder.CreateCall(
      rt.result_get_trace_meta_pool, {result}, "trace_meta_pool");
  auto* trace_meta_pool_size = builder.CreateCall(
      rt.result_get_trace_meta_pool_size, {result}, "trace_meta_pool_sz");
  auto* inst_paths = builder.CreateCall(
      rt.result_get_instance_paths, {result}, "instance_paths");
  auto* inst_path_count = builder.CreateCall(
      rt.result_get_instance_path_count, {result}, "instance_path_count");
  auto* inst_ptrs =
      builder.CreateCall(rt.result_get_instances, {result}, "instance_ptrs");
  auto* inst_count = builder.CreateCall(
      rt.result_get_instance_count, {result}, "instance_count");
  auto* inst_bundles = builder.CreateCall(
      rt.result_get_instance_bundles, {result}, "instance_bundles");
  auto* inst_bundle_count = builder.CreateCall(
      rt.result_get_instance_bundle_count, {result}, "instance_bundle_count");

  return ConstructorEmissionResult{
      .states_array = states_array,
      .num_total = num_total,
      .result_handle = result,
      .destroy_fn = rt.result_destroy,
      .process_meta =
          ConstructorProcessMeta{
              .words = meta_words,
              .word_count = meta_word_count,
              .pool = meta_pool,
              .pool_size = meta_pool_sz,
          },
      .trigger_comb =
          ConstructorTriggerCombMeta{
              .trigger_words = trigger_words,
              .trigger_word_count = trigger_word_count,
              .comb_words = comb_words,
              .comb_word_count = comb_word_count,
          },
      .slot_trace =
          ConstructorSlotTraceMeta{
              .slot_meta_words = slot_meta_words,
              .slot_meta_count = slot_meta_count,
              .trace_meta_words = trace_meta_words,
              .trace_meta_word_count = trace_meta_word_count,
              .trace_meta_pool = trace_meta_pool,
              .trace_meta_pool_size = trace_meta_pool_size,
              .instance_paths = inst_paths,
              .instance_path_count = inst_path_count,
              .instance_ptrs = inst_ptrs,
              .instance_count = inst_count,
              .instance_bundles = inst_bundles,
              .instance_bundle_count = inst_bundle_count,
          },
  };
}

auto EmitDesignMain(
    lowering::mir_to_llvm::CodegenSession& session,
    const EmitDesignMainInput& input) -> Result<LoweringReport> {
  ForwardingAnalysisReport forwarding_report;
  auto& context = *session.context;
  const auto& layout = *session.layout;
  const auto& slot_info = session.slot_info;
  const auto& process_funcs = session.process_funcs;
  size_t num_init = session.num_init_processes;
  const auto& realization = session.realization;

  auto [main_func, exit_block, design_state] =
      CreateMainFunction(context, input.main_abi);

  EmitDesignStateZero(context, design_state);

  auto num_simulation = static_cast<uint32_t>(process_funcs.size() - num_init);
  auto num_kernelized =
      static_cast<uint32_t>(layout.connection_kernel_entries.size());

  auto* null_abi_ptr = llvm::ConstantPointerNull::get(
      llvm::PointerType::getUnqual(context.GetLlvmContext()));
  llvm::Value* abi_for_exit = null_abi_ptr;
  ConstructorEmissionResult ctor_result{};

  if (num_simulation > 0 || num_kernelized > 0) {
    auto& ctx = context.GetLlvmContext();
    auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
    auto* i32_ty = llvm::Type::getInt32Ty(ctx);
    auto* null_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

    // Default to null for zero-simulation case.
    llvm::Value* states_array = null_ptr;
    llvm::Constant* connection_funcs = null_ptr;
    llvm::Value* num_total_val = llvm::ConstantInt::get(i32_ty, 0);
    auto num_connection = static_cast<uint32_t>(
        layout.num_module_process_base - layout.num_init_processes);

    if (num_simulation > 0) {
      // Emit per-schema frame-init functions.
      auto init_fns = EmitPerSchemaFrameInitFunctions(context, layout);

      // Emit shared globals: state schemas, connection function table.
      auto* schemas_ptr = EmitProcessStateSchemas(context, layout, init_fns);
      auto num_schemas = static_cast<uint32_t>(layout.state_schemas.size());
      connection_funcs = EmitConnectionProcessFunctions(
          context, process_funcs, num_init, num_connection);

      // Emit constructor-side definition artifacts.
      auto* slot_offsets_ptr = EmitSlotByteOffsets(context, layout);
      auto body_descs = EmitBodyRealizationDescs(
          context, layout, session.body_compiled_funcs);
      auto* conn_descs_ptr = EmitConnectionRealizationDescs(
          context, layout, process_funcs, num_init);
      auto conn_meta_emission = EmitConnectionMetaTemplate(context, layout);
      auto conn_trigger_emission =
          EmitConnectionTriggerTemplate(context, layout);

      // Emit package/global observable descriptor globals.
      auto pkg_obs_emission = EmitObservableDescriptorTemplate(
          ctx, context.GetModule(), layout.package_observable_descriptors,
          "__lyra_pkg");

      // Emit package/global init descriptor globals.
      auto pkg_init_emission = EmitInitDescriptor(
          ctx, context.GetModule(),
          layout.package_init_descriptor.storage_recipe,
          layout.package_init_descriptor.recipe_root_indices,
          layout.package_init_descriptor.recipe_child_indices,
          std::span<const runtime::ParamInitSlotEntry>{}, "__lyra_pkg");

      // Emit the constructor function: creates the constructor, adds
      // connections, and replays the construction program via a single
      // LyraConstructorRunProgram call with pooled path/param globals.
      ctor_result = EmitConstructorFunction(
          context, layout, design_state, schemas_ptr, num_schemas,
          slot_offsets_ptr, body_descs, conn_descs_ptr, conn_meta_emission,
          conn_trigger_emission, pkg_obs_emission, pkg_init_emission,
          realization.construction_program);

      states_array = ctor_result.states_array;
      num_total_val = ctor_result.num_total;
    }

    // Runtime init and init processes run AFTER constructor (which owns
    // design-state init) but BEFORE simulation.
    EmitRuntimeInit(context, main_func, input);

    if (input.hooks != nullptr) {
      input.hooks->OnAfterInitializeDesignState(
          context, slot_info, design_state);
    }

    EmitInitProcesses(context, design_state, layout, process_funcs, num_init);

    if (input.hooks != nullptr) {
      input.hooks->OnBeforeRunSimulation(context, slot_info, design_state);
    }

    auto plusargs = BuildPlusargs(context, main_func, input);

    auto metadata_outputs = BuildDesignMetadataOutputs(context, layout, input);
    forwarding_report = std::move(metadata_outputs.forwarding_analysis);
    auto& meta_globals = metadata_outputs.globals;

    auto wait_site_meta = EmitWaitSiteMetaTable(context, session.wait_sites);

    // Fallback: zero metadata for the no-constructor topology case
    // (num_simulation == 0 but num_kernelized > 0). When the constructor is
    // active, constructor-produced metadata replaces this.
    ConstructorProcessMeta process_meta_for_abi{
        .words = null_ptr,
        .word_count = llvm::ConstantInt::get(i32_ty, 0),
        .pool = null_ptr,
        .pool_size = llvm::ConstantInt::get(i32_ty, 0),
    };
    ConstructorTriggerCombMeta trigger_comb_for_abi{
        .trigger_words = null_ptr,
        .trigger_word_count = llvm::ConstantInt::get(i32_ty, 0),
        .comb_words = null_ptr,
        .comb_word_count = llvm::ConstantInt::get(i32_ty, 0),
    };
    ConstructorSlotTraceMeta slot_trace_for_abi{
        .slot_meta_words = null_ptr,
        .slot_meta_count = llvm::ConstantInt::get(i32_ty, 0),
        .trace_meta_words = null_ptr,
        .trace_meta_word_count = llvm::ConstantInt::get(i32_ty, 0),
        .trace_meta_pool = null_ptr,
        .trace_meta_pool_size = llvm::ConstantInt::get(i32_ty, 0),
        .instance_paths = null_ptr,
        .instance_path_count = llvm::ConstantInt::get(i32_ty, 0),
        .instance_ptrs = null_ptr,
        .instance_count = llvm::ConstantInt::get(i32_ty, 0),
        .instance_bundles = null_ptr,
        .instance_bundle_count = llvm::ConstantInt::get(i32_ty, 0),
    };
    if (ctor_result.result_handle != nullptr) {
      process_meta_for_abi = ctor_result.process_meta;
      trigger_comb_for_abi = ctor_result.trigger_comb;
      slot_trace_for_abi = ctor_result.slot_trace;
    }

    auto* deferred_site_meta_global = EmitDeferredAssertionSiteMetaGlobal(
        context, input.design->deferred_assertion_sites);
    auto* abi_alloca = BuildRuntimeAbi(
        context, meta_globals, wait_site_meta, num_connection,
        input.feature_flags, input.signal_trace_path, design_state,
        process_meta_for_abi, trigger_comb_for_abi, slot_trace_for_abi,
        static_cast<uint32_t>(input.design->immediate_cover_sites.size()),
        input.design->global_precision_power, deferred_site_meta_global,
        static_cast<uint32_t>(input.design->deferred_assertion_sites.size()));
    abi_for_exit = abi_alloca;

    EmitRunSimulation(
        context, connection_funcs, states_array, num_total_val, plusargs,
        slot_trace_for_abi, abi_alloca);

  } else {
    // No simulation processes or kernelized connections. Still need
    // runtime init and init process execution.
    EmitRuntimeInit(context, main_func, input);

    if (input.hooks != nullptr) {
      input.hooks->OnAfterInitializeDesignState(
          context, slot_info, design_state);
    }

    EmitInitProcesses(context, design_state, layout, process_funcs, num_init);

    if (input.hooks != nullptr) {
      input.hooks->OnBeforeRunSimulation(context, slot_info, design_state);
    }
  }

  // Build typed inspection plan from session data.
  // Backend owns placement construction; test framework only provides
  // identity (name + slot_id) via GetTrackedVariables().
  InspectionPlan inspection_plan;
  if (input.hooks != nullptr) {
    auto refs = input.hooks->GetTrackedVariables();
    if (!refs.empty()) {
      inspection_plan = BuildInspectionPlan(session, refs);
    }
  }

  EmitMainExit(
      context, design_state, slot_info, input, exit_block, abi_for_exit,
      ctor_result, inspection_plan);

  return LoweringReport{
      .forwarding_analysis = std::move(forwarding_report),
  };
}

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult> {
  auto session_result = CompileDesignProcesses(input);
  if (!session_result) return std::unexpected(session_result.error());

  auto emit_result =
      EmitDesignMain(*session_result, BuildEmitDesignMainInput(input));
  if (!emit_result) return std::unexpected(emit_result.error());

  return FinalizeModule(std::move(*session_result), std::move(*emit_result));
}

}  // namespace lyra::lowering::mir_to_llvm
