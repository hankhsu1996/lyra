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

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/design_metadata_lowering.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/storage_boundary.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/four_state_init.hpp"
#include "lyra/mir/design.hpp"
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

void EmitConstructOwnedHandles(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots,
    const lowering::mir_to_llvm::DesignLayout& design_layout) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  auto* handle_ty =
      llvm::StructType::get(ctx, {llvm::PointerType::getUnqual(ctx), i64_ty});

  for (size_t i = 0; i < slots.size(); ++i) {
    if (slots[i].storage_shape != mir::StorageShape::kOwnedContainer) {
      continue;
    }

    uint64_t handle_offset = design_layout.slot_byte_offsets[i];
    uint64_t data_offset = *design_layout.owned_data_offsets[i];
    uint64_t backing_size = design_layout.slot_storage_specs[i].TotalByteSize();

    auto* handle_ptr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(handle_offset),
        "owned_handle_ptr");
    auto* data_ptr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(data_offset), "owned_data_ptr");

    // Store handle.data
    auto* data_field = builder.CreateStructGEP(handle_ty, handle_ptr, 0);
    builder.CreateStore(data_ptr, data_field);

    // Store handle.byte_size
    auto* size_field = builder.CreateStructGEP(handle_ty, handle_ptr, 1);
    builder.CreateStore(
        llvm::ConstantInt::get(i64_ty, backing_size), size_field);
  }
}

void EmitInitializeOwnedBackingStorage(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots,
    const lowering::mir_to_llvm::DesignLayout& design_layout) {
  if (context.IsForceTwoState()) return;

  auto& builder = context.GetBuilder();
  auto* i8_ty = llvm::Type::getInt8Ty(context.GetLlvmContext());
  const auto& spec_arena = context.GetDesignStorageSpecArena();

  for (size_t i = 0; i < slots.size(); ++i) {
    if (slots[i].storage_shape != mir::StorageShape::kOwnedContainer) {
      continue;
    }
    const auto& backing_spec = design_layout.slot_storage_specs[i];
    if (!HasFourStateContent(backing_spec, spec_arena)) continue;

    uint64_t data_offset = *design_layout.owned_data_offsets[i];
    auto* data_ptr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(data_offset),
        "owned_backing_init_ptr");
    EmitSVDefaultInitAfterZero(context, data_ptr, slots[i].type_id);
  }
}

void InitializeDesignState(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots,
    const lowering::mir_to_llvm::DesignLayout& design_layout) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* arena_ty = llvm::ArrayType::get(i8_ty, context.GetDesignArenaSize());

  // Step 1: Zero entire arena (inline + appendix).
  EmitMemsetZero(context, design_state, arena_ty);

  if (!context.IsForceTwoState()) {
    // Step 2: Apply 4-state patches for inline scalar slots.
    EmitApply4StatePatches(
        context, design_state, design_layout.four_state_patches, "design");

    // Step 3: Recursive 4-state init for inline non-patchable slots.
    // Skips kOwnedContainer slots (their backing data is initialized in step
    // 5).
    const auto& arena = context.GetDesignStorageSpecArena();
    for (const auto& slot : slots) {
      if (slot.storage_shape == mir::StorageShape::kOwnedContainer) continue;
      const auto& spec = context.GetDesignSlotStorageSpec(slot.slot_id);
      if (!HasFourStateContent(spec, arena)) continue;
      if (IsPatchTableEligible(spec)) continue;
      uint64_t offset = context.GetDesignSlotByteOffset(slot.slot_id);
      auto* slot_ptr = builder.CreateGEP(
          i8_ty, design_state, builder.getInt64(offset), "slot_init_ptr");
      EmitSVDefaultInitAfterZero(context, slot_ptr, slot.type_id);
    }
  }

  // Step 4: Construct owned storage handles.
  EmitConstructOwnedHandles(context, design_state, slots, design_layout);

  // Step 5: Initialize owned backing storage (4-state X-encoding).
  EmitInitializeOwnedBackingStorage(
      context, design_state, slots, design_layout);
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

void ReleaseStringSlots(
    Context& context, const std::vector<SlotInfo>& slots,
    llvm::Value* design_state) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  for (const auto& slot : slots) {
    if (slot.type_info.kind != lowering::mir_to_llvm::VarTypeKind::kString) {
      continue;
    }

    // Handle slots (string, dynarray, queue) store a canonical pointer-width
    // value. Direct typed pointer load is correct for handle storage.
    uint64_t offset = context.GetDesignSlotByteOffset(slot.slot_id);
    auto* slot_ptr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(offset), "str_release_ptr");
    auto* val = builder.CreateLoad(ptr_ty, slot_ptr);
    builder.CreateCall(context.GetLyraStringRelease(), {val});
  }
}

void EmitParamInitStores(
    Context& context, llvm::Value* design_state,
    const RealizationData& realization) {
  if (realization.param_inits.empty()) return;

  auto& builder = context.GetBuilder();
  auto* i8_ty = llvm::Type::getInt8Ty(context.GetLlvmContext());

  for (const auto& entries : realization.param_inits) {
    for (const auto& entry : entries) {
      auto slot_id = mir::SlotId{entry.slot_id};
      uint64_t offset = context.GetDesignSlotByteOffset(slot_id);

      auto* slot_ptr = builder.CreateGEP(
          i8_ty, design_state, builder.getInt64(offset), "param_ptr");

      Constant constant{.type = entry.type_id, .value = entry.value};
      auto value_result = LowerConstant(context, constant);
      if (!value_result) {
        throw common::InternalError(
            "EmitParamInitStores", "failed to materialize param constant");
      }
      const auto& spec = context.GetDesignSlotStorageSpec(slot_id);
      const auto& arena = context.GetDesignStorageSpecArena();

      // Lower packed values from semantic width to storage lane width
      // before passing to the storage boundary.
      llvm::Value* lowered = *value_result;
      if (const auto* packed = std::get_if<PackedStorageSpec>(&spec.data)) {
        lowered = LowerToStorageLaneWidth(builder, lowered, *packed);
      }
      EmitStoreToCanonicalStorage(builder, slot_ptr, lowered, spec, arena);
    }
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

void EmitDesignStateInit(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slot_info, const Layout& layout,
    const RealizationData& realization) {
  InitializeDesignState(context, design_state, slot_info, layout.design);
  EmitParamInitStores(context, design_state, realization);
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

  const auto& offsets = layout.design.slot_byte_offsets;
  auto count = static_cast<uint32_t>(offsets.size());
  if (count == 0) {
    return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(ctx));
  }

  std::vector<llvm::Constant*> entries;
  entries.reserve(count);
  for (uint64_t offset : offsets) {
    entries.push_back(llvm::ConstantInt::get(i64_ty, offset));
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

// Emitted body descriptor package: descriptor header, process entries,
// and metadata template. One per body.
struct BodyDescriptorPackageEmission {
  llvm::Constant* header_ptr;
  llvm::Constant* entries_ptr;
  uint32_t num_processes;
  MetaTemplateEmission meta;
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

// Emit per-body realization descriptors: one BodyRealizationDesc header,
// one BodyProcessEntry array, and one metadata template per body. Built
// from body-shaped sources (layout body_realization_infos + body compiled
// functions), not from per-instance artifacts.
auto EmitBodyRealizationDescs(
    Context& context, const Layout& layout,
    std::span<const lowering::mir_to_llvm::CodegenSession::BodyCompiledFuncs>
        body_compiled_funcs) -> std::vector<BodyDescriptorPackageEmission> {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  if (body_compiled_funcs.size() != layout.body_realization_infos.size()) {
    throw common::InternalError(
        "EmitBodyRealizationDescs",
        std::format(
            "body_compiled_funcs count {} != body_realization_infos count {}",
            body_compiled_funcs.size(), layout.body_realization_infos.size()));
  }

  // BodyRealizationDesc ABI: {u32 num_processes, u32 slot_count}
  auto* header_type = llvm::StructType::get(ctx, {i32_ty, i32_ty});
  // BodyProcessEntry ABI: {ptr shared_body_fn, u32 schema_index, u32 pad}
  auto* entry_type = llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});

  std::vector<BodyDescriptorPackageEmission> result;
  result.reserve(layout.body_realization_infos.size());

  for (size_t bi = 0; bi < layout.body_realization_infos.size(); ++bi) {
    const auto& info = layout.body_realization_infos[bi];
    const auto& funcs = body_compiled_funcs[bi];
    uint32_t body_id_val = info.body_id.value;
    uint32_t num_procs =
        static_cast<uint32_t>(info.process_schema_indices.size());

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
        header_type, {llvm::ConstantInt::get(i32_ty, num_procs),
                      llvm::ConstantInt::get(i32_ty, info.slot_count)});
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
    uint32_t num_meta = static_cast<uint32_t>(info.meta.entries.size());
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

  const auto& conn_meta = layout.connection_meta;
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

// Build compile-time design metadata globals.
// Process metadata is NOT built here -- it is constructor-produced.
// realization.instance_paths is still used here for trace signal metadata
// and %m runtime paths; that ownership moves to the constructor in a
// later migration step.
auto BuildDesignMetadata(
    Context& context, const RealizationData& realization, const Layout& layout,
    const std::vector<SlotInfo>& slot_info,
    const std::vector<ProcessTriggerEntry>& process_triggers,
    const EmitDesignMainInput& input, size_t num_init) -> MetadataGlobals {
  auto& builder = context.GetBuilder();
  const auto& type_arena = context.GetTypeArena();

  auto slot_meta_inputs = ExtractSlotMetaInputs(slot_info, layout.design);
  auto conn_desc_entries = ExtractConnectionDescriptorEntries(layout);
  auto comb_inputs = PrepareCombKernelInputs(layout, num_init);
  auto back_edge_site_inputs =
      PrepareBackEdgeSiteInputs(context, input.diag_ctx, input.source_manager);
  auto slot_count = static_cast<uint32_t>(slot_meta_inputs.size());
  auto process_trigger_inputs =
      BuildProcessTriggerInputs(process_triggers, slot_count);

  auto trace_signal_inputs = PrepareTraceSignalMetaInputs(
      realization.slot_trace_provenance, realization.slot_trace_string_pool,
      realization.slot_types, realization.slot_kinds,
      realization.instance_paths, type_arena);

  // Port-binding forwarding candidate analysis (analysis only, no transform).
  // TODO(hankhsu): Gate behind verbose/debug flag when one is available
  // in the emit_design_main input. Currently always runs (cheap) but
  // logging is conditional on candidates existing.
  auto forwarding_candidates = FindPortBindingForwardingCandidates(
      conn_desc_entries, process_trigger_inputs, comb_inputs, slot_count);
  LogPortBindingForwardingCandidates(forwarding_candidates);

  metadata::DesignMetadataInputs metadata_inputs{
      .slot_meta = std::move(slot_meta_inputs),
      .back_edge_sites = std::move(back_edge_site_inputs),
      .connection_descriptors = std::move(conn_desc_entries),
      .comb_kernels = std::move(comb_inputs),
      .process_triggers = std::move(process_trigger_inputs),
      .instance_paths = realization.instance_paths,
      .trace_signal_meta = std::move(trace_signal_inputs),
  };
  auto metadata = realization::BuildDesignMetadata(metadata_inputs);

  return EmitDesignMetadataGlobals(context, metadata, builder);
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

struct ConstructorEmissionResult {
  llvm::Value* states_array = nullptr;
  llvm::Value* num_total = nullptr;
  llvm::Value* result_handle = nullptr;
  llvm::Function* destroy_fn = nullptr;
  ConstructorProcessMeta process_meta;
};

auto BuildRuntimeAbi(
    Context& context, const MetadataGlobals& meta_globals,
    const WaitSiteMetaResult& wait_site_meta, uint32_t num_connection,
    uint32_t feature_flags, const std::string& signal_trace_path,
    llvm::Value* design_state, const ConstructorProcessMeta& process_meta)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  // v13: process metadata from constructor result.
  constexpr unsigned kAbiFieldCount = 27;
  std::array<llvm::Type*, kAbiFieldCount> abi_fields = {
      i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
      ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty,
      ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, ptr_ty, i32_ty, i32_ty, ptr_ty,
  };
  auto* abi_struct_type = llvm::StructType::get(ctx, abi_fields, false);

  auto* abi_alloca = builder.CreateAlloca(abi_struct_type, nullptr, "abi");

  auto store_field = [&](unsigned idx, llvm::Value* val) {
    auto* gep = builder.CreateStructGEP(abi_struct_type, abi_alloca, idx);
    builder.CreateStore(val, gep);
  };

  store_field(0, llvm::ConstantInt::get(i32_ty, kRuntimeAbiVersion));
  store_field(1, meta_globals.slot_meta_words);
  store_field(2, llvm::ConstantInt::get(i32_ty, meta_globals.slot_meta_count));
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
  store_field(13, meta_globals.comb_kernel_words);
  store_field(
      14, llvm::ConstantInt::get(i32_ty, meta_globals.comb_kernel_word_count));
  store_field(15, llvm::ConstantInt::get(i32_ty, feature_flags));
  store_field(16, wait_site_meta.words_ptr);
  store_field(17, llvm::ConstantInt::get(i32_ty, wait_site_meta.count));
  store_field(18, meta_globals.trace_signal_meta_words);
  store_field(
      19, llvm::ConstantInt::get(
              i32_ty, meta_globals.trace_signal_meta_word_count));
  store_field(20, meta_globals.trace_signal_meta_pool);
  store_field(
      21,
      llvm::ConstantInt::get(i32_ty, meta_globals.trace_signal_meta_pool_size));

  if (signal_trace_path.empty()) {
    store_field(22, null_ptr);
  } else {
    store_field(
        22,
        builder.CreateGlobalStringPtr(signal_trace_path, "signal_trace_path"));
  }

  store_field(23, meta_globals.process_trigger_words);
  store_field(
      24,
      llvm::ConstantInt::get(i32_ty, meta_globals.process_trigger_word_count));

  // Dispatch partition boundary (connection/module process split).
  // Currently computed from compile-time layout topology; will move
  // to constructor result extraction when remaining dispatch partition
  // ownership migrates.
  store_field(25, llvm::ConstantInt::get(i32_ty, num_connection));

  // Design-state binding.
  store_field(26, design_state);

  return abi_alloca;
}

void EmitRunSimulation(
    Context& context, llvm::Constant* funcs_array, llvm::Value* states_array,
    llvm::Value* num_processes, const PlusargsSetup& plusargs,
    const MetadataGlobals& meta_globals, llvm::Value* abi_alloca) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  // instance_paths_array is a legacy compile-time per-instance global
  // used by %m and trace signal metadata. Ownership moves to the
  // constructor in a later migration step; until then this is the
  // only remaining compile-time instance path consumer.
  builder.CreateCall(
      context.GetLyraRunSimulation(),
      {funcs_array, states_array, num_processes, plusargs.array, plusargs.count,
       meta_globals.instance_paths_array,
       llvm::ConstantInt::get(i32_ty, meta_globals.instance_path_count),
       abi_alloca});
}

void EmitMainExit(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slot_info, const EmitDesignMainInput& input,
    llvm::BasicBlock* exit_block) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();

  builder.CreateBr(exit_block);
  builder.SetInsertPoint(exit_block);

  ReleaseStringSlots(context, slot_info, design_state);

  if (input.hooks != nullptr) {
    input.hooks->OnAfterRunSimulation(context, slot_info, design_state);
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
  };
}

// Runtime constructor C ABI function declarations for LLVM IR emission.
struct ConstructorRuntimeFuncs {
  llvm::Function* create;
  llvm::Function* add_connection;
  llvm::Function* begin_body;
  llvm::Function* add_instance;
  llvm::Function* finalize;
  llvm::Function* result_get_states;
  llvm::Function* result_get_num_total;
  llvm::Function* result_get_meta_words;
  llvm::Function* result_get_meta_word_count;
  llvm::Function* result_get_meta_pool;
  llvm::Function* result_get_meta_pool_size;
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
          {ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i64_ty, ptr_ty,
           i32_ty, ptr_ty, i32_ty}),
      .add_connection =
          declare("LyraConstructorAddConnection", void_ty, {ptr_ty, ptr_ty}),
      .begin_body = declare(
          "LyraConstructorBeginBody", void_ty,
          {ptr_ty, ptr_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty}),
      .add_instance =
          declare("LyraConstructorAddInstance", void_ty, {ptr_ty, ptr_ty}),
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
      .result_destroy =
          declare("LyraConstructionResultDestroy", void_ty, {ptr_ty}),
  };
}

// Emit one LyraConstructorBeginBody call from a BodyDescriptorPackageEmission.
// Centralizes the decomposition of the package into C ABI arguments so the
// orchestration loop does not manually unpack fields.
void EmitBeginBodyCall(
    llvm::IRBuilder<>& builder, const ConstructorRuntimeFuncs& rt,
    llvm::Value* ctor, const BodyDescriptorPackageEmission& pkg,
    llvm::Type* i32_ty) {
  builder.CreateCall(
      rt.begin_body,
      {ctor, pkg.header_ptr, pkg.entries_ptr,
       llvm::ConstantInt::get(i32_ty, pkg.num_processes), pkg.meta.entries_ptr,
       llvm::ConstantInt::get(i32_ty, pkg.meta.num_entries), pkg.meta.pool_ptr,
       llvm::ConstantInt::get(i32_ty, pkg.meta.pool_size)});
}

// Emit the constructor function: LLVM IR that calls RuntimeConstructor C API
// to build process construction order and bindings for the current design
// topology summary. The emitted program walks instances in ModuleIndex order
// (instance-major), switching the active body via BeginBody as needed. This
// preserves instance-major process ordering for metadata table alignment.
// The compile-owned topology summary (instance_body_group) drives the
// emitted call sequence.

auto EmitConstructorFunction(
    Context& context, const Layout& layout,
    const lowering::mir_to_llvm::CodegenSession& session,
    llvm::Value* design_state, llvm::Constant* schemas_ptr,
    uint32_t num_schemas, llvm::Constant* slot_byte_offsets_ptr,
    const std::vector<BodyDescriptorPackageEmission>& body_descs,
    llvm::Constant* conn_descs_ptr, const MetaTemplateEmission& conn_meta,
    const std::vector<std::string>& instance_paths)
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
      {schemas_ptr, llvm::ConstantInt::get(i32_ty, num_schemas),
       slot_byte_offsets_ptr, llvm::ConstantInt::get(i32_ty, num_slots),
       llvm::ConstantInt::get(i32_ty, layout.num_package_slots), design_state,
       llvm::ConstantInt::get(i64_ty, arena_size), conn_meta.entries_ptr,
       llvm::ConstantInt::get(i32_ty, conn_meta.num_entries),
       conn_meta.pool_ptr, llvm::ConstantInt::get(i32_ty, conn_meta.pool_size)},
      "ctor");

  // Add connection processes.
  const auto& conn_infos = layout.connection_realization_infos;
  for (uint32_t ci = 0; ci < conn_infos.size(); ++ci) {
    // GEP into the connection descriptor array.
    auto* conn_entry_type =
        llvm::StructType::get(ctx, {ptr_ty, i32_ty, i32_ty});
    auto* desc_ptr = builder.CreateConstInBoundsGEP1_32(
        conn_entry_type, conn_descs_ptr, ci, std::format("conn_desc_{}", ci));
    builder.CreateCall(rt.add_connection, {ctor, desc_ptr});
  }

  // Validate topology source-of-truth parallelism before emission.
  // These structures must be parallel: body_realization_infos,
  // body_compiled_funcs, and body_descs all indexed by body group.
  const auto& ibg = session.instance_body_group;
  auto num_body_groups = static_cast<uint32_t>(body_descs.size());

  if (session.body_compiled_funcs.size() != num_body_groups) {
    throw common::InternalError(
        "EmitConstructorFunction",
        std::format(
            "body_compiled_funcs size {} != body_descs size {}",
            session.body_compiled_funcs.size(), num_body_groups));
  }
  if (layout.body_realization_infos.size() != num_body_groups) {
    throw common::InternalError(
        "EmitConstructorFunction",
        std::format(
            "body_realization_infos size {} != body_descs size {}",
            layout.body_realization_infos.size(), num_body_groups));
  }
  for (uint32_t gi = 0; gi < num_body_groups; ++gi) {
    if (session.body_compiled_funcs[gi].body_id !=
        layout.body_realization_infos[gi].body_id) {
      throw common::InternalError(
          "EmitConstructorFunction",
          std::format(
              "body group {} body_id mismatch: compiled={} vs info={}", gi,
              session.body_compiled_funcs[gi].body_id.value,
              layout.body_realization_infos[gi].body_id.value));
    }
  }

  // Validate instance_paths count matches instance count.
  if (instance_paths.size() != ibg.size()) {
    throw common::InternalError(
        "EmitConstructorFunction",
        std::format(
            "instance_paths size {} != instance_body_group size {}",
            instance_paths.size(), ibg.size()));
  }

  // Validate instance_body_group entries are in range.
  for (uint32_t mi = 0; mi < ibg.size(); ++mi) {
    if (ibg[mi] >= num_body_groups) {
      throw common::InternalError(
          "EmitConstructorFunction",
          std::format(
              "instance {} body group {} >= num_body_groups {}", mi, ibg[mi],
              num_body_groups));
    }
  }

  // Instance-major constructor program: walk instances in ModuleIndex
  // order, switching the active body via BeginBody when the body group
  // changes. The same body may appear multiple times if instances of
  // different bodies are interleaved in ModuleIndex order. This is the
  // Instance-major ordering preserves metadata table alignment.
  uint32_t last_body_group = UINT32_MAX;
  for (uint32_t mi = 0; mi < ibg.size(); ++mi) {
    uint32_t bg = ibg[mi];
    if (bg != last_body_group) {
      EmitBeginBodyCall(builder, rt, ctor, body_descs[bg], i32_ty);
      last_body_group = bg;
    }
    // Pass instance path string literal to AddInstance.
    auto* path_str = builder.CreateGlobalStringPtr(
        instance_paths[mi], std::format("inst_path_{}", mi));
    builder.CreateCall(rt.add_instance, {ctor, path_str});
  }

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
  };
}

auto EmitDesignMain(
    lowering::mir_to_llvm::CodegenSession& session,
    const EmitDesignMainInput& input) -> Result<void> {
  auto& context = *session.context;
  const auto& layout = *session.layout;
  const auto& slot_info = session.slot_info;
  const auto& process_funcs = session.process_funcs;
  size_t num_init = session.num_init_processes;
  const auto& realization = session.realization;

  auto [main_func, exit_block, design_state] =
      CreateMainFunction(context, input.main_abi);

  EmitDesignStateInit(context, design_state, slot_info, layout, realization);

  EmitRuntimeInit(context, main_func, input);

  if (input.hooks != nullptr) {
    input.hooks->OnAfterInitializeDesignState(context, slot_info, design_state);
  }

  EmitInitProcesses(context, design_state, layout, process_funcs, num_init);

  if (input.hooks != nullptr) {
    input.hooks->OnBeforeRunSimulation(context, slot_info, design_state);
  }

  auto num_simulation = static_cast<uint32_t>(process_funcs.size() - num_init);
  auto num_kernelized =
      static_cast<uint32_t>(layout.connection_kernel_entries.size());

  if (num_simulation > 0 || num_kernelized > 0) {
    auto& builder = context.GetBuilder();
    auto& ctx = context.GetLlvmContext();
    auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
    auto* i32_ty = llvm::Type::getInt32Ty(ctx);
    auto* null_ptr =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

    // Default to null for zero-simulation case.
    llvm::Value* states_array = null_ptr;
    llvm::Constant* connection_funcs = null_ptr;
    llvm::Value* num_total_val = llvm::ConstantInt::get(i32_ty, 0);
    ConstructorEmissionResult ctor_result{};
    uint32_t num_connection = static_cast<uint32_t>(
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

      // Emit the constructor function: LLVM IR that calls the runtime
      // constructor to build process construction order and bindings.
      ctor_result = EmitConstructorFunction(
          context, layout, session, design_state, schemas_ptr, num_schemas,
          slot_offsets_ptr, body_descs, conn_descs_ptr, conn_meta_emission,
          realization.instance_paths);

      states_array = ctor_result.states_array;
      num_total_val = ctor_result.num_total;
    }

    auto plusargs = BuildPlusargs(context, main_func, input);

    auto meta_globals = BuildDesignMetadata(
        context, realization, layout, slot_info, session.process_triggers,
        input, num_init);

    auto wait_site_meta = EmitWaitSiteMetaTable(context, session.wait_sites);

    // Fallback: zero process metadata for the no-constructor topology case
    // (num_simulation == 0 but num_kernelized > 0). When the constructor is
    // active, constructor-produced metadata replaces this.
    ConstructorProcessMeta process_meta_for_abi{
        .words = null_ptr,
        .word_count = llvm::ConstantInt::get(i32_ty, 0),
        .pool = null_ptr,
        .pool_size = llvm::ConstantInt::get(i32_ty, 0),
    };
    if (ctor_result.result_handle != nullptr) {
      process_meta_for_abi = ctor_result.process_meta;
    }

    auto* abi_alloca = BuildRuntimeAbi(
        context, meta_globals, wait_site_meta, num_connection,
        input.feature_flags, input.signal_trace_path, design_state,
        process_meta_for_abi);

    EmitRunSimulation(
        context, connection_funcs, states_array, num_total_val, plusargs,
        meta_globals, abi_alloca);

    // Destroy the constructor-owned result after simulation completes.
    if (ctor_result.result_handle != nullptr) {
      builder.CreateCall(ctor_result.destroy_fn, {ctor_result.result_handle});
    }
  }

  EmitMainExit(context, design_state, slot_info, input, exit_block);

  return {};
}

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult> {
  auto session_result = CompileDesignProcesses(input);
  if (!session_result) return std::unexpected(session_result.error());

  auto emit_result =
      EmitDesignMain(*session_result, BuildEmitDesignMainInput(input));
  if (!emit_result) return std::unexpected(emit_result.error());

  return FinalizeModule(std::move(*session_result));
}

}  // namespace lyra::lowering::mir_to_llvm
