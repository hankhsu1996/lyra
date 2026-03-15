#include "lyra/llvm_backend/emit_design_main.hpp"

#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <unordered_map>
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

void InitializeDesignState(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots,
    const lowering::mir_to_llvm::FourStatePatchTable& patches) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* arena_ty = llvm::ArrayType::get(i8_ty, context.GetDesignArenaSize());

  EmitMemsetZero(context, design_state, arena_ty);

  if (context.IsForceTwoState()) return;

  EmitApply4StatePatches(context, design_state, patches, "design");

  // Non-patch-table-eligible 4-state slots need recursive default init.
  // Both queries are fully spec-driven: HasFourStateContent checks the
  // resolved spec tree for any 4-state packed content; IsPatchTableEligible
  // checks whether the patch table already handled it.
  const auto& arena = context.GetDesignStorageSpecArena();
  for (const auto& slot : slots) {
    const auto& spec = context.GetDesignSlotStorageSpec(slot.slot_id);
    if (!HasFourStateContent(spec, arena)) {
      continue;
    }
    if (IsPatchTableEligible(spec)) {
      continue;
    }
    uint64_t offset = context.GetDesignSlotByteOffset(slot.slot_id);
    auto* slot_ptr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(offset), "slot_init_ptr");
    EmitSVDefaultInitAfterZero(context, slot_ptr, slot.type_id);
  }
}

void InitializeProcessState(
    Context& context, llvm::Value* process_state, llvm::Value* design_state,
    const ProcessLayout& proc_layout, size_t process_index) {
  auto& builder = context.GetBuilder();
  auto* state_type = proc_layout.state_type;
  auto* header_type = context.GetHeaderType();
  bool force_two_state = context.IsForceTwoState();

  EmitMemsetZero(context, process_state, state_type);

  auto* header_ptr = builder.CreateStructGEP(state_type, process_state, 0);
  auto* design_ptr_ptr = builder.CreateStructGEP(header_type, header_ptr, 1);
  builder.CreateStore(design_state, design_ptr_ptr);

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
  InitializeDesignState(
      context, design_state, slot_info, layout.design.four_state_patches);
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

struct ProcessInitDesc {
  uint64_t offset;
  uint64_t size;
  size_t layout_index;
  bool has_4state_patches;
  bool has_composite_4state;
};

struct PackedProcessLayout {
  std::vector<ProcessInitDesc> descs;
  uint64_t buffer_size;
  uint64_t max_align;
  uint32_t count;
};

auto ComputePackedProcessLayout(
    const Context& context, const Layout& layout,
    const std::vector<llvm::Function*>& process_funcs, size_t num_init,
    const llvm::DataLayout& dl) -> PackedProcessLayout {
  auto count = static_cast<uint32_t>(process_funcs.size() - num_init);
  bool force_two_state = context.IsForceTwoState();

  std::vector<ProcessInitDesc> descs;
  descs.reserve(count);
  uint64_t buffer_size = 0;
  uint64_t max_align = 16;

  for (size_t i = num_init; i < process_funcs.size(); ++i) {
    const auto& proc_layout = layout.processes[i];
    auto* state_type = proc_layout.state_type;
    uint64_t size = dl.getTypeAllocSize(state_type);
    uint64_t align = dl.getABITypeAlign(state_type).value();
    max_align = std::max(max_align, align);

    buffer_size = (buffer_size + align - 1) & ~(align - 1);

    bool has_patches =
        !force_two_state && !proc_layout.frame.four_state_patches.IsEmpty();
    bool has_composite = false;
    if (!force_two_state) {
      const auto& types = context.GetTypeArena();
      for (TypeId type_id : proc_layout.frame.root_types) {
        if (context.IsFourState(type_id) &&
            !IsScalarPatchable(type_id, types, force_two_state)) {
          has_composite = true;
          break;
        }
      }
    }

    descs.push_back({
        .offset = buffer_size,
        .size = size,
        .layout_index = i,
        .has_4state_patches = has_patches,
        .has_composite_4state = has_composite,
    });
    buffer_size += size;
  }

  return {
      .descs = std::move(descs),
      .buffer_size = buffer_size,
      .max_align = max_align,
      .count = count,
  };
}

auto EmitPackedStateInit(
    Context& context, llvm::Function* main_func, llvm::Value* design_state,
    const Layout& layout, const PackedProcessLayout& packed_layout)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  const auto& dl = mod.getDataLayout();
  bool force_two_state = context.IsForceTwoState();
  uint32_t count = packed_layout.count;

  auto* states_array = builder.CreateAlloca(
      ptr_ty, llvm::ConstantInt::get(i32_ty, count), "states_array");

  if (count == 0) return states_array;

  auto* packed_states = builder.CreateAlloca(
      i8_ty, builder.getInt64(packed_layout.buffer_size), "packed_states");
  if (auto* alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(packed_states)) {
    alloca_inst->setAlignment(llvm::Align(packed_layout.max_align));
  }
  builder.CreateMemSet(
      packed_states, builder.getInt8(0), packed_layout.buffer_size,
      llvm::MaybeAlign(packed_layout.max_align));

  // Build offset table as global constant for the init loop.
  std::vector<llvm::Constant*> offset_constants;
  offset_constants.reserve(count);
  for (const auto& desc : packed_layout.descs) {
    offset_constants.push_back(llvm::ConstantInt::get(i64_ty, desc.offset));
  }
  auto* offset_array_type = llvm::ArrayType::get(i64_ty, count);
  auto* offsets_global = new llvm::GlobalVariable(
      mod, offset_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(offset_array_type, offset_constants),
      "__lyra_proc_offsets");
  offsets_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  auto* header_llvm_type = context.GetHeaderType();
  const llvm::StructLayout* hdr_sl = dl.getStructLayout(header_llvm_type);
  uint64_t design_ptr_offset_in_header = hdr_sl->getElementOffset(1);

  // Emit loop: write design_state pointer into each header and store
  // process state pointer into states_array.
  auto* proc_loop_preheader = builder.GetInsertBlock();
  auto* proc_loop_header =
      llvm::BasicBlock::Create(ctx, "proc_init_header", main_func);
  auto* proc_loop_body =
      llvm::BasicBlock::Create(ctx, "proc_init_body", main_func);
  auto* proc_loop_exit =
      llvm::BasicBlock::Create(ctx, "proc_init_exit", main_func);

  builder.CreateBr(proc_loop_header);
  builder.SetInsertPoint(proc_loop_header);

  auto* proc_phi = builder.CreatePHI(i32_ty, 2, "proc_idx");
  proc_phi->addIncoming(builder.getInt32(0), proc_loop_preheader);
  auto* proc_cond = builder.CreateICmpULT(proc_phi, builder.getInt32(count));
  builder.CreateCondBr(proc_cond, proc_loop_body, proc_loop_exit);

  builder.SetInsertPoint(proc_loop_body);

  auto* offset_ptr = builder.CreateGEP(
      offset_array_type, offsets_global, {builder.getInt32(0), proc_phi});
  auto* offset_val = builder.CreateLoad(i64_ty, offset_ptr);
  auto* state_ptr = builder.CreateGEP(i8_ty, packed_states, {offset_val});
  auto* design_ptr_loc = builder.CreateGEP(
      i8_ty, state_ptr, {builder.getInt64(design_ptr_offset_in_header)});
  builder.CreateStore(design_state, design_ptr_loc);
  auto* state_slot = builder.CreateGEP(ptr_ty, states_array, {proc_phi});
  builder.CreateStore(state_ptr, state_slot);

  auto* proc_next = builder.CreateAdd(proc_phi, builder.getInt32(1));
  proc_phi->addIncoming(proc_next, proc_loop_body);
  builder.CreateBr(proc_loop_header);

  builder.SetInsertPoint(proc_loop_exit);

  // Apply 4-state patches for processes that need them (rare).
  for (uint32_t pi = 0; pi < count; ++pi) {
    const auto& desc = packed_layout.descs[pi];
    if (!desc.has_4state_patches && !desc.has_composite_4state) continue;

    const auto& proc_layout = layout.processes[desc.layout_index];
    auto* state_ptr_4s = builder.CreateGEP(
        i8_ty, packed_states, {builder.getInt64(desc.offset)});
    auto* state_type = proc_layout.state_type;

    if (desc.has_4state_patches) {
      auto* frame_ptr = builder.CreateStructGEP(state_type, state_ptr_4s, 1);
      std::string prefix = std::format("frame.{}", desc.layout_index);
      EmitApply4StatePatches(
          context, frame_ptr, proc_layout.frame.four_state_patches, prefix);
    }

    if (desc.has_composite_4state) {
      auto* frame_type = proc_layout.frame.llvm_type;
      auto* frame_ptr = builder.CreateStructGEP(state_type, state_ptr_4s, 1);
      const auto& types = context.GetTypeArena();
      for (uint32_t fi = 0; fi < proc_layout.frame.root_types.size(); ++fi) {
        TypeId type_id = proc_layout.frame.root_types[fi];
        if (!context.IsFourState(type_id)) continue;
        if (IsScalarPatchable(type_id, types, force_two_state)) continue;
        auto* field_ptr = builder.CreateStructGEP(frame_type, frame_ptr, fi);
        EmitSVDefaultInitAfterZero(context, field_ptr, type_id);
      }
    }
  }

  return states_array;
}

auto EmitProcessFuncArray(
    Context& context, const std::vector<llvm::Function*>& process_funcs,
    size_t num_init, uint32_t num_regular) -> llvm::Constant* {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  std::vector<llvm::Constant*> func_constants;
  func_constants.reserve(num_regular);
  for (size_t i = num_init; i < process_funcs.size(); ++i) {
    func_constants.push_back(process_funcs[i]);
  }

  auto* func_array_type = llvm::ArrayType::get(ptr_ty, num_regular);
  auto* funcs_global = new llvm::GlobalVariable(
      mod, func_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(func_array_type, func_constants),
      "__lyra_module_funcs");
  funcs_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getInBoundsGetElementPtr(
      func_array_type, funcs_global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});
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

auto BuildDesignMetadata(
    Context& context, const RealizationData& realization, const Layout& layout,
    const std::vector<SlotInfo>& slot_info, const EmitDesignMainInput& input,
    size_t num_init) -> MetadataGlobals {
  auto& builder = context.GetBuilder();
  const auto& design_arena = *input.design_arena;
  const auto& type_arena = context.GetTypeArena();

  auto slot_meta_inputs = ExtractSlotMetaInputs(slot_info, layout.design);
  auto conn_desc_entries =
      ExtractConnectionDescriptorEntries(design_arena, layout);
  auto scheduled_inputs = PrepareScheduledProcessInputs(
      realization.instance_paths, *input.design, design_arena, input.diag_ctx,
      input.source_manager, layout.scheduled_processes, num_init);
  auto comb_inputs = PrepareCombKernelInputs(design_arena, layout, num_init);
  auto back_edge_site_inputs =
      PrepareBackEdgeSiteInputs(context, input.diag_ctx, input.source_manager);

  auto trace_signal_inputs = PrepareTraceSignalMetaInputs(
      realization.slot_trace_provenance, realization.slot_trace_string_pool,
      realization.slot_types, realization.slot_kinds,
      realization.instance_paths, type_arena);

  realization::DesignMetadataInputs metadata_inputs{
      .slot_meta = std::move(slot_meta_inputs),
      .scheduled_processes = std::move(scheduled_inputs),
      .back_edge_sites = std::move(back_edge_site_inputs),
      .connection_descriptors = std::move(conn_desc_entries),
      .comb_kernels = std::move(comb_inputs),
      .instance_paths = realization.instance_paths,
      .trace_signal_meta = std::move(trace_signal_inputs),
  };
  auto metadata = realization::BuildDesignMetadata(metadata_inputs);

  return EmitDesignMetadataGlobals(context, metadata, builder);
}

struct CombWrappersResult {
  llvm::Value* funcs_ptr;
  uint32_t count;
};

auto EmitCombWrappers(
    Context& context, const Layout& layout,
    const std::vector<llvm::Function*>& process_funcs, size_t num_init,
    uint32_t num_module_processes) -> CombWrappersResult {
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));

  auto num_comb = static_cast<uint32_t>(layout.comb_kernel_entries.size());
  if (num_comb == 0) {
    return {.funcs_ptr = null_ptr, .count = 0};
  }

  // Build ProcessId -> module process index mapping.
  std::unordered_map<uint32_t, uint32_t> proc_id_to_module_idx;
  for (uint32_t pi = 0; pi < num_module_processes; ++pi) {
    auto proc_id = layout.scheduled_processes[num_init + pi].process_id;
    proc_id_to_module_idx[proc_id.value] = pi;
  }

  // Process functions use pointer-out ABI: void(ptr, i32, ptr %out).
  // Comb runtime ABI is void(ptr, i32) -- no outcome pointer.
  // Each wrapper allocates a local outcome buffer, calls the process
  // function with &outcome, and discards the result.
  auto* void_ty = llvm::Type::getVoidTy(ctx);
  auto* comb_fn_ty = llvm::FunctionType::get(void_ty, {ptr_ty, i32_ty}, false);
  auto* outcome_mem_ty =
      llvm::StructType::get(ctx, {i32_ty, i32_ty, i32_ty, i32_ty});
  auto* proc_fn_ty =
      llvm::FunctionType::get(void_ty, {ptr_ty, i32_ty, ptr_ty}, false);

  std::vector<llvm::Constant*> comb_func_constants;
  comb_func_constants.reserve(num_comb);
  for (uint32_t ki = 0; ki < num_comb; ++ki) {
    const auto& ck = layout.comb_kernel_entries[ki];
    auto it = proc_id_to_module_idx.find(ck.process_id.value);
    uint32_t module_idx = it->second;
    auto* proc_fn = process_funcs[num_init + module_idx];

    auto* wrapper = llvm::Function::Create(
        comb_fn_ty, llvm::Function::InternalLinkage,
        std::format("__lyra_comb_wrapper_{}", ki), mod);
    auto* entry = llvm::BasicBlock::Create(ctx, "entry", wrapper);
    llvm::IRBuilder<> wb(entry);
    auto* outcome_buf = wb.CreateAlloca(outcome_mem_ty, nullptr, "outcome");
    wb.CreateCall(
        proc_fn_ty, proc_fn,
        {wrapper->getArg(0), wrapper->getArg(1), outcome_buf});
    wb.CreateRetVoid();

    comb_func_constants.push_back(wrapper);
  }

  auto* comb_func_array_type = llvm::ArrayType::get(ptr_ty, num_comb);
  auto* comb_funcs_global = new llvm::GlobalVariable(
      mod, comb_func_array_type, true, llvm::GlobalValue::InternalLinkage,
      llvm::ConstantArray::get(comb_func_array_type, comb_func_constants),
      "__lyra_comb_funcs");
  comb_funcs_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  auto* comb_funcs_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      comb_func_array_type, comb_funcs_global,
      llvm::ArrayRef<llvm::Constant*>{
          llvm::ConstantInt::get(i32_ty, 0),
          llvm::ConstantInt::get(i32_ty, 0)});

  return {.funcs_ptr = comb_funcs_ptr, .count = num_comb};
}

auto BuildRuntimeAbi(
    Context& context, const MetadataGlobals& meta_globals,
    const WaitSiteMetaResult& wait_site_meta,
    const CombWrappersResult& comb_wrappers, uint32_t feature_flags,
    const std::string& signal_trace_path) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);

  constexpr unsigned kAbiFieldCount = 25;
  std::array<llvm::Type*, kAbiFieldCount> abi_fields = {
      i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
      ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty,
      ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty,
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
  store_field(3, meta_globals.process_meta_words);
  store_field(
      4, llvm::ConstantInt::get(i32_ty, meta_globals.process_meta_count));
  store_field(5, meta_globals.process_meta_pool);
  store_field(
      6, llvm::ConstantInt::get(i32_ty, meta_globals.process_meta_pool_size));
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
  store_field(18, comb_wrappers.funcs_ptr);
  store_field(19, llvm::ConstantInt::get(i32_ty, comb_wrappers.count));
  store_field(20, meta_globals.trace_signal_meta_words);
  store_field(
      21, llvm::ConstantInt::get(
              i32_ty, meta_globals.trace_signal_meta_word_count));
  store_field(22, meta_globals.trace_signal_meta_pool);
  store_field(
      23,
      llvm::ConstantInt::get(i32_ty, meta_globals.trace_signal_meta_pool_size));

  if (signal_trace_path.empty()) {
    store_field(
        24,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty)));
  } else {
    store_field(
        24,
        builder.CreateGlobalStringPtr(signal_trace_path, "signal_trace_path"));
  }

  return abi_alloca;
}

void EmitRunSimulation(
    Context& context, llvm::Constant* funcs_array, llvm::Value* states_array,
    uint32_t num_module_processes, const PlusargsSetup& plusargs,
    const MetadataGlobals& meta_globals, llvm::Value* abi_alloca) {
  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  builder.CreateCall(
      context.GetLyraRunSimulation(),
      {funcs_array, states_array,
       llvm::ConstantInt::get(i32_ty, num_module_processes), plusargs.array,
       plusargs.count, meta_globals.instance_paths_array,
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

  auto num_regular_module =
      static_cast<uint32_t>(process_funcs.size() - num_init);
  auto num_kernelized =
      static_cast<uint32_t>(layout.connection_kernel_entries.size());

  if (num_regular_module > 0 || num_kernelized > 0) {
    const auto& dl = context.GetModule().getDataLayout();
    auto packed_layout = ComputePackedProcessLayout(
        context, layout, process_funcs, num_init, dl);

    auto* states_array = EmitPackedStateInit(
        context, main_func, design_state, layout, packed_layout);

    auto* funcs_array = EmitProcessFuncArray(
        context, process_funcs, num_init, packed_layout.count);

    auto plusargs = BuildPlusargs(context, main_func, input);

    auto meta_globals = BuildDesignMetadata(
        context, realization, layout, slot_info, input, num_init);

    auto wait_site_meta = EmitWaitSiteMetaTable(context, session.wait_sites);

    auto comb_wrappers = EmitCombWrappers(
        context, layout, process_funcs, num_init, packed_layout.count);

    auto* abi_alloca = BuildRuntimeAbi(
        context, meta_globals, wait_site_meta, comb_wrappers,
        input.feature_flags, input.signal_trace_path);

    EmitRunSimulation(
        context, funcs_array, states_array, packed_layout.count, plusargs,
        meta_globals, abi_alloca);
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
