#include "lyra/llvm_backend/lower.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <mutex>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/four_state_init.hpp"
#include "lyra/llvm_backend/type_query.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/slot_meta_abi.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Initialize LLVM native targets (thread-safe, once per process).
void InitializeLlvmTargets() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

// Set module's DataLayout and TargetTriple to match the host.
// This MUST be called before any IR generation that uses DataLayout
// (e.g., getTypeAllocSize for memset).
//
// Uses host CPU and features to ensure DataLayout matches what ORC JIT expects.
void SetHostDataLayout(llvm::Module& module) {
  InitializeLlvmTargets();

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName().str();

  // Collect host CPU features
  llvm::StringMap<bool> feature_map;
  llvm::sys::getHostCPUFeatures(feature_map);
  llvm::SubtargetFeatures subtarget_features;
  for (const auto& kv : feature_map) {
    if (kv.getValue()) {
      subtarget_features.AddFeature(kv.getKey().str());
    }
  }
  std::string features = subtarget_features.getString();

  std::string error;
  const llvm::Target* target =
      llvm::TargetRegistry::lookupTarget(triple, error);
  if (target == nullptr) {
    throw common::InternalError(
        "SetHostDataLayout",
        std::format("failed to lookup target for '{}': {}", triple, error));
  }

  std::unique_ptr<llvm::TargetMachine> tm(target->createTargetMachine(
      triple, cpu, features, llvm::TargetOptions(), std::nullopt));
  if (tm == nullptr) {
    throw common::InternalError(
        "SetHostDataLayout",
        std::format("failed to create TargetMachine for '{}'", triple));
  }

  module.setTargetTriple(triple);
  module.setDataLayout(tm->createDataLayout());
}

// Classify a design slot's storage kind from its TypeId.
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

// Build the slot metadata global constant table.
// Returns {constant_ptr, slot_count}. If no slots, returns {null, 0}.
struct SlotMetaTableResult {
  llvm::Constant* table_ptr;
  uint32_t count;
};

auto EmitSlotMetaTable(
    Context& context, const std::vector<SlotInfo>& slots,
    const DesignLayout& design_layout, const llvm::DataLayout& dl,
    const TypeArena& types) -> SlotMetaTableResult {
  if (slots.empty()) {
    auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
    return {
        .table_ptr = llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptr_ty)),
        .count = 0};
  }

  auto* design_struct = design_layout.llvm_type;
  const llvm::StructLayout* sl = dl.getStructLayout(design_struct);
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  std::vector<llvm::Constant*> words;
  words.reserve(slots.size() * runtime::slot_meta_abi::kStride);

  for (const auto& slot : slots) {
    auto it = design_layout.slot_to_field.find(slot.slot_id);
    if (it == design_layout.slot_to_field.end()) {
      throw common::InternalError(
          "EmitSlotMetaTable",
          std::format("slot_id {} not in design layout", slot.slot_id.value));
    }
    uint32_t field_idx = it->second;
    auto* field_type = design_struct->getElementType(field_idx);

    uint64_t raw_base_off = sl->getElementOffset(field_idx);
    uint64_t raw_total_bytes = dl.getTypeAllocSize(field_type);
    if (raw_base_off > UINT32_MAX || raw_total_bytes > UINT32_MAX) {
      throw common::InternalError(
          "EmitSlotMetaTable", "DesignState exceeds 4GB (not supported)");
    }
    auto base_off = static_cast<uint32_t>(raw_base_off);
    auto total_bytes = static_cast<uint32_t>(raw_total_bytes);

    auto kind =
        ClassifySlotStorageKind(slot.type_id, types, context.IsForceTwoState());
    auto kind_val = static_cast<uint32_t>(kind);

    uint32_t value_off = 0;
    uint32_t value_bytes = 0;
    uint32_t unk_off = 0;
    uint32_t unk_bytes = 0;

    if (kind == runtime::SlotStorageKind::kPacked4) {
      auto* four_state = llvm::dyn_cast<llvm::StructType>(field_type);
      if (four_state == nullptr) {
        throw common::InternalError(
            "EmitSlotMetaTable", std::format(
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
            "EmitSlotMetaTable",
            "4-state plane layout exceeds 4GB (not supported)");
      }
      value_off = static_cast<uint32_t>(raw_value_off);
      value_bytes = static_cast<uint32_t>(raw_value_bytes);
      unk_off = static_cast<uint32_t>(raw_unk_off);
      unk_bytes = static_cast<uint32_t>(raw_unk_bytes);
    }

    words.push_back(llvm::ConstantInt::get(i32_ty, base_off));
    words.push_back(llvm::ConstantInt::get(i32_ty, total_bytes));
    words.push_back(llvm::ConstantInt::get(i32_ty, kind_val));
    words.push_back(llvm::ConstantInt::get(i32_ty, value_off));
    words.push_back(llvm::ConstantInt::get(i32_ty, value_bytes));
    words.push_back(llvm::ConstantInt::get(i32_ty, unk_off));
    words.push_back(llvm::ConstantInt::get(i32_ty, unk_bytes));
  }

  auto* array_type = llvm::ArrayType::get(i32_ty, words.size());
  auto* initializer = llvm::ConstantArray::get(array_type, words);
  auto* global_var = new llvm::GlobalVariable(
      context.GetModule(), array_type, true, llvm::GlobalValue::InternalLinkage,
      initializer, "__lyra_slot_meta_table");

  // GEP to first element: [N x i32]* -> i32*
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* table_ptr = llvm::ConstantExpr::getInBoundsGetElementPtr(
      array_type, global_var, llvm::ArrayRef<llvm::Constant*>{zero, zero});

  return {.table_ptr = table_ptr, .count = static_cast<uint32_t>(slots.size())};
}

// Initialize DesignState: zero everything, apply 4-state patches, then handle
// composite 4-state fields with recursive init.
void InitializeDesignState(
    Context& context, llvm::Value* design_state,
    const std::vector<SlotInfo>& slots, const FourStatePatchTable& patches) {
  auto& builder = context.GetBuilder();
  auto* design_type = context.GetDesignStateType();
  bool force_two_state = context.IsForceTwoState();

  // 1. Zero-initialize via memset (avoids LLVM assertion on wide aggregates)
  EmitMemsetZero(context, design_state, design_type);

  // In two-state mode, memset(0) is sufficient (no 4-state patches or init)
  if (force_two_state) return;

  // 2. Apply scalar 4-state patches via runtime helper (O(1) IR calls)
  EmitApply4StatePatches(context, design_state, patches, "design");

  // 3. Handle composite 4-state fields (arrays, structs with 4-state content)
  //    These cannot be patched and need the recursive init path.
  const auto& types = context.GetTypeArena();
  for (const auto& slot : slots) {
    // Skip if not 4-state at all
    if (!context.IsFourState(slot.type_id)) {
      continue;
    }
    // Skip if scalar patchable (already handled by patch table)
    if (IsScalarPatchable(slot.type_id, types, force_two_state)) {
      continue;
    }
    // Composite 4-state: use recursive init
    uint32_t field_index = context.GetDesignFieldIndex(slot.slot_id);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    EmitSVDefaultInitAfterZero(context, slot_ptr, slot.type_id);
  }
}

// Initialize ProcessStateN: zero everything, set design pointer, apply 4-state
// patches, then handle composite 4-state fields with recursive init.
// Takes ProcessLayout explicitly to avoid implicit dependency on
// Context::SetCurrentProcess.
void InitializeProcessState(
    Context& context, llvm::Value* process_state, llvm::Value* design_state,
    const ProcessLayout& proc_layout, size_t process_index) {
  auto& builder = context.GetBuilder();
  auto* state_type = proc_layout.state_type;
  auto* header_type = context.GetHeaderType();
  bool force_two_state = context.IsForceTwoState();

  // 1. Zero-initialize via memset (avoids LLVM assertion on wide aggregates)
  EmitMemsetZero(context, process_state, state_type);

  // 2. Set design pointer: header->design (field 1)
  auto* header_ptr = builder.CreateStructGEP(state_type, process_state, 0);
  auto* design_ptr_ptr = builder.CreateStructGEP(header_type, header_ptr, 1);
  builder.CreateStore(design_state, design_ptr_ptr);

  // In two-state mode, memset(0) is sufficient (no 4-state patches or init)
  if (force_two_state) return;

  // 3. Apply scalar 4-state patches to frame via runtime helper
  //    Note: patches are relative to frame base, not process_state base
  auto* frame_type = proc_layout.frame.llvm_type;
  auto* frame_ptr = builder.CreateStructGEP(state_type, process_state, 1);
  const auto& frame_layout = proc_layout.frame;

  std::string frame_prefix = std::format("frame.{}", process_index);
  EmitApply4StatePatches(
      context, frame_ptr, frame_layout.four_state_patches, frame_prefix);

  // 4. Handle composite 4-state fields (arrays, structs with 4-state content)
  const auto& types = context.GetTypeArena();
  for (uint32_t i = 0; i < frame_layout.root_types.size(); ++i) {
    TypeId type_id = frame_layout.root_types[i];
    // Skip if not 4-state at all
    if (!context.IsFourState(type_id)) {
      continue;
    }
    // Skip if scalar patchable (already handled by patch table)
    if (IsScalarPatchable(type_id, types, force_two_state)) {
      continue;
    }
    // Composite 4-state: use recursive init
    auto* field_ptr = builder.CreateStructGEP(frame_type, frame_ptr, i);
    EmitSVDefaultInitAfterZero(context, field_ptr, type_id);
  }
}

// Release all string slots to prevent memory leaks
void ReleaseStringSlots(
    Context& context, const std::vector<SlotInfo>& slots,
    llvm::Value* design_state) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* design_type = context.GetDesignStateType();

  for (const auto& slot : slots) {
    if (slot.type_info.kind != VarTypeKind::kString) {
      continue;
    }

    uint32_t field_index = context.GetDesignFieldIndex(slot.slot_id);
    auto* slot_ptr =
        builder.CreateStructGEP(design_type, design_state, field_index);
    auto* val = builder.CreateLoad(ptr_ty, slot_ptr);
    builder.CreateCall(context.GetLyraStringRelease(), {val});
  }
}

}  // namespace

void EmitVariableInspection(
    Context& context, const std::vector<VariableInfo>& variables,
    const std::vector<SlotInfo>& slots, llvm::Value* design_state) {
  if (variables.empty()) {
    return;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* design_type = context.GetDesignStateType();

  for (const auto& var : variables) {
    if (var.slot_id >= slots.size()) {
      continue;
    }

    auto slot_id = mir::SlotId{static_cast<uint32_t>(var.slot_id)};
    uint32_t field_index = context.GetDesignFieldIndex(slot_id);
    auto* slot_ptr = builder.CreateStructGEP(
        design_type, design_state, field_index, "var_ptr");

    const auto& type_info = slots[var.slot_id].type_info;

    auto* name_ptr = builder.CreateGlobalStringPtr(var.name);
    auto* kind_val =
        llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(type_info.kind));
    auto* width_val = llvm::ConstantInt::get(i32_ty, type_info.width);
    auto* signed_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_signed ? 1 : 0);
    auto* four_state_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_four_state ? 1 : 0);
    builder.CreateCall(
        context.GetLyraRegisterVar(),
        {name_ptr, slot_ptr, kind_val, width_val, signed_val, four_state_val});
  }

  builder.CreateCall(context.GetLyraSnapshotVars());
}

void EmitTimeReport(Context& context) {
  context.GetBuilder().CreateCall(context.GetLyraReportTime());
}

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult> {
  // Create LLVM context and module
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);

  // Set DataLayout BEFORE any IR generation that depends on type
  // sizes/alignment. Without this, getTypeAllocSize() uses default rules that
  // may differ from the target's actual layout, causing size mismatches (e.g.,
  // memset length).
  SetHostDataLayout(*module);

  bool force_two_state = input.force_two_state;

  // Build slot info from design's slot_table (MIR is single source of truth)
  auto slot_info = BuildSlotInfoFromDesign(
      *input.design, *input.type_arena, force_two_state);

  // Build layout first (pure analysis, no LLVM IR emission)
  Layout layout = BuildLayout(
      *input.design, *input.mir_arena, *input.type_arena, slot_info, *llvm_ctx,
      module->getDataLayout(), force_two_state);

  // Create context with layout
  Context context(
      *input.design, *input.mir_arena, *input.type_arena, layout,
      std::move(llvm_ctx), std::move(module), input.diag_ctx, force_two_state);

  auto& builder = context.GetBuilder();
  auto& ctx = context.GetLlvmContext();
  auto& mod = context.GetModule();

  // Pre-scan all processes for MonitorEffect instructions and register their
  // thunk info in the context. This must happen before DeclareUserFunction
  // because:
  // - Check thunks have a different signature (3 args instead of 2)
  // - Setup thunks need appended serialization + registration code
  auto register_monitor_info = [&](mir::ProcessId proc_id) {
    const auto& process = (*input.mir_arena)[proc_id];
    for (const auto& block : process.blocks) {
      for (const auto& instr : block.statements) {
        const auto* effect = std::get_if<mir::Effect>(&instr.data);
        if (effect == nullptr) continue;
        const auto* monitor = std::get_if<mir::MonitorEffect>(&effect->op);
        if (monitor == nullptr) continue;

        // Register monitor layout (offsets/sizes only - format_ops come from
        // check thunk's own DisplayEffect to ensure correct MIR operand
        // context)
        Context::MonitorLayout layout{
            .offsets = monitor->offsets,
            .byte_sizes = monitor->byte_sizes,
            .total_size = monitor->prev_buffer_size,
        };
        context.RegisterMonitorLayout(monitor->check_thunk, std::move(layout));

        // Register setup thunk marker (just references check_thunk for layout)
        Context::MonitorSetupInfo setup_info{
            .check_thunk = monitor->check_thunk,
        };
        context.RegisterMonitorSetupInfo(
            monitor->setup_thunk, std::move(setup_info));
      }
    }
  };
  // Scan module processes
  for (const auto& element : input.design->elements) {
    if (const auto* mod_elem = std::get_if<mir::Module>(&element)) {
      for (mir::ProcessId proc_id : mod_elem->processes) {
        register_monitor_info(proc_id);
      }
    }
  }
  // Scan init_processes (package variable initialization)
  for (mir::ProcessId proc_id : input.design->init_processes) {
    register_monitor_info(proc_id);
  }

  // Two-pass user function generation for mutual recursion support
  // Collect all function IDs from modules, packages, and generated functions
  std::vector<mir::FunctionId> all_func_ids;
  for (const auto& element : input.design->elements) {
    std::visit(
        [&](const auto& elem) {
          for (mir::FunctionId func_id : elem.functions) {
            all_func_ids.push_back(func_id);
          }
        },
        element);
  }
  // Include dynamically generated functions (e.g., strobe thunks)
  for (mir::FunctionId func_id : input.design->generated_functions) {
    all_func_ids.push_back(func_id);
  }

  // Pass 1: Declare all user functions (builds function types, registers them)
  std::vector<std::pair<mir::FunctionId, llvm::Function*>> declared_funcs;
  declared_funcs.reserve(all_func_ids.size());
  for (size_t i = 0; i < all_func_ids.size(); ++i) {
    mir::FunctionId func_id = all_func_ids[i];
    auto llvm_func_or_err =
        DeclareUserFunction(context, func_id, std::format("user_func_{}", i));
    if (!llvm_func_or_err) return std::unexpected(llvm_func_or_err.error());
    llvm::Function* llvm_func = *llvm_func_or_err;
    declared_funcs.emplace_back(func_id, llvm_func);
  }

  // Pass 2: Define all user functions (emits bodies, can reference other funcs)
  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineUserFunction(context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Generate process functions (use process_ids from layout for single source
  // of truth)
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout.process_ids.size());

  for (size_t i = 0; i < layout.process_ids.size(); ++i) {
    context.SetCurrentProcess(i);

    // Use the canonical process list from layout
    mir::ProcessId proc_id = layout.process_ids[i];
    const auto& mir_process = (*input.mir_arena)[proc_id];

    // Set instance_id for %m support
    context.SetCurrentInstanceId(mir_process.owner_instance_id);

    auto func_result = GenerateProcessFunction(
        context, mir_process, std::format("process_{}", i));
    if (!func_result) return std::unexpected(func_result.error());
    process_funcs.push_back(*func_result);
  }

  // Create main function.
  // kEmbeddedPlusargs: int main() - plusargs baked into IR
  // kArgvForwarding: int main(int argc, char** argv) - plusargs from CLI
  // Note: In LLVM opaque pointer mode (17+), all pointers are `ptr`.
  // The `ptr` for argv represents `char**`; GEPs use element type `ptr`
  // to step by sizeof(pointer), correctly indexing the argv array.
  bool argv_forwarding = input.main_abi == MainAbi::kArgvForwarding;
  llvm::FunctionType* main_type = nullptr;
  if (argv_forwarding) {
    auto* i32_ty_main = llvm::Type::getInt32Ty(ctx);
    auto* ptr_ty_main = llvm::PointerType::getUnqual(ctx);
    main_type =
        llvm::FunctionType::get(i32_ty_main, {i32_ty_main, ptr_ty_main}, false);
  } else {
    main_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(ctx), false);
  }
  auto* main_func = llvm::Function::Create(
      main_type, llvm::Function::ExternalLinkage, "main", &mod);

  auto* entry = llvm::BasicBlock::Create(ctx, "entry", main_func);
  auto* exit_block = llvm::BasicBlock::Create(ctx, "exit", main_func);
  builder.SetInsertPoint(entry);

  // Allocate DesignState (shared by all processes)
  auto* design_state = builder.CreateAlloca(
      context.GetDesignStateType(), nullptr, "design_state");

  // Initialize DesignState (zero + 4-state patches + composite 4-state init)
  InitializeDesignState(
      context, design_state, slot_info, layout.design.four_state_patches);

  // Initialize runtime state (reset time tracker, set fs_base_dir)
  llvm::Value* fs_base_dir_str = nullptr;
  if (argv_forwarding) {
    // AOT: resolve base dir from argv[0] (bundle root) or LYRA_FS_BASE_DIR env
    auto* argv0 = builder.CreateLoad(
        llvm::PointerType::getUnqual(ctx), main_func->getArg(1), "argv0");
    fs_base_dir_str =
        builder.CreateCall(context.GetLyraResolveBaseDir(), {argv0});
  } else {
    fs_base_dir_str =
        builder.CreateGlobalStringPtr(input.fs_base_dir, "fs_base_dir");
  }
  builder.CreateCall(context.GetLyraInitRuntime(), {fs_base_dir_str});

  // Hook: after design state and runtime are initialized
  if (input.hooks != nullptr) {
    input.hooks->OnAfterInitializeDesignState(context, slot_info, design_state);
  }

  auto* ptr_ty = llvm::PointerType::getUnqual(ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(ctx);

  // Phase 1: Init processes (package variable initialization)
  // Run synchronously before scheduler, matching MIR interpreter behavior
  size_t num_init = layout.num_init_processes;
  for (size_t i = 0; i < num_init; ++i) {
    const auto& proc_layout = layout.processes[i];

    auto* process_state = builder.CreateAlloca(
        proc_layout.state_type, nullptr, std::format("init_state_{}", i));

    InitializeProcessState(
        context, process_state, design_state, proc_layout, i);

    // Run synchronously to completion via runtime API
    // (entry block number is encapsulated in LyraRunProcessSync)
    builder.CreateCall(
        context.GetLyraRunProcessSync(), {process_funcs[i], process_state});
  }

  // Hook: after init processes, before module simulation
  if (input.hooks != nullptr) {
    input.hooks->OnBeforeRunSimulation(context, slot_info, design_state);
  }

  // Phase 2: Module processes through scheduler
  auto num_regular_module =
      static_cast<uint32_t>(process_funcs.size() - num_init);
  auto num_kernelized =
      static_cast<uint32_t>(layout.connection_kernel_entries.size());
  auto num_module_processes = num_regular_module + num_kernelized;

  if (num_module_processes > 0) {
    auto* i8_ty = llvm::Type::getInt8Ty(ctx);
    const auto& dl = mod.getDataLayout();
    auto* header_type = context.GetHeaderType();

    // Compute packed process state buffer layout.
    // All regular module process frames are packed into a single alloca,
    // replacing N allocas + N memsets with 1 alloca + 1 memset.
    struct ProcessInitDesc {
      uint64_t offset;      // Byte offset into packed buffer
      uint64_t size;        // State type alloc size
      size_t layout_index;  // Index into layout.processes
      bool has_4state_patches;
      bool has_composite_4state;
    };

    std::vector<ProcessInitDesc> proc_descs;
    proc_descs.reserve(num_regular_module);
    uint64_t packed_buffer_size = 0;
    uint64_t max_align = 16;

    for (size_t i = num_init; i < process_funcs.size(); ++i) {
      const auto& proc_layout = layout.processes[i];
      auto* state_type = proc_layout.state_type;
      uint64_t size = dl.getTypeAllocSize(state_type);
      uint64_t align = dl.getABITypeAlign(state_type).value();
      max_align = std::max(max_align, align);

      // Align offset
      packed_buffer_size = (packed_buffer_size + align - 1) & ~(align - 1);

      // Check for 4-state work
      bool has_patches = !context.IsForceTwoState() &&
                         !proc_layout.frame.four_state_patches.IsEmpty();
      bool has_composite = false;
      if (!context.IsForceTwoState()) {
        const auto& types = context.GetTypeArena();
        for (TypeId type_id : proc_layout.frame.root_types) {
          if (context.IsFourState(type_id) &&
              !IsScalarPatchable(type_id, types, force_two_state)) {
            has_composite = true;
            break;
          }
        }
      }

      proc_descs.push_back({
          .offset = packed_buffer_size,
          .size = size,
          .layout_index = i,
          .has_4state_patches = has_patches,
          .has_composite_4state = has_composite,
      });
      packed_buffer_size += size;
    }

    // Allocate states_array upfront (used by both init loops)
    auto* states_array = builder.CreateAlloca(
        ptr_ty, llvm::ConstantInt::get(i32_ty, num_module_processes),
        "states_array");

    // Allocate and zero-initialize the packed process state buffer
    llvm::Value* packed_states = nullptr;
    if (num_regular_module > 0) {
      packed_states = builder.CreateAlloca(
          i8_ty, builder.getInt64(packed_buffer_size), "packed_states");
      if (auto* alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(packed_states)) {
        alloca_inst->setAlignment(llvm::Align(max_align));
      }
      builder.CreateMemSet(
          packed_states, builder.getInt8(0), packed_buffer_size,
          llvm::MaybeAlign(max_align));
    }

    // Initialize regular module process states from packed buffer.
    // Emit a global offset table and use a loop for design pointer setup,
    // with per-process 4-state patches handled separately (rare).
    if (num_regular_module > 0) {
      // Emit offset table as global constant: [N x i64]
      auto* i64_ty = llvm::Type::getInt64Ty(ctx);
      std::vector<llvm::Constant*> offset_constants;
      offset_constants.reserve(num_regular_module);
      for (const auto& desc : proc_descs) {
        offset_constants.push_back(llvm::ConstantInt::get(i64_ty, desc.offset));
      }
      auto* offset_array_type =
          llvm::ArrayType::get(i64_ty, num_regular_module);
      auto* offsets_global = new llvm::GlobalVariable(
          mod, offset_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(offset_array_type, offset_constants),
          "__lyra_proc_offsets");
      offsets_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

      // Compute design pointer offset within ProcessState:
      // header is field 0, design_ptr is field 1 within header
      auto* header_llvm_type = context.GetHeaderType();
      const llvm::StructLayout* hdr_sl = dl.getStructLayout(header_llvm_type);
      uint64_t design_ptr_offset_in_header = hdr_sl->getElementOffset(1);

      // Loop: for each regular process, compute state ptr, set design ptr
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
      auto* proc_cond =
          builder.CreateICmpULT(proc_phi, builder.getInt32(num_regular_module));
      builder.CreateCondBr(proc_cond, proc_loop_body, proc_loop_exit);

      builder.SetInsertPoint(proc_loop_body);

      // Load offset from table
      auto* offset_ptr = builder.CreateGEP(
          offset_array_type, offsets_global, {builder.getInt32(0), proc_phi});
      auto* offset_val = builder.CreateLoad(i64_ty, offset_ptr);

      // Compute state pointer
      auto* state_ptr = builder.CreateGEP(i8_ty, packed_states, {offset_val});

      // Set design pointer via byte offset (avoid per-type StructGEP)
      auto* design_ptr_loc = builder.CreateGEP(
          i8_ty, state_ptr, {builder.getInt64(design_ptr_offset_in_header)});
      builder.CreateStore(design_state, design_ptr_loc);

      // Store into states_array
      auto* state_slot = builder.CreateGEP(ptr_ty, states_array, {proc_phi});
      builder.CreateStore(state_ptr, state_slot);

      auto* proc_next = builder.CreateAdd(proc_phi, builder.getInt32(1));
      proc_phi->addIncoming(proc_next, proc_loop_body);
      builder.CreateBr(proc_loop_header);

      builder.SetInsertPoint(proc_loop_exit);
    }

    // Handle 4-state patches for processes that need them (rare, O(few))
    for (uint32_t pi = 0; pi < num_regular_module; ++pi) {
      const auto& desc = proc_descs[pi];
      if (!desc.has_4state_patches && !desc.has_composite_4state) continue;

      const auto& proc_layout = layout.processes[desc.layout_index];
      auto* state_ptr = builder.CreateGEP(
          i8_ty, packed_states, {builder.getInt64(desc.offset)});
      auto* state_type = proc_layout.state_type;

      if (desc.has_4state_patches) {
        auto* frame_ptr = builder.CreateStructGEP(state_type, state_ptr, 1);
        std::string prefix = std::format("frame.{}", desc.layout_index);
        EmitApply4StatePatches(
            context, frame_ptr, proc_layout.frame.four_state_patches, prefix);
      }

      if (desc.has_composite_4state) {
        auto* frame_type = proc_layout.frame.llvm_type;
        auto* frame_ptr = builder.CreateStructGEP(state_type, state_ptr, 1);
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

    // Kernelized connection processes: global descriptor table + loop
    llvm::Value* conn_buffer = nullptr;
    uint64_t conn_state_size = 0;
    if (num_kernelized > 0) {
      auto* design_struct = layout.design.llvm_type;
      const llvm::StructLayout* design_sl = dl.getStructLayout(design_struct);

      // Build connection descriptor constants (pre-compute all values as C++
      // data, then emit as a single typed LLVM global constant array)
      auto* i32_llvm = llvm::Type::getInt32Ty(ctx);
      auto* i16_llvm = llvm::Type::getInt16Ty(ctx);
      auto* i8_llvm = llvm::Type::getInt8Ty(ctx);

      // ConnectionDescriptor LLVM struct type (mirrors C++ layout exactly)
      auto* conn_desc_llvm_type = llvm::StructType::create(
          ctx,
          {i32_llvm, i32_llvm, i32_llvm,
           i32_llvm,  // src/dst offset, size, dst_slot
           i32_llvm, i8_llvm, i8_llvm,
           i16_llvm,             // trigger_slot, edge, bit_idx, pad
           i32_llvm, i32_llvm},  // trigger byte offset/size
          "ConnectionDescriptor");

      std::vector<llvm::Constant*> desc_constants;
      desc_constants.reserve(num_kernelized);

      for (uint32_t ki = 0; ki < num_kernelized; ++ki) {
        const auto& entry = layout.connection_kernel_entries[ki];

        auto src_it = layout.design.slot_to_field.find(entry.src_slot);
        auto dst_it = layout.design.slot_to_field.find(entry.dst_slot);
        if (src_it == layout.design.slot_to_field.end() ||
            dst_it == layout.design.slot_to_field.end()) {
          throw common::InternalError(
              "LowerMirToLlvm",
              "kernelized connection slot not in design layout");
        }

        uint32_t src_field = src_it->second;
        uint32_t dst_field = dst_it->second;
        auto src_offset =
            static_cast<uint32_t>(design_sl->getElementOffset(src_field));
        auto dst_offset =
            static_cast<uint32_t>(design_sl->getElementOffset(dst_field));
        auto* dst_field_type = design_struct->getElementType(dst_field);
        auto byte_size =
            static_cast<uint32_t>(dl.getTypeAllocSize(dst_field_type));

        uint32_t trigger_byte_offset = 0;
        uint32_t trigger_byte_size = 0;
        uint8_t trigger_bit_index = 0;
        if (entry.trigger_observed_place) {
          auto trigger_slot_it =
              layout.design.slot_to_field.find(entry.trigger_slot);
          if (trigger_slot_it != layout.design.slot_to_field.end()) {
            TypeId trigger_root_type =
                input.design->slot_table[trigger_slot_it->first.value];
            const auto& trigger_place =
                (*input.mir_arena)[*entry.trigger_observed_place];
            auto range = ResolveByteRange(
                ctx, dl, *input.type_arena, trigger_place, trigger_root_type,
                nullptr, force_two_state);
            if (range.kind == RangeKind::kPrecise) {
              trigger_byte_offset = range.byte_offset;
              trigger_byte_size = range.byte_size;
              trigger_bit_index = range.bit_index;
            }
          }
        }

        desc_constants.push_back(
            llvm::ConstantStruct::get(
                conn_desc_llvm_type,
                {llvm::ConstantInt::get(i32_llvm, src_offset),
                 llvm::ConstantInt::get(i32_llvm, dst_offset),
                 llvm::ConstantInt::get(i32_llvm, byte_size),
                 llvm::ConstantInt::get(i32_llvm, entry.dst_slot.value),
                 llvm::ConstantInt::get(i32_llvm, entry.trigger_slot.value),
                 llvm::ConstantInt::get(
                     i8_llvm, static_cast<uint8_t>(entry.trigger_edge)),
                 llvm::ConstantInt::get(i8_llvm, trigger_bit_index),
                 llvm::ConstantInt::get(i16_llvm, 0),
                 llvm::ConstantInt::get(i32_llvm, trigger_byte_offset),
                 llvm::ConstantInt::get(i32_llvm, trigger_byte_size)}));
      }

      // Emit global constant table: @conn_descs = internal constant [N x
      // %ConnDesc]
      auto* desc_array_type =
          llvm::ArrayType::get(conn_desc_llvm_type, num_kernelized);
      auto* desc_table = new llvm::GlobalVariable(
          mod, desc_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(desc_array_type, desc_constants),
          "__lyra_conn_descs");
      desc_table->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

      // Build connection kernel state type:
      // {ProcessStateHeader, ConnectionDescriptor}
      auto* raw_desc_type =
          llvm::ArrayType::get(i8_ty, sizeof(runtime::ConnectionDescriptor));
      auto* conn_state_type = llvm::StructType::create(
          ctx, {header_type, raw_desc_type}, "ConnectionKernelState");

      // Allocate all connection states in one packed buffer
      conn_state_size = dl.getTypeAllocSize(conn_state_type);
      uint64_t conn_state_align = dl.getABITypeAlign(conn_state_type).value();
      uint64_t conn_buffer_size = conn_state_size * num_kernelized;

      conn_buffer = builder.CreateAlloca(
          i8_ty, builder.getInt64(conn_buffer_size), "conn_states");
      if (auto* alloca_inst = llvm::dyn_cast<llvm::AllocaInst>(conn_buffer)) {
        alloca_inst->setAlignment(llvm::Align(conn_state_align));
      }
      builder.CreateMemSet(
          conn_buffer, builder.getInt8(0), conn_buffer_size,
          llvm::MaybeAlign(conn_state_align));

      // Descriptor copy size
      uint64_t desc_copy_size = sizeof(runtime::ConnectionDescriptor);

      // Loop: for each connection, set design pointer + memcpy descriptor
      auto* loop_preheader = builder.GetInsertBlock();
      auto* loop_header =
          llvm::BasicBlock::Create(ctx, "conn_init_header", main_func);
      auto* loop_body =
          llvm::BasicBlock::Create(ctx, "conn_init_body", main_func);
      auto* loop_exit =
          llvm::BasicBlock::Create(ctx, "conn_init_exit", main_func);

      builder.CreateBr(loop_header);
      builder.SetInsertPoint(loop_header);

      auto* phi = builder.CreatePHI(i32_ty, 2, "conn_idx");
      phi->addIncoming(builder.getInt32(0), loop_preheader);
      auto* cond = builder.CreateICmpULT(phi, builder.getInt32(num_kernelized));
      builder.CreateCondBr(cond, loop_body, loop_exit);

      builder.SetInsertPoint(loop_body);

      // Compute state pointer: conn_buffer + i * conn_state_size
      auto* idx_ext = builder.CreateZExt(phi, builder.getInt64Ty());
      auto* state_byte_offset =
          builder.CreateMul(idx_ext, builder.getInt64(conn_state_size));
      auto* conn_state_ptr =
          builder.CreateGEP(i8_ty, conn_buffer, {state_byte_offset});

      // Set design pointer in header
      auto* hdr_ptr =
          builder.CreateStructGEP(conn_state_type, conn_state_ptr, 0);
      auto* design_ptr_ptr = builder.CreateStructGEP(header_type, hdr_ptr, 1);
      builder.CreateStore(design_state, design_ptr_ptr);

      // Memcpy descriptor from global table
      auto* desc_src_ptr = builder.CreateGEP(
          desc_array_type, desc_table, {builder.getInt32(0), phi});
      // The raw descriptor subobject in conn_state is at field 1
      auto* desc_dst_ptr =
          builder.CreateStructGEP(conn_state_type, conn_state_ptr, 1);
      builder.CreateMemCpy(
          desc_dst_ptr, llvm::MaybeAlign(4), desc_src_ptr, llvm::MaybeAlign(4),
          desc_copy_size);

      // Increment and branch back
      auto* next_idx = builder.CreateAdd(phi, builder.getInt32(1));
      phi->addIncoming(next_idx, loop_body);
      builder.CreateBr(loop_header);

      builder.SetInsertPoint(loop_exit);
    }

    // Build function pointer array as global constant
    std::vector<llvm::Constant*> func_constants;
    func_constants.reserve(num_module_processes);
    for (size_t i = num_init; i < process_funcs.size(); ++i) {
      func_constants.push_back(process_funcs[i]);
    }
    if (num_kernelized > 0) {
      auto* kernel_func = context.GetLyraConnectionKernel();
      for (uint32_t ki = 0; ki < num_kernelized; ++ki) {
        func_constants.push_back(kernel_func);
      }
    }

    auto* func_array_type = llvm::ArrayType::get(ptr_ty, num_module_processes);
    auto* funcs_global = new llvm::GlobalVariable(
        mod, func_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(func_array_type, func_constants),
        "__lyra_module_funcs");
    funcs_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    // GEP to first element for scheduler
    auto* funcs_array = llvm::ConstantExpr::getInBoundsGetElementPtr(
        func_array_type, funcs_global,
        llvm::ArrayRef<llvm::Constant*>{
            llvm::ConstantInt::get(i32_ty, 0),
            llvm::ConstantInt::get(i32_ty, 0)});

    // Connection state pointers via loop (from connection buffer, stride)
    if (num_kernelized > 0) {
      // Emit global offset table for connection state buffer strides
      // All connection states have the same size, so compute: base + i * stride
      auto* conn_loop_preheader = builder.GetInsertBlock();
      auto* conn_states_header =
          llvm::BasicBlock::Create(ctx, "conn_states_header", main_func);
      auto* conn_states_body =
          llvm::BasicBlock::Create(ctx, "conn_states_body", main_func);
      auto* conn_states_exit =
          llvm::BasicBlock::Create(ctx, "conn_states_exit", main_func);

      builder.CreateBr(conn_states_header);
      builder.SetInsertPoint(conn_states_header);

      auto* cs_phi = builder.CreatePHI(i32_ty, 2, "cs_idx");
      cs_phi->addIncoming(builder.getInt32(0), conn_loop_preheader);
      auto* cs_cond =
          builder.CreateICmpULT(cs_phi, builder.getInt32(num_kernelized));
      builder.CreateCondBr(cs_cond, conn_states_body, conn_states_exit);

      builder.SetInsertPoint(conn_states_body);

      // Compute connection state pointer: conn_buffer + i * conn_state_size
      auto* cs_idx_ext = builder.CreateZExt(cs_phi, builder.getInt64Ty());
      auto* cs_byte_off =
          builder.CreateMul(cs_idx_ext, builder.getInt64(conn_state_size));
      auto* cs_state_ptr = builder.CreateGEP(i8_ty, conn_buffer, {cs_byte_off});

      // Store into states_array at offset num_regular_module + i
      auto* cs_array_idx =
          builder.CreateAdd(cs_phi, builder.getInt32(num_regular_module));
      auto* cs_state_slot =
          builder.CreateGEP(ptr_ty, states_array, {cs_array_idx});
      builder.CreateStore(cs_state_ptr, cs_state_slot);

      auto* cs_next = builder.CreateAdd(cs_phi, builder.getInt32(1));
      cs_phi->addIncoming(cs_next, conn_states_body);
      builder.CreateBr(conn_states_header);

      builder.SetInsertPoint(conn_states_exit);
    }

    // Build plusargs array for $test$plusargs and $value$plusargs
    llvm::Value* plusargs_array = nullptr;
    llvm::Value* num_plusargs_val = nullptr;
    if (argv_forwarding) {
      // AOT: plusargs come from argv[1:] at runtime
      auto* argc_val = main_func->getArg(0);
      auto* argv_val = main_func->getArg(1);
      // Clamp to 0 if argc < 1 (practically impossible but defensive)
      auto* argc_minus_1 =
          builder.CreateSub(argc_val, llvm::ConstantInt::get(i32_ty, 1));
      auto* is_positive = builder.CreateICmpSGT(
          argc_minus_1, llvm::ConstantInt::get(i32_ty, 0));
      num_plusargs_val = builder.CreateSelect(
          is_positive, argc_minus_1, llvm::ConstantInt::get(i32_ty, 0),
          "num_plusargs");
      plusargs_array = builder.CreateGEP(
          ptr_ty, argv_val, {llvm::ConstantInt::get(i32_ty, 1)}, "plusargs");
    } else {
      // JIT: plusargs baked into IR as global string constants
      auto num_plusargs = static_cast<uint32_t>(input.plusargs.size());
      num_plusargs_val = llvm::ConstantInt::get(i32_ty, num_plusargs);
      if (num_plusargs > 0) {
        plusargs_array = builder.CreateAlloca(
            ptr_ty, llvm::ConstantInt::get(i32_ty, num_plusargs), "plusargs");
        for (uint32_t i = 0; i < num_plusargs; ++i) {
          auto* plusarg_str = builder.CreateGlobalStringPtr(
              input.plusargs[i], std::format("plusarg_{}", i));
          auto* slot =
              builder.CreateGEP(ptr_ty, plusargs_array, {builder.getInt32(i)});
          builder.CreateStore(plusarg_str, slot);
        }
      } else {
        plusargs_array = llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptr_ty));
      }
    }

    // Build instance paths array as global constant
    const auto& instance_entries = input.design->instance_table.entries;
    auto num_instance_paths = static_cast<uint32_t>(instance_entries.size());
    llvm::Value* instance_paths_array = nullptr;
    if (num_instance_paths > 0) {
      // Create global string constants for all paths
      std::vector<llvm::Constant*> path_constants;
      path_constants.reserve(num_instance_paths);
      for (uint32_t i = 0; i < num_instance_paths; ++i) {
        auto* path_str = builder.CreateGlobalStringPtr(
            instance_entries[i].full_path, std::format("inst_path_{}", i));
        path_constants.push_back(llvm::cast<llvm::Constant>(path_str));
      }
      auto* path_array_type = llvm::ArrayType::get(ptr_ty, num_instance_paths);
      auto* paths_global = new llvm::GlobalVariable(
          mod, path_array_type, true, llvm::GlobalValue::InternalLinkage,
          llvm::ConstantArray::get(path_array_type, path_constants),
          "__lyra_instance_paths");
      paths_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
      instance_paths_array = llvm::ConstantExpr::getInBoundsGetElementPtr(
          path_array_type, paths_global,
          llvm::ArrayRef<llvm::Constant*>{
              llvm::ConstantInt::get(i32_ty, 0),
              llvm::ConstantInt::get(i32_ty, 0)});
    } else {
      instance_paths_array =
          llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    }

    // Build slot metadata table
    auto [meta_table, meta_count] = EmitSlotMetaTable(
        context, slot_info, layout.design, mod.getDataLayout(),
        *input.type_arena);

    // Call multi-process scheduler
    builder.CreateCall(
        context.GetLyraRunSimulation(),
        {funcs_array, states_array,
         llvm::ConstantInt::get(i32_ty, num_module_processes), plusargs_array,
         num_plusargs_val, instance_paths_array,
         llvm::ConstantInt::get(i32_ty, num_instance_paths), meta_table,
         llvm::ConstantInt::get(i32_ty, meta_count),
         llvm::ConstantInt::get(i32_ty, runtime::slot_meta_abi::kVersion),
         llvm::ConstantInt::get(i32_ty, input.feature_flags)});
  }

  // Branch to exit block
  builder.CreateBr(exit_block);

  // Exit block: release strings, run hooks (if any), return 0
  builder.SetInsertPoint(exit_block);

  // Release all string locals to prevent memory leaks
  ReleaseStringSlots(context, slot_info, design_state);

  // Hook: after simulation completes
  if (input.hooks != nullptr) {
    input.hooks->OnAfterRunSimulation(context, slot_info, design_state);
  }

  builder.CreateRet(llvm::ConstantInt::get(ctx, llvm::APInt(32, 0)));

  auto [result_ctx, result_mod] = context.TakeOwnership();
  return LoweringResult{
      .context = std::move(result_ctx),
      .module = std::move(result_mod),
  };
}

auto DumpLlvmIr(const LoweringResult& result) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  result.module->print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
