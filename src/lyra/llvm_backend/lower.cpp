#include "lyra/llvm_backend/lower.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/link/build_design_metadata.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/design_metadata_lowering.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/type_ops/default_init.hpp"
#include "lyra/llvm_backend/type_ops/four_state_init.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/runtime/feature_flags.hpp"
#include "lyra/runtime/runtime_abi.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/wait_site.hpp"

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

  // Word table format:
  // [version, num_sites, (id, resume_block, shape_and_triggers, flags)*]
  std::vector<llvm::Constant*> words;
  words.reserve(2 + entries.size() * runtime::wait_site_abi::kStride);

  words.push_back(
      llvm::ConstantInt::get(i32_ty, runtime::wait_site_abi::kVersion));
  words.push_back(
      llvm::ConstantInt::get(i32_ty, static_cast<uint32_t>(entries.size())));

  for (uint32_t i = 0; i < entries.size(); ++i) {
    const auto& entry = entries[i];

    // Classify shape from compiled trigger properties.
    // kStatic: all installed nodes refreshable in place (no rebind, no
    // container) kRebindable: signal set fixed, observation targets may move
    // (late-bound) kDynamic: structural reasons preventing in-place refresh
    // (containers)
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

void EmitParamInitStores(
    Context& context, llvm::Value* design_state, const mir::Design& design) {
  if (design.instance_param_inits.empty()) return;

  auto& builder = context.GetBuilder();
  auto* design_type = context.GetDesignStateType();

  for (const auto& entries : design.instance_param_inits) {
    for (const auto& entry : entries) {
      auto slot_id = mir::SlotId{entry.slot_id};
      uint32_t field_index = context.GetDesignFieldIndex(slot_id);
      TypeId type_id = design.slots[entry.slot_id].type;

      auto* slot_ptr =
          builder.CreateStructGEP(design_type, design_state, field_index);

      Constant constant{.type = type_id, .value = entry.value};
      auto value_result = LowerConstant(context, constant);
      if (!value_result) {
        throw common::InternalError(
            "EmitParamInitStores", "failed to materialize param constant");
      }
      builder.CreateStore(*value_result, slot_ptr);
    }
  }
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

  // Build slot info from design's slots (MIR is single source of truth)
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

  // Gate loop guard emission via feature flag (opt-in).
  bool loop_guard_enabled = runtime::HasFlag(
      static_cast<runtime::FeatureFlag>(input.feature_flags),
      runtime::FeatureFlag::kEnableLoopGuard);
  context.SetLoopGuardEnabled(loop_guard_enabled);

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
  // Scan module processes (dedup with shared bodies)
  std::unordered_set<uint32_t> seen_proc_ids;
  for (const auto& element : input.design->elements) {
    if (const auto* mod_elem = std::get_if<mir::Module>(&element)) {
      const auto& body = mir::GetModuleBody(*input.design, *mod_elem);
      for (mir::ProcessId proc_id : body.processes) {
        if (seen_proc_ids.insert(proc_id.value).second) {
          register_monitor_info(proc_id);
        }
      }
    }
  }
  // Scan init_processes (package variable initialization)
  for (mir::ProcessId proc_id : input.design->init_processes) {
    register_monitor_info(proc_id);
  }

  // Two-pass user function generation for mutual recursion support.
  // Collect all unique function IDs from modules, packages, and generated
  // functions. With shared bodies, the same FunctionId appears for multiple
  // instances; dedup by tracking seen IDs.
  //
  // Module-scoped functions (non-thunk) are registered with specialization-
  // owned lowering metadata: rel_byte_offsets from the ModuleVariant,
  // base_slot_id from a representative instance's placement.
  std::vector<mir::FunctionId> all_func_ids;
  std::unordered_set<uint32_t> seen_func_ids;
  uint32_t func_collection_module_idx = 0;
  for (const auto& element : input.design->elements) {
    std::visit(
        common::Overloaded{
            [&](const mir::Module& mod) {
              const auto& body = mir::GetModuleBody(*input.design, mod);
              ModuleIndex mod_idx{func_collection_module_idx};
              for (mir::FunctionId func_id : body.functions) {
                if (seen_func_ids.insert(func_id.value).second) {
                  all_func_ids.push_back(func_id);
                }
                const auto& func = (*input.mir_arena)[func_id];
                if (func.thunk_kind == mir::ThunkKind::kNone) {
                  const auto& variant = layout.GetInstanceVariant(mod_idx);
                  const auto& placement = mir::GetInstancePlacement(
                      input.design->placement, mod_idx.value);
                  context.RegisterModuleScopedFunction(
                      func_id, {&variant.rel_byte_offsets,
                                placement.design_state_base_slot});
                }
              }
              ++func_collection_module_idx;
            },
            [&](const mir::Package& pkg) {
              for (mir::FunctionId func_id : pkg.functions) {
                if (seen_func_ids.insert(func_id.value).second) {
                  all_func_ids.push_back(func_id);
                }
              }
            },
        },
        element);
  }
  // Include dynamically generated functions (e.g., strobe thunks)
  for (mir::FunctionId func_id : input.design->generated_functions) {
    if (seen_func_ids.insert(func_id.value).second) {
      all_func_ids.push_back(func_id);
    }
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

  // Pass 2: Define all user functions (emits bodies, can reference other
  // funcs). Module-scoped functions set up their own addressing state from
  // the registered lowering metadata -- no ambient setup needed.
  for (const auto& [func_id, llvm_func] : declared_funcs) {
    auto result = DefineUserFunction(context, func_id, llvm_func);
    if (!result) return std::unexpected(result.error());
  }

  // Generate process functions (use scheduled_processes from layout for single
  // source of truth)
  std::vector<llvm::Function*> process_funcs;
  process_funcs.reserve(layout.scheduled_processes.size());

  // Collect wait-site entries from all process codegen results.
  // Entries are appended in ID order (Context::NextWaitSiteId is sequential).
  std::vector<WaitSiteEntry> all_wait_sites;

  // Phase 1: Emit template functions (one per ProcessTemplate)
  std::vector<llvm::Function*> template_fns(layout.process_templates.size());
  for (size_t t = 0; t < layout.process_templates.size(); ++t) {
    const auto& tmpl = layout.process_templates[t];
    context.SetCurrentProcess(tmpl.template_layout_index.value);

    const auto& mir_process = (*input.mir_arena)[tmpl.template_process];
    context.SetCurrentInstanceId(tmpl.representative_module_idx.value);

    const auto& variant =
        layout.GetInstanceVariant(tmpl.representative_module_idx);
    context.SetRelByteOffsets(
        variant.rel_byte_offsets, tmpl.template_base_slot_id);

    auto func_result =
        GenerateSharedProcessFunction(context, mir_process, tmpl.func_name);
    if (!func_result) return std::unexpected(func_result.error());
    template_fns[t] = func_result->function;
    all_wait_sites.insert(
        all_wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
  }

  // Phase 2: Emit per-process functions (wrappers vs standalone)
  size_t num_init = layout.num_init_processes;
  for (size_t i = 0; i < layout.scheduled_processes.size(); ++i) {
    context.SetCurrentProcess(i);

    const auto& bp = layout.scheduled_processes[i];
    const auto& mir_process = (*input.mir_arena)[bp.process_id];
    context.SetCurrentInstanceId(bp.module_index.value);

    auto route =
        layout.RouteProcess(LayoutProcessIndex{static_cast<uint32_t>(i)});
    if (auto* templated = std::get_if<TemplatedRoute>(&route)) {
      // Module behavioral process: emit a thin wrapper that calls the shared
      // template function with instance-specific bindings.
      uint64_t base_byte_offset =
          layout.GetInstanceBaseByteOffset(templated->module_idx);
      uint32_t base_slot_id = mir::GetInstanceBaseSlot(
          input.design->placement, templated->module_idx.value);

      auto* wrapper = GenerateProcessWrapper(
          context, template_fns[templated->template_id.value],
          bp.module_index.value, base_byte_offset, base_slot_id,
          std::format("process_{}", i));
      process_funcs.push_back(wrapper);
      continue;
    }

    // Standalone path: only for non-module processes (connection processes
    // that weren't kernelized). Module behavioral processes always route
    // through the specialization/shared path above.
    auto func_result = GenerateProcessFunction(
        context, mir_process, std::format("process_{}", i));
    if (!func_result) return std::unexpected(func_result.error());
    process_funcs.push_back(func_result->function);
    all_wait_sites.insert(
        all_wait_sites.end(),
        std::make_move_iterator(func_result->wait_sites.begin()),
        std::make_move_iterator(func_result->wait_sites.end()));
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

  // Initialize promoted param slots with constant values.
  // Must happen after InitializeDesignState (zero+4state patches)
  // but before any process reads them.
  EmitParamInitStores(context, design_state, *input.design);

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
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);

  // Phase 1: Init processes (package variable initialization)
  // Run synchronously before scheduler, matching MIR interpreter behavior
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
  auto num_module_processes = num_regular_module;

  if (num_module_processes > 0 || num_kernelized > 0) {
    auto* i8_ty = llvm::Type::getInt8Ty(ctx);
    const auto& dl = mod.getDataLayout();

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

    // Build function pointer array as global constant
    std::vector<llvm::Constant*> func_constants;
    func_constants.reserve(num_module_processes);
    for (size_t i = num_init; i < process_funcs.size(); ++i) {
      func_constants.push_back(process_funcs[i]);
    }

    auto* func_array_type = llvm::ArrayType::get(ptr_ty, num_module_processes);
    auto* funcs_global = new llvm::GlobalVariable(
        mod, func_array_type, true, llvm::GlobalValue::InternalLinkage,
        llvm::ConstantArray::get(func_array_type, func_constants),
        "__lyra_module_funcs");
    funcs_global->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

    auto* funcs_array = llvm::ConstantExpr::getInBoundsGetElementPtr(
        func_array_type, funcs_global,
        llvm::ArrayRef<llvm::Constant*>{
            llvm::ConstantInt::get(i32_ty, 0),
            llvm::ConstantInt::get(i32_ty, 0)});

    // Build plusargs array for $test$plusargs and $value$plusargs
    llvm::Value* plusargs_array = nullptr;
    llvm::Value* num_plusargs_val = nullptr;
    if (argv_forwarding) {
      auto* argc_val = main_func->getArg(0);
      auto* argv_val = main_func->getArg(1);
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

    // Extract metadata inputs from layout/codegen facts
    auto slot_meta_inputs = ExtractSlotMetaInputs(
        context, slot_info, layout.design, dl, *input.type_arena);
    auto conn_desc_entries = ExtractConnectionDescriptorEntries(
        input, layout, dl, ctx, force_two_state);
    auto scheduled_inputs = PrepareScheduledProcessInputs(
        input, layout.scheduled_processes, num_init);
    auto comb_inputs = PrepareCombKernelInputs(layout, num_init);
    auto instance_paths = PrepareInstancePaths(input);
    auto loop_site_inputs = PrepareLoopSiteInputs(context, input);

    // Build design metadata via link (all table construction in one place)
    link::DesignMetadataInputs metadata_inputs{
        .slot_meta = std::move(slot_meta_inputs),
        .scheduled_processes = std::move(scheduled_inputs),
        .loop_sites = std::move(loop_site_inputs),
        .connection_descriptors = std::move(conn_desc_entries),
        .comb_kernels = std::move(comb_inputs),
        .instance_paths = std::move(instance_paths),
    };
    auto metadata = link::BuildDesignMetadata(metadata_inputs);

    // Emit metadata as LLVM globals (thin emission only)
    auto meta_globals = EmitDesignMetadataGlobals(context, metadata, builder);

    // Build wait-site metadata table (from entries collected during codegen)
    auto wait_site_meta = EmitWaitSiteMetaTable(context, all_wait_sites);

    // LyraRuntimeAbi field indices. Must match runtime_abi.hpp layout.
    constexpr unsigned kAbiVersion = 0;
    constexpr unsigned kAbiSlotMetaWords = 1;
    constexpr unsigned kAbiSlotMetaWordCount = 2;
    constexpr unsigned kAbiProcessMetaWords = 3;
    constexpr unsigned kAbiProcessMetaWordCount = 4;
    constexpr unsigned kAbiProcessMetaStringPool = 5;
    constexpr unsigned kAbiProcessMetaStringPoolSize = 6;
    constexpr unsigned kAbiLoopSiteMetaWords = 7;
    constexpr unsigned kAbiLoopSiteMetaWordCount = 8;
    constexpr unsigned kAbiLoopSiteMetaStringPool = 9;
    constexpr unsigned kAbiLoopSiteMetaStringPoolSize = 10;
    constexpr unsigned kAbiConnDescs = 11;
    constexpr unsigned kAbiNumConnDescs = 12;
    constexpr unsigned kAbiCombKernelWords = 13;
    constexpr unsigned kAbiNumCombKernelWords = 14;
    constexpr unsigned kAbiFeatureFlags = 15;
    constexpr unsigned kAbiWaitSiteWords = 16;
    constexpr unsigned kAbiWaitSiteWordCount = 17;
    constexpr unsigned kAbiFieldCount = 18;

    // Build RuntimeAbi struct on the stack
    std::array<llvm::Type*, kAbiFieldCount> abi_fields = {
        i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty,
        ptr_ty, i32_ty, ptr_ty, i32_ty, ptr_ty, i32_ty, i32_ty, ptr_ty, i32_ty,
    };
    auto* abi_struct_type = llvm::StructType::get(ctx, abi_fields, false);

    auto* abi_alloca = builder.CreateAlloca(abi_struct_type, nullptr, "abi");

    auto store_field = [&](unsigned idx, llvm::Value* val) {
      auto* gep = builder.CreateStructGEP(abi_struct_type, abi_alloca, idx);
      builder.CreateStore(val, gep);
    };

    store_field(
        kAbiVersion, llvm::ConstantInt::get(i32_ty, kRuntimeAbiVersion));
    store_field(kAbiSlotMetaWords, meta_globals.slot_meta_words);
    store_field(
        kAbiSlotMetaWordCount,
        llvm::ConstantInt::get(i32_ty, meta_globals.slot_meta_count));
    store_field(kAbiProcessMetaWords, meta_globals.process_meta_words);
    store_field(
        kAbiProcessMetaWordCount,
        llvm::ConstantInt::get(i32_ty, meta_globals.process_meta_count));
    store_field(kAbiProcessMetaStringPool, meta_globals.process_meta_pool);
    store_field(
        kAbiProcessMetaStringPoolSize,
        llvm::ConstantInt::get(i32_ty, meta_globals.process_meta_pool_size));
    store_field(kAbiLoopSiteMetaWords, meta_globals.loop_site_meta_words);
    store_field(
        kAbiLoopSiteMetaWordCount,
        llvm::ConstantInt::get(i32_ty, meta_globals.loop_site_meta_count));
    store_field(kAbiLoopSiteMetaStringPool, meta_globals.loop_site_meta_pool);
    store_field(
        kAbiLoopSiteMetaStringPoolSize,
        llvm::ConstantInt::get(i32_ty, meta_globals.loop_site_meta_pool_size));
    store_field(kAbiConnDescs, meta_globals.conn_desc_table);
    store_field(
        kAbiNumConnDescs,
        llvm::ConstantInt::get(i32_ty, meta_globals.conn_desc_count));
    store_field(kAbiCombKernelWords, meta_globals.comb_kernel_words);
    store_field(
        kAbiNumCombKernelWords,
        llvm::ConstantInt::get(i32_ty, meta_globals.comb_kernel_word_count));
    store_field(
        kAbiFeatureFlags, llvm::ConstantInt::get(i32_ty, input.feature_flags));
    store_field(kAbiWaitSiteWords, wait_site_meta.words_ptr);
    store_field(
        kAbiWaitSiteWordCount,
        llvm::ConstantInt::get(i32_ty, wait_site_meta.count));

    // Call multi-process scheduler
    builder.CreateCall(
        context.GetLyraRunSimulation(),
        {funcs_array, states_array,
         llvm::ConstantInt::get(i32_ty, num_module_processes), plusargs_array,
         num_plusargs_val, meta_globals.instance_paths_array,
         llvm::ConstantInt::get(i32_ty, meta_globals.instance_path_count),
         abi_alloca});
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
