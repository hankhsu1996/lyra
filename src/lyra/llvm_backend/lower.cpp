#include "lyra/llvm_backend/lower.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
#include <memory>
#include <mutex>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/SubtargetFeature.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/behavioral_trigger_contracts.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/connection_lowering.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/dpi_abi.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout_pipeline.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/runtime_abi_codegen.hpp"
#include "lyra/llvm_backend/runtime_data_extraction.hpp"
#include "lyra/llvm_backend/spec_planning.hpp"
#include "lyra/llvm_backend/spec_session.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void InitializeLlvmTargets() {
  static std::once_flag flag;
  std::call_once(flag, [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
  });
}

void SetHostDataLayout(llvm::Module& module) {
  InitializeLlvmTargets();

  std::string triple = llvm::sys::getDefaultTargetTriple();
  std::string cpu = llvm::sys::getHostCPUName().str();

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

// Connection trigger writeback entry: collected during standalone process
// compilation, applied to layout in one narrow pass.
struct ConnectionTriggerWriteback {
  uint32_t ordinal;
  Layout::ConnectionTriggerResult result;
};

// All products from standalone (init + connection) process compilation.
struct StandaloneProcessProducts {
  std::vector<llvm::Function*> process_funcs;
  std::vector<WaitSiteEntry> wait_sites;
  std::vector<ConnectionTriggerWriteback> connection_trigger_writebacks;
};

auto CompileStandaloneProcesses(
    Context& context, const LoweringInput& input,
    std::span<const LayoutModulePlan> module_plans, const Layout& layout)
    -> Result<StandaloneProcessProducts> {
  StandaloneProcessProducts products;
  products.process_funcs.reserve(layout.scheduled_processes.size());
  uint32_t conn_ordinal = 0;

  size_t num_init = layout.num_init_processes;
  for (size_t i = 0; i < layout.scheduled_processes.size(); ++i) {
    context.SetCurrentProcess(i);

    const auto& bp = layout.scheduled_processes[i];
    const mir::Arena& proc_arena =
        (bp.module_index.value < module_plans.size())
            ? module_plans[bp.module_index.value].body->arena
            : *input.mir_arena;
    const auto& mir_process = proc_arena[bp.process_id];

    if (i < num_init || bp.module_index.value == ModuleIndex::kNone) {
      Context::ArenaScope arena_scope(context, input.mir_arena);
      auto execution_kind = (i < num_init) ? ProcessExecutionKind::kInit
                                           : ProcessExecutionKind::kSimulation;
      BodySiteContext no_sites;
      auto func_result = GenerateProcessFunction(
          context, mir_process, std::format("process_{}", i), execution_kind,
          no_sites);
      if (!func_result) return std::unexpected(func_result.error());
      products.process_funcs.push_back(func_result->function);
      products.wait_sites.insert(
          products.wait_sites.end(),
          std::make_move_iterator(func_result->wait_sites.begin()),
          std::make_move_iterator(func_result->wait_sites.end()));

      // Collect connection trigger facts as writebacks.
      if (i >= num_init && i < layout.num_module_process_base) {
        if (conn_ordinal >= layout.connection_realization_infos.size()) {
          throw common::InternalError(
              "CompileStandaloneProcesses",
              std::format(
                  "connection ordinal {} >= "
                  "connection_realization_infos size {}",
                  conn_ordinal, layout.connection_realization_infos.size()));
        }
        const auto& conn_info =
            layout.connection_realization_infos[conn_ordinal];
        if (bp.process_id != conn_info.process_id) {
          throw common::InternalError(
              "CompileStandaloneProcesses",
              std::format(
                  "connection ordinal {} process_id mismatch: "
                  "scheduled {} != realization {}",
                  conn_ordinal, bp.process_id.value,
                  conn_info.process_id.value));
        }
        if (func_result->process_trigger) {
          const auto& pt = *func_result->process_trigger;
          Layout::ConnectionTriggerResult trigger_result;
          trigger_result.shape = pt.shape;
          trigger_result.triggers.reserve(pt.triggers.size());
          for (const auto& fact : pt.triggers) {
            if (fact.signal.scope != mir::SignalRef::Scope::kDesignGlobal) {
              throw common::InternalError(
                  "CompileStandaloneProcesses",
                  std::format(
                      "connection ordinal {} trigger signal is not "
                      "design-global (scope={}, id={})",
                      conn_ordinal, static_cast<int>(fact.signal.scope),
                      fact.signal.id));
            }
            trigger_result.triggers.push_back(
                Layout::ConnectionTriggerFact{
                    .signal = fact.signal,
                    .edge = fact.edge,
                    .has_observed_place = fact.has_observed_place,
                });
          }
          products.connection_trigger_writebacks.push_back(
              ConnectionTriggerWriteback{
                  .ordinal = conn_ordinal,
                  .result = std::move(trigger_result),
              });
        }
        ++conn_ordinal;
      }
      continue;
    }

    // Module processes dispatch through descriptors, not this array.
    products.process_funcs.push_back(nullptr);
  }

  return products;
}

void ApplyConnectionTriggerWritebacks(
    std::span<const ConnectionTriggerWriteback> writebacks, Layout& layout) {
  for (const auto& wb : writebacks) {
    layout.connection_realization_infos[wb.ordinal].trigger = wb.result;
  }
}

// Final packaging: body compiled funcs and instance body group.
struct FinalPackaging {
  std::vector<CodegenSession::BodyCompiledFuncs> body_funcs;
  std::vector<uint32_t> instance_body_group;
};

auto BuildFinalPackaging(
    const mir::Design& design, const mir::ConstructionInput& construction,
    const Layout& layout,
    std::unordered_map<const mir::ModuleBody*, std::vector<llvm::Function*>>
        body_to_compiled_funcs) -> FinalPackaging {
  if (construction.objects.size() != construction.const_blocks.size()) {
    throw common::InternalError(
        "BuildFinalPackaging",
        std::format(
            "construction objects/const_blocks size mismatch: {} vs {}",
            construction.objects.size(), construction.const_blocks.size()));
  }

  FinalPackaging pkg;

  for (const auto& info : layout.body_realization_infos) {
    auto it = body_to_compiled_funcs.find(info.body);
    if (it == body_to_compiled_funcs.end()) {
      throw common::InternalError(
          "BuildFinalPackaging",
          std::format("no compiled functions for body {}", info.body_id.value));
    }
    if (it->second.size() != info.process_schema_indices.size()) {
      throw common::InternalError(
          "BuildFinalPackaging",
          std::format(
              "body {} compiled function count {} != schema count {}",
              info.body_id.value, it->second.size(),
              info.process_schema_indices.size()));
    }
    pkg.body_funcs.push_back(
        CodegenSession::BodyCompiledFuncs{
            .functions = std::move(it->second),
        });
  }

  std::unordered_map<const mir::ModuleBody*, uint32_t> body_to_group;
  for (size_t gi = 0; gi < layout.body_realization_infos.size(); ++gi) {
    body_to_group[layout.body_realization_infos[gi].body] =
        static_cast<uint32_t>(gi);
  }
  {
    uint32_t mi = 0;
    for (const auto& element : design.elements) {
      const auto* mod = std::get_if<mir::Module>(&element);
      if (mod == nullptr) continue;
      auto it = body_to_group.find(mod->body);
      if (it == body_to_group.end()) {
        throw common::InternalError(
            "BuildFinalPackaging",
            std::format("instance {} body not in body_realization_infos", mi));
      }
      pkg.instance_body_group.push_back(it->second);
      ++mi;
    }
  }
  if (pkg.instance_body_group.size() != layout.instance_storage_bases.size()) {
    throw common::InternalError(
        "BuildFinalPackaging",
        std::format(
            "instance_body_group size {} != instance count {}",
            pkg.instance_body_group.size(),
            layout.instance_storage_bases.size()));
  }

  return pkg;
}

void VerifyLoweringInput(const LoweringInput& input) {
  if (input.body_timescales == nullptr) {
    throw common::InternalError(
        "CompileDesignProcesses", "body_timescales must be non-null");
  }
  size_t expected = 0;
  for (const auto& body : input.design->module_bodies) {
    expected += body.deferred_assertion_sites.size();
  }
  if (expected != input.design->deferred_assertion_sites.size()) {
    throw common::InternalError(
        "CompileDesignProcesses",
        std::format(
            "deferred site concatenation invariant violated: "
            "sum of body-local counts ({}) != design-global count ({})",
            expected, input.design->deferred_assertion_sites.size()));
  }
}

}  // namespace

auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession> {
  VerifyLoweringInput(input);

  // Backend setup.
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);
  SetHostDataLayout(*module);

  // Topology + layout.
  auto topology = BuildTopologyPlan(input);
  auto connections = LowerConnectionArtifacts(input);
  auto layout = BuildBackendLayout(
      input, topology, connections, *llvm_ctx, module->getDataLayout());

  PopulateBehavioralTriggerContracts(
      topology.module_plans, *input.design, *input.mir_arena,
      input.construction, *layout);

  auto context = std::make_unique<Context>(
      *input.mir_arena, *input.type_arena, *layout, std::move(llvm_ctx),
      std::move(module), input.diag_ctx, input.source_manager,
      input.force_two_state);

  // Specialization planning + compilation.
  auto spec_plan = BuildSpecPlan(
      *input.design, *layout, topology.module_plans, input.origin_provenance);

  auto globals_result = CompileGlobalFunctions(*context, input);
  if (!globals_result) return std::unexpected(globals_result.error());

  auto specs_result = CompileSpecializations(*context, input, spec_plan);
  if (!specs_result) return std::unexpected(specs_result.error());
  auto specs = std::move(*specs_result);

  if (input.dpi_export_wrappers != nullptr &&
      !input.dpi_export_wrappers->empty()) {
    auto export_result = dpi::EmitDpiExportWrappers(
        *context, *input.dpi_export_wrappers, specs.module_export_callees);
    if (!export_result) return std::unexpected(export_result.error());
  }

  const auto& deferred_sites = input.design->deferred_assertion_sites;
  auto deferred_result = CompileDeferredAssertionArtifacts(
      *context, deferred_sites, specs.deferred_site_callee_info);
  if (!deferred_result) return std::unexpected(deferred_result.error());
  if (deferred_result->size() != deferred_sites.size()) {
    throw common::InternalError(
        "CompileDesignProcesses",
        std::format(
            "deferred artifacts count {} != sites count {}",
            deferred_result->size(), deferred_sites.size()));
  }

  auto standalone_result = CompileStandaloneProcesses(
      *context, input, topology.module_plans, *layout);
  if (!standalone_result) return std::unexpected(standalone_result.error());
  auto standalone = std::move(*standalone_result);
  ApplyConnectionTriggerWritebacks(
      standalone.connection_trigger_writebacks, *layout);

  // Final packaging + runtime data extraction.
  auto packaging = BuildFinalPackaging(
      *input.design, *input.construction, *layout,
      std::move(specs.body_to_compiled_funcs));

  auto runtime_products = ExtractRuntimeData(
      input, *layout, topology.module_plans, specs.body_to_process_triggers,
      packaging.instance_body_group);
  RealizationData realization;
  ApplyRuntimeDataToLayout(std::move(runtime_products), *layout, realization);

  // Assemble session.
  auto num_init = layout->num_init_processes;
  std::vector<WaitSiteEntry> all_wait_sites;
  all_wait_sites.insert(
      all_wait_sites.end(), std::make_move_iterator(specs.wait_sites.begin()),
      std::make_move_iterator(specs.wait_sites.end()));
  all_wait_sites.insert(
      all_wait_sites.end(),
      std::make_move_iterator(standalone.wait_sites.begin()),
      std::make_move_iterator(standalone.wait_sites.end()));

  return CodegenSession{
      .layout = std::move(layout),
      .context = std::move(context),
      .realization = std::move(realization),
      .process_funcs = std::move(standalone.process_funcs),
      .wait_sites = std::move(all_wait_sites),
      .num_init_processes = num_init,
      .body_compiled_funcs = std::move(packaging.body_funcs),
      .deferred_site_artifacts = std::move(*deferred_result),
  };
}

auto FinalizeModule(CodegenSession session, LoweringReport report)
    -> LoweringResult {
  auto [result_ctx, result_mod] = session.context->TakeOwnership();
  return LoweringResult{
      .context = std::move(result_ctx),
      .module = std::move(result_mod),
      .report = std::move(report),
  };
}

void EmitVariableInspection(
    Context& context, const InspectionPlan& plan, const mir::Design& design,
    llvm::Value* design_state, llvm::Value* abi_ptr) {
  if (plan.IsEmpty()) {
    return;
  }

  const auto& layout = context.GetLayout();
  const auto& types = context.GetTypeArena();
  bool force_two_state = context.IsForceTwoState();

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  auto classify_slot = [&](common::SlotId slot_id) -> SlotTypeInfo {
    uint32_t gsi = slot_id.value;
    if (gsi < layout.num_package_slots) {
      return ClassifySlotTypeInfo(
          design.slots[gsi].type, types, force_two_state);
    }
    auto owner = ResolveInstanceOwnedFlatSlot(layout, gsi);
    auto body_id = layout.instance_body_ids[owner.instance_id.value];
    const auto& body = design.module_bodies.at(body_id.value);
    return ClassifySlotTypeInfo(
        body.slots[owner.local_signal_id.value].type, types, force_two_state);
  };

  auto emit_register = [&](llvm::Value* slot_ptr, std::string_view name,
                           common::SlotId slot_id) {
    const auto type_info = classify_slot(slot_id);
    auto* name_ptr = builder.CreateGlobalStringPtr(name);
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
  };

  for (const auto& var : plan.globals) {
    auto* addr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(var.placement.abs_off.value),
        "var_ptr");
    emit_register(addr, var.name, var.slot_id);
  }

  if (!plan.instance_owned.empty()) {
    auto* fn = builder.GetInsertBlock()->getParent();
    auto* with_abi_bb = llvm::BasicBlock::Create(llvm_ctx, "inspect.inst", fn);
    auto* skip_bb = llvm::BasicBlock::Create(llvm_ctx, "inspect.inst.done", fn);

    auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
    auto* null_ptr_val =
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(ptr_ty));
    auto* has_abi = builder.CreateICmpNE(abi_ptr, null_ptr_val, "has_abi");
    builder.CreateCondBr(has_abi, with_abi_bb, skip_bb);

    builder.SetInsertPoint(with_abi_bb);
    for (const auto& var : plan.instance_owned) {
      auto* inst = EmitLoadAbiInstancePtr(
          context, abi_ptr, var.placement.owner_instance_id);
      auto* addr =
          EmitInstanceOwnedByteAddress(context, inst, var.placement.rel_off);
      emit_register(addr, var.name, var.slot_id);
    }
    builder.CreateBr(skip_bb);
    builder.SetInsertPoint(skip_bb);
  }

  builder.CreateCall(context.GetLyraSnapshotVars());
}

void EmitTimeReport(Context& context) {
  context.GetBuilder().CreateCall(context.GetLyraReportTime());
}

auto DumpLlvmIr(const LoweringResult& result) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  result.module->print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
