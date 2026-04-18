#include "lyra/llvm_backend/lower.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/llvm_backend/behavioral_trigger_contracts.hpp"
#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/connection_lowering.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/dpi_abi.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/layout_pipeline.hpp"
#include "lyra/llvm_backend/process.hpp"
#include "lyra/llvm_backend/runtime_abi_codegen.hpp"
#include "lyra/llvm_backend/runtime_data_extraction.hpp"
#include "lyra/llvm_backend/spec_planning.hpp"
#include "lyra/llvm_backend/spec_session.hpp"
#include "lyra/llvm_backend/target_policy.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

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
  std::vector<common::OriginId> back_edge_origins;
  std::vector<ConnectionTriggerWriteback> connection_trigger_writebacks;
};

auto CompileStandaloneProcesses(
    Context& context, const CuFacts& facts, const mir::Arena* mir_arena,
    std::span<const LayoutModulePlan> module_plans,
    std::span<const ScheduledProcess> scheduled_processes,
    std::span<const ProcessLayout> process_layouts, size_t num_init_processes,
    size_t num_module_process_base,
    std::span<const Layout::ConnectionRealizationInfo> connection_infos,
    uint32_t wait_site_base, uint32_t back_edge_site_base)
    -> Result<StandaloneProcessProducts> {
  StandaloneProcessProducts products;
  products.process_funcs.reserve(scheduled_processes.size());
  uint32_t conn_ordinal = 0;

  for (size_t i = 0; i < scheduled_processes.size(); ++i) {
    context.SetCurrentProcess(&process_layouts[i]);

    const auto& bp = scheduled_processes[i];
    const mir::Arena& proc_arena =
        (bp.module_index.value < module_plans.size())
            ? module_plans[bp.module_index.value].body->arena
            : *mir_arena;
    const auto& mir_process = proc_arena[bp.process_id];

    if (i < num_init_processes || bp.module_index.value == ModuleIndex::kNone) {
      Context::ArenaScope arena_scope(context, mir_arena);
      auto execution_kind = (i < num_init_processes)
                                ? ProcessExecutionKind::kInit
                                : ProcessExecutionKind::kSimulation;
      BodySiteContext standalone_sites{
          .wait_site_base = wait_site_base +
                            static_cast<uint32_t>(products.wait_sites.size()),
          .back_edge_site_base =
              back_edge_site_base +
              static_cast<uint32_t>(products.back_edge_origins.size()),
          .deferred_sites = {},
      };
      auto func_result = GenerateProcessFunction(
          context, facts, mir_process, std::format("process_{}", i),
          execution_kind, standalone_sites);
      if (!func_result) return std::unexpected(func_result.error());
      products.process_funcs.push_back(func_result->function);
      products.wait_sites.insert(
          products.wait_sites.end(),
          std::make_move_iterator(func_result->wait_sites.begin()),
          std::make_move_iterator(func_result->wait_sites.end()));
      products.back_edge_origins.insert(
          products.back_edge_origins.end(),
          std::make_move_iterator(func_result->back_edge_origins.begin()),
          std::make_move_iterator(func_result->back_edge_origins.end()));

      // Collect connection trigger facts as writebacks.
      if (i >= num_init_processes && i < num_module_process_base) {
        if (conn_ordinal >= connection_infos.size()) {
          throw common::InternalError(
              "CompileStandaloneProcesses",
              std::format(
                  "connection ordinal {} >= "
                  "connection_realization_infos size {}",
                  conn_ordinal, connection_infos.size()));
        }
        const auto& conn_info = connection_infos[conn_ordinal];
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
            if (fact.signal.scope != mir::SignalRef::Scope::kDesignGlobal &&
                fact.signal.scope != mir::SignalRef::Scope::kObjectLocal) {
              throw common::InternalError(
                  "CompileStandaloneProcesses",
                  std::format(
                      "connection ordinal {} trigger signal has unexpected "
                      "scope (scope={}, id={})",
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
    std::span<const mir::DesignElement> design_elements,
    std::span<const Layout::BodyRealizationInfo> body_realization_infos,
    std::span<const Layout::BodyRuntimeDescriptors> body_runtime_descriptors,
    uint32_t num_instances,
    std::vector<std::vector<llvm::Function*>> body_compiled_funcs,
    std::vector<std::vector<llvm::Function*>> body_ic_fns) -> FinalPackaging {
  if (body_compiled_funcs.size() != body_realization_infos.size()) {
    throw common::InternalError(
        "BuildFinalPackaging",
        std::format(
            "body_compiled_funcs size {} != body_realization_infos size {}",
            body_compiled_funcs.size(), body_realization_infos.size()));
  }

  FinalPackaging pkg;

  for (size_t bi = 0; bi < body_realization_infos.size(); ++bi) {
    const auto& rt = body_runtime_descriptors[bi];
    if (body_compiled_funcs[bi].size() != rt.process_schema_indices.size()) {
      throw common::InternalError(
          "BuildFinalPackaging",
          std::format(
              "body group {} compiled function count {} != schema count {}", bi,
              body_compiled_funcs[bi].size(),
              rt.process_schema_indices.size()));
    }
    pkg.body_funcs.push_back(
        CodegenSession::BodyCompiledFuncs{
            .functions = std::move(body_compiled_funcs[bi]),
            .installable_computation_fns = bi < body_ic_fns.size()
                                               ? std::move(body_ic_fns[bi])
                                               : std::vector<llvm::Function*>{},
        });
  }

  std::unordered_map<const mir::ModuleBody*, uint32_t> body_to_group;
  for (size_t gi = 0; gi < body_realization_infos.size(); ++gi) {
    body_to_group[body_realization_infos[gi].body] = static_cast<uint32_t>(gi);
  }
  {
    uint32_t mi = 0;
    for (const auto& element : design_elements) {
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
  if (pkg.instance_body_group.size() != num_instances) {
    throw common::InternalError(
        "BuildFinalPackaging",
        std::format(
            "instance_body_group size {} != instance count {}",
            pkg.instance_body_group.size(), num_instances));
  }

  return pkg;
}

}  // namespace

auto CompileDesignProcesses(const LoweringInput& input)
    -> Result<CodegenSession> {
  // Backend setup.
  auto llvm_ctx = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("lyra_module", *llvm_ctx);
  SetModuleDataLayout(*module);

  // Topology + layout.
  auto topology = BuildTopologyPlan(input);
  auto connections = LowerConnectionArtifacts(input);
  auto layout = BuildBackendLayout(
      input, topology, connections, *llvm_ctx, module->getDataLayout());

  PopulateBehavioralTriggerContracts(
      topology.module_plans, *input.design, *input.mir_arena,
      *input.construction, *layout);

  auto facts = std::make_unique<CuFacts>(CuFacts{
      .design_arena = input.mir_arena,
      .types = input.type_arena,
      .layout = layout.get(),
      .force_two_state = input.force_two_state,
      .source_manager = input.source_manager,
  });

  auto context = std::make_unique<Context>(
      *input.mir_arena, *facts, std::move(llvm_ctx), std::move(module),
      input.diag_ctx);

  // Specialization planning + compilation.
  std::span<const mir::DpiExportWrapperDesc> dpi_exports;
  if (input.dpi_export_wrappers != nullptr) {
    dpi_exports = *input.dpi_export_wrappers;
  }
  auto spec_plan = BuildSpecPlan(
      *input.design, *layout, topology.module_plans, input.origin_provenance,
      dpi_exports);

  auto globals_result = CompileGlobalFunctions(*context, *facts, input);
  if (!globals_result) return std::unexpected(globals_result.error());

  auto specs_result =
      CompileSpecializations(*context, *facts, input, spec_plan);
  if (!specs_result) return std::unexpected(specs_result.error());
  auto specs = std::move(*specs_result);

  if (input.dpi_export_wrappers != nullptr &&
      !input.dpi_export_wrappers->empty()) {
    auto export_result = dpi::EmitDpiExportWrappers(
        *context, *facts, *input.dpi_export_wrappers,
        specs.module_export_callees);
    if (!export_result) return std::unexpected(export_result.error());
  }

  auto standalone_result = CompileStandaloneProcesses(
      *context, *facts, input.mir_arena, topology.module_plans,
      layout->scheduled_processes, layout->processes,
      layout->num_init_processes, layout->num_module_process_base,
      layout->connection_realization_infos,
      static_cast<uint32_t>(specs.wait_sites.size()),
      static_cast<uint32_t>(specs.back_edge_origins.size()));
  if (!standalone_result) return std::unexpected(standalone_result.error());
  auto standalone = std::move(*standalone_result);
  ApplyConnectionTriggerWritebacks(
      standalone.connection_trigger_writebacks, *layout);

  // Final packaging + runtime data extraction.
  if (input.construction->objects.size() !=
      input.construction->const_blocks.size()) {
    throw common::InternalError(
        "CompileDesignProcesses",
        std::format(
            "construction objects/const_blocks size mismatch: {} vs {}",
            input.construction->objects.size(),
            input.construction->const_blocks.size()));
  }
  auto packaging = BuildFinalPackaging(
      input.design->elements, layout->body_realization_infos,
      layout->body_runtime_descriptors,
      static_cast<uint32_t>(layout->instance_storage_bases.size()),
      std::move(specs.body_compiled_funcs), std::move(specs.body_ic_fns));

  auto runtime_products = ExtractRuntimeData(
      input, *layout, specs.body_process_triggers,
      packaging.instance_body_group);
  RealizationData realization;
  ApplyRuntimeDataToLayout(std::move(runtime_products), *layout, realization);

  // Assemble session: merge spec + standalone wait-sites and back-edge origins.
  auto num_init = layout->num_init_processes;
  std::vector<WaitSiteEntry> all_wait_sites;
  all_wait_sites.insert(
      all_wait_sites.end(), std::make_move_iterator(specs.wait_sites.begin()),
      std::make_move_iterator(specs.wait_sites.end()));
  all_wait_sites.insert(
      all_wait_sites.end(),
      std::make_move_iterator(standalone.wait_sites.begin()),
      std::make_move_iterator(standalone.wait_sites.end()));

  std::vector<common::OriginId> all_back_edge_origins;
  all_back_edge_origins.insert(
      all_back_edge_origins.end(),
      std::make_move_iterator(specs.back_edge_origins.begin()),
      std::make_move_iterator(specs.back_edge_origins.end()));
  all_back_edge_origins.insert(
      all_back_edge_origins.end(),
      std::make_move_iterator(standalone.back_edge_origins.begin()),
      std::make_move_iterator(standalone.back_edge_origins.end()));

  return CodegenSession{
      .layout = std::move(layout),
      .facts = std::move(facts),
      .context = std::move(context),
      .realization = std::move(realization),
      .process_funcs = std::move(standalone.process_funcs),
      .wait_sites = std::move(all_wait_sites),
      .num_init_processes = num_init,
      .body_compiled_funcs = std::move(packaging.body_funcs),
      .deferred_site_artifacts = std::move(specs.deferred_site_artifacts),
      .back_edge_origins = std::move(all_back_edge_origins),
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
    Context& context, const CuFacts& facts, const InspectionPlan& plan,
    llvm::Value* design_state, llvm::Value* abi_ptr,
    llvm::Value* run_session_ptr) {
  if (plan.IsEmpty()) {
    return;
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  auto* buf = builder.CreateCall(
      context.GetLyraCreateVarSnapshotBuffer(), {}, "varbuf");

  auto emit_register = [&](llvm::Value* slot_ptr, std::string_view name,
                           const SlotTypeInfo& type_info) {
    auto* name_ptr = builder.CreateGlobalStringPtr(name);
    auto* kind_val =
        llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(type_info.kind));
    auto* width_val = llvm::ConstantInt::get(i32_ty, type_info.width);
    auto* signed_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_signed ? 1 : 0);
    auto* four_state_val =
        llvm::ConstantInt::get(i1_ty, type_info.is_four_state ? 1 : 0);
    builder.CreateCall(
        context.GetLyraRegisterVar(), {buf, name_ptr, slot_ptr, kind_val,
                                       width_val, signed_val, four_state_val});
  };

  for (const auto& var : plan.globals) {
    auto* addr = builder.CreateGEP(
        i8_ty, design_state, builder.getInt64(var.placement.abs_off.value),
        "var_ptr");
    emit_register(addr, var.name, var.type_info);
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
      auto* addr = EmitInstanceOwnedByteAddress(
          context, facts, inst, var.placement.rel_off);
      emit_register(addr, var.name, var.type_info);
    }
    builder.CreateBr(skip_bb);
    builder.SetInsertPoint(skip_bb);
  }

  builder.CreateCall(context.GetLyraSnapshotVars(), {buf, run_session_ptr});
}

void EmitTimeReport(Context& context, llvm::Value* run_session_ptr) {
  context.GetBuilder().CreateCall(
      context.GetLyraReportTime(), {run_session_ptr});
}

auto DumpLlvmIr(const llvm::Module& module) -> std::string {
  std::string ir;
  llvm::raw_string_ostream stream(ir);
  module.print(stream, nullptr);
  return ir;
}

}  // namespace lyra::lowering::mir_to_llvm
