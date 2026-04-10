#include "lyra/llvm_backend/layout_pipeline.hpp"

#include <cstdint>
#include <format>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/connection_analysis.hpp"
#include "lyra/llvm_backend/lower.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_llvm {

auto BuildTopologyPlan(const LoweringInput& input) -> TopologyPlan {
  TopologyPlan topo;

  uint32_t module_idx = 0;
  for (const auto& element : input.design->elements) {
    const auto* mod = std::get_if<mir::Module>(&element);
    if (mod == nullptr) continue;
    const auto& body = *mod->body;
    const auto& obj = input.construction->objects.at(module_idx);
    topo.module_plans.push_back(
        LayoutModulePlan{
            .body_processes = body.processes,
            .body = mod->body,
            .design_state_base_slot = obj.design_state_base_slot,
            .slot_count = obj.slot_count,
        });
    ++module_idx;
  }
  if (module_idx != input.construction->objects.size()) {
    throw common::InternalError(
        "BuildTopologyPlan",
        std::format(
            "module count {} from elements does not match construction "
            "object count {}",
            module_idx, input.construction->objects.size()));
  }

  topo.total_design_slot_count =
      static_cast<uint32_t>(input.design->slots.size());
  for (const auto& plan : topo.module_plans) {
    topo.total_design_slot_count += plan.slot_count;
  }

  return topo;
}

auto BuildBackendLayout(
    const LoweringInput& input, const TopologyPlan& topology,
    LoweredConnectionArtifacts& connections, llvm::LLVMContext& llvm_ctx,
    const llvm::DataLayout& data_layout) -> std::unique_ptr<Layout> {
  // Instance slot ranges for per-instance byte-offset stamping.
  std::vector<InstanceSlotRange> instance_ranges;
  instance_ranges.reserve(topology.module_plans.size());
  for (const auto& plan : topology.module_plans) {
    instance_ranges.push_back(
        InstanceSlotRange{
            .base_slot = plan.design_state_base_slot,
            .slot_count = plan.slot_count,
            .body_slots = plan.body->slots,
        });
  }

  // Per-body storage layouts from body-local MIR inputs.
  std::unordered_map<uint32_t, BodyStorageLayout> body_storage_layouts;
  for (uint32_t bi = 0; bi < input.design->module_bodies.size(); ++bi) {
    const auto& body = input.design->module_bodies[bi];
    if (body.slots.empty()) continue;
    body_storage_layouts.emplace(
        bi, BuildBodyStorageLayout(
                body, *input.type_arena, data_layout, input.force_two_state));
  }

  auto connection_analysis = AnalyzeConnections(
      std::move(connections.kernel_entries), topology.module_plans,
      *input.design, *input.mir_arena, topology.total_design_slot_count);

  auto design_layout = BuildDesignLayout(
      input.design->slots, *input.type_arena, data_layout,
      input.force_two_state, instance_ranges);

  return std::make_unique<Layout>(BuildLayout(
      input.design->init_processes,
      std::move(connection_analysis.connection_edges),
      std::move(connections.non_kernelized_processes), topology.module_plans,
      *input.design, *input.mir_arena, *input.type_arena,
      std::move(design_layout), body_storage_layouts, input.body_timescales,
      llvm_ctx, data_layout, input.force_two_state));
}

}  // namespace lyra::lowering::mir_to_llvm
