#include <cstdint>
#include <expected>
#include <format>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/placement.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

template <class>
inline constexpr bool kAlwaysFalse = false;

using common::ModuleSpecIdHash;

// Deterministic specialization group for MIR lowering.
// representative_module_index is the first instance in design order for this
// spec. All members share the same ModuleDefId and behavioral fingerprint.
struct MirSpecGroup {
  common::ModuleSpecId spec_id;
  uint32_t representative_module_index;
  std::vector<uint32_t> member_module_indices;
};

// Build deterministic specialization groups from the SpecializationMap.
// Group order follows SpecializationMap::groups (sorted by spec_id).
// Representative is the first instance in each group (lowest module_index).
//
// Validates invariants that the specialization map must uphold:
//   - No empty groups
//   - All member indices in range
//   - No duplicate member indices across groups
//   - Representative is the first member
//   - spec_id_by_instance agrees with group membership
auto BuildMirSpecGroups(
    const common::SpecializationMap& specialization_map,
    uint32_t num_hir_modules) -> std::vector<MirSpecGroup> {
  // Track seen indices to detect duplicates across groups.
  std::vector<bool> seen(num_hir_modules, false);

  std::vector<MirSpecGroup> result;
  result.reserve(specialization_map.groups.size());
  for (const auto& group : specialization_map.groups) {
    if (group.instance_indices.empty()) continue;

    for (uint32_t idx : group.instance_indices) {
      if (idx >= num_hir_modules) {
        throw common::InternalError(
            "BuildMirSpecGroups",
            std::format(
                "member index {} out of range (num_modules={})", idx,
                num_hir_modules));
      }
      if (seen[idx]) {
        throw common::InternalError(
            "BuildMirSpecGroups",
            std::format("duplicate member index {}", idx));
      }
      seen[idx] = true;

      if (idx >= specialization_map.spec_id_by_instance.size() ||
          !(specialization_map.spec_id_by_instance[idx] == group.spec_id)) {
        throw common::InternalError(
            "BuildMirSpecGroups",
            std::format(
                "spec_id_by_instance[{}] disagrees with group spec_id", idx));
      }
    }

    MirSpecGroup mir_group;
    mir_group.spec_id = group.spec_id;
    mir_group.representative_module_index = group.instance_indices[0];
    mir_group.member_module_indices.assign(
        group.instance_indices.begin(), group.instance_indices.end());
    result.push_back(std::move(mir_group));
  }

  // Coverage check: every HIR module index must appear in exactly one group.
  for (uint32_t i = 0; i < num_hir_modules; ++i) {
    if (!seen[i]) {
      throw common::InternalError(
          "BuildMirSpecGroups",
          std::format(
              "module index {} not covered by any specialization group", i));
    }
  }

  return result;
}

}  // namespace

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map)
    -> Result<DesignLoweringResult> {
  const DesignDeclarations decls =
      CollectDesignDeclarations(design, input, mir_arena);

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slots = decls.slots;
  result.slot_trace_provenance = decls.slot_trace_provenance;
  result.slot_trace_string_pool = decls.slot_trace_string_pool;

  if (result.slot_trace_provenance.size() != result.slots.size()) {
    throw common::InternalError(
        "LowerDesign",
        std::format(
            "slot_trace_provenance.size() ({}) != slots.size() ({})",
            result.slot_trace_provenance.size(), result.slots.size()));
  }
  if (result.slot_trace_string_pool.empty() ||
      result.slot_trace_string_pool[0] != '\0') {
    throw common::InternalError(
        "LowerDesign",
        "slot_trace_string_pool must be non-empty and start with '\\0'");
  }
  result.global_precision_power = input.global_precision_power;
  result.module_def_ids = decls.module_def_ids;
  if (input.instance_table != nullptr) {
    result.instance_table = *input.instance_table;
  }

  // Lower package init processes
  // Collect dynamically generated functions (e.g., observer programs)
  DeclView init_view{
      .design_places = &decls.design_places,
      .functions = &decls.functions,
      .slots = &decls.slots};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        hir::ProcessId hir_proc_id = pkg->init_process;
        const hir::Process& proc = (*input.hir_arena)[hir_proc_id];
        Result<mir::ProcessId> mir_proc_result = LowerProcess(
            hir_proc_id, proc, input, mir_arena, init_view, origin_map,
            &result.generated_functions, hir::kInvalidModuleBodyId);
        if (!mir_proc_result) {
          return std::unexpected(mir_proc_result.error());
        }
        result.init_processes.push_back(*mir_proc_result);
      }
    }
  }

  // Collect HIR modules in element order (parallel to module_index)
  std::vector<const hir::Module*> hir_modules;
  for (const auto& element : design.elements) {
    if (const auto* mod = std::get_if<hir::Module>(&element)) {
      hir_modules.push_back(mod);
    }
  }

  const auto& spec_map = *input.specialization_map;

  // Phase 1: Lower one ModuleBody per specialization group.
  // spec_id -> body_id mapping for Phase 2.
  std::unordered_map<common::ModuleSpecId, mir::ModuleBodyId, ModuleSpecIdHash>
      spec_to_body;

  auto spec_groups =
      BuildMirSpecGroups(spec_map, static_cast<uint32_t>(hir_modules.size()));
  for (const auto& group : spec_groups) {
    uint32_t rep_idx = group.representative_module_index;
    if (rep_idx >= hir_modules.size()) {
      throw common::InternalError(
          "LowerDesign", "representative module index out of range");
    }
    const hir::Module& rep_mod = *hir_modules[rep_idx];
    const hir::ModuleBody& hir_body =
        design.module_bodies[rep_mod.body_id.value];
    BodyLocalDecls body_decls =
        CollectBodyLocalDecls(rep_mod, *input.symbol_table, mir_arena);
    LoweringInput body_input = input;
    body_input.hir_arena = &hir_body.arena;
    Result<mir::ModuleBody> body_result = LowerModule(
        hir_body, body_input, mir_arena, origin_map, decls, body_decls,
        rep_mod.body_id);
    if (!body_result) {
      return std::unexpected(body_result.error());
    }
    mir::ModuleBodyId body_id{
        static_cast<uint32_t>(result.module_bodies.size())};
    result.module_bodies.push_back(std::move(*body_result));
    spec_to_body[group.spec_id] = body_id;
  }

  // Phase 2: Emit one mir::Module per HIR module instance and build placement.
  // Walk design elements in original order to preserve instance ordering.
  //
  // Placement computes its own running base counter from the authoritative
  // package slot count + specialization body sizes. This is the source of
  // truth for per-instance base offsets. instance_slot_ranges is then derived
  // from placement below.
  uint32_t next_placement_base = decls.num_package_slots;

  uint32_t module_index = 0;
  for (const auto& element : design.elements) {
    if (std::holds_alternative<hir::Module>(element)) {
      const auto& mod = std::get<hir::Module>(element);
      if (module_index >= spec_map.spec_id_by_instance.size()) {
        throw common::InternalError(
            "LowerDesign", "module index exceeds specialization map size");
      }
      auto spec_id = spec_map.spec_id_by_instance[module_index];
      auto it = spec_to_body.find(spec_id);
      if (it == spec_to_body.end()) {
        throw common::InternalError(
            "LowerDesign", "no body lowered for specialization");
      }
      result.elements.emplace_back(
          mir::Module{.instance_sym = mod.symbol, .body_id = it->second});

      const auto& body = result.module_bodies.at(it->second.value);
      auto slot_count = static_cast<uint32_t>(body.slots.size());
      result.placement.instances.push_back(
          mir::InstancePlacement{
              .instance_sym = mod.symbol,
              .spec_id = spec_id,
              .design_state_base_slot = next_placement_base,
              .slot_count = slot_count,
          });

      // Build InstanceConstBlock from transient design-global param inits.
      // Convert absolute slot IDs to body-local using the canonical
      // instance_slot_ranges as the conversion source of truth.
      mir::InstanceConstBlock const_block;
      if (module_index < decls.instance_param_inits.size()) {
        const auto& slot_range = decls.instance_slot_ranges.at(module_index);
        uint32_t slot_begin = slot_range.slot_begin;
        uint32_t slot_end = slot_begin + slot_range.slot_count;
        for (const auto& entry : decls.instance_param_inits[module_index]) {
          if (entry.slot_id < slot_begin || entry.slot_id >= slot_end) {
            throw common::InternalError(
                "LowerDesign",
                std::format(
                    "param init slot_id {} outside instance slot range "
                    "[{}, {})",
                    entry.slot_id, slot_begin, slot_end));
          }
          uint32_t body_local_slot = entry.slot_id - slot_begin;
          const_block.slot_inits.push_back(
              mir::ConstSlotInit{
                  .body_local_slot = body_local_slot,
                  .value = entry.value,
              });
        }
      }
      result.placement.const_blocks.push_back(std::move(const_block));

      next_placement_base += slot_count;

      ++module_index;
    } else if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      Result<mir::Package> pkg_result =
          LowerPackage(*pkg, input, mir_arena, origin_map, decls);
      if (!pkg_result) {
        return std::unexpected(pkg_result.error());
      }
      result.elements.emplace_back(std::move(*pkg_result));
    }
  }

  // Enforce placement/const-block parallelism invariant.
  if (result.placement.instances.size() !=
      result.placement.const_blocks.size()) {
    throw common::InternalError(
        "LowerDesign",
        std::format(
            "placement instances ({}) and const_blocks ({}) size mismatch",
            result.placement.instances.size(),
            result.placement.const_blocks.size()));
  }

  // Derive compatibility instance_slot_ranges from placement (source of truth).
  result.instance_slot_ranges.reserve(result.placement.instances.size());
  for (const auto& p : result.placement.instances) {
    result.instance_slot_ranges.push_back(
        {p.design_state_base_slot, p.slot_count});
  }

  // Compile port bindings into assembly-ready artifacts (no design mutation).
  mir::CompiledBindingPlan compiled_bindings;
  if (input.binding_plan != nullptr) {
    auto binding_result =
        CompileBindings(*input.binding_plan, decls, input, mir_arena);
    if (!binding_result) return std::unexpected(binding_result.error());
    compiled_bindings = std::move(*binding_result);
  }

  return DesignLoweringResult{
      .design = std::move(result),
      .compiled_bindings = std::move(compiled_bindings),
  };
}

}  // namespace lyra::lowering::hir_to_mir
