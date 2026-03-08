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

namespace lyra::lowering::hir_to_mir {

namespace {

template <class>
inline constexpr bool kAlwaysFalse = false;

struct ModuleSpecIdHash {
  auto operator()(const common::ModuleSpecId& id) const noexcept -> size_t {
    auto h1 = std::hash<uint32_t>{}(id.def_id.value);
    auto h2 = std::hash<uint64_t>{}(id.fingerprint.value);
    return h1 ^ (h2 << 1);
  }
};

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
    mir::Arena& mir_arena, OriginMap* origin_map) -> Result<mir::Design> {
  const DesignDeclarations decls =
      CollectDeclarations(design, input, mir_arena);

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slots = decls.slots;
  result.global_precision_power = input.global_precision_power;
  // Thread slot ranges and def keys for process template grouping in LLVM
  // backend
  result.instance_slot_ranges.reserve(decls.instance_slot_ranges.size());
  for (const auto& range : decls.instance_slot_ranges) {
    result.instance_slot_ranges.push_back({range.slot_begin, range.slot_count});
  }
  result.module_def_ids = decls.module_def_ids;
  result.instance_param_inits = decls.instance_param_inits;
  if (input.instance_table != nullptr) {
    result.instance_table = *input.instance_table;
  }

  // Lower package init processes
  // Collect dynamically generated functions (e.g., strobe thunks)
  DeclView init_view{
      .design_places = &decls.design_places,
      .functions = &decls.functions,
      .slots = &decls.slots};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        hir::ProcessId hir_proc_id = pkg->init_process;
        const hir::Process& proc = (*input.hir_arena)[hir_proc_id];
        // Package init processes have no owning instance (UINT32_MAX sentinel)
        Result<mir::ProcessId> mir_proc_result = LowerProcess(
            hir_proc_id, proc, input, mir_arena, init_view, origin_map,
            UINT32_MAX, &result.generated_functions);
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
    Result<mir::ModuleBody> body_result =
        LowerModule(rep_mod, input, mir_arena, origin_map, decls, rep_idx);
    if (!body_result) {
      return std::unexpected(body_result.error());
    }
    mir::ModuleBodyId body_id{
        static_cast<uint32_t>(result.module_bodies.size())};
    result.module_bodies.push_back(std::move(*body_result));
    spec_to_body[group.spec_id] = body_id;
  }

  // Phase 2: Emit one mir::Module per HIR module instance.
  // Walk design elements in original order to preserve instance ordering.
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

  // Apply port drive bindings (creates synthetic always_comb processes)
  if (input.binding_plan != nullptr) {
    auto binding_result =
        ApplyBindings(*input.binding_plan, decls, input, mir_arena, result);
    if (!binding_result) return std::unexpected(binding_result.error());
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
