#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <span>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/expression.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"
#include "lyra/lowering/hir_to_mir/bound_hierarchy.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/hir_to_mir/design_internal.hpp"
#include "lyra/lowering/hir_to_mir/dpi_header.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"
#include "lyra/lowering/hir_to_mir/process.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/compiled_module_header.hpp"
#include "lyra/mir/connection_recipe.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/module_body.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

template <class>
inline constexpr bool kAlwaysFalse = false;

using common::ModuleSpecIdHash;

// Deterministic specialization group for MIR lowering.
// representative_module_index is the first instance in design order for this
// spec. All members share the same ModuleDefId and behavioral fingerprint.
struct MirSpecGroup {
  common::ModuleSpecId spec_id{};
  uint32_t representative_module_index = 0;
  std::vector<uint32_t> member_module_indices;
};

// Derive durable child-site identity from the parent repertoire.
// Matches by child instance name and returns the repertoire coord +
// definition-local child ordinal (position among same-name entries).
struct DurableChildIdentity {
  common::RepertoireCoord coord;
  uint32_t child_ordinal;
};

auto BuildChildSiteIdentity(
    std::string_view child_name, uint32_t name_occurrence,
    const std::vector<common::ChildCoordEntry>* repertoire_entries)
    -> DurableChildIdentity {
  if (repertoire_entries == nullptr) {
    return {.coord = {}, .child_ordinal = name_occurrence};
  }
  // Find the nth occurrence of this name in the repertoire.
  // The ordinal is the encounter position among all child instances
  // with this coord prefix (not the global site number).
  uint32_t seen = 0;
  uint32_t global_ordinal = 0;
  for (const auto& entry : *repertoire_entries) {
    if (entry.inst_name == child_name) {
      if (seen == name_occurrence) {
        return {.coord = entry.coord, .child_ordinal = global_ordinal};
      }
      ++seen;
    }
    ++global_ordinal;
  }
  // Not found in repertoire -- return empty coord with occurrence ordinal.
  return {.coord = {}, .child_ordinal = name_occurrence};
}

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
  // Phase 0: Design-global declaration collection.
  // All design-global places and package function reservations go into
  // the design arena (mir_arena). This is immutable after Phase 0.
  const DesignDeclarations decls =
      CollectDesignDeclarations(design, input, mir_arena);

  // Surface export signature diagnostics as user-facing errors.
  if (!decls.export_diagnostics.empty()) {
    const auto& first = decls.export_diagnostics[0];
    auto diag = Diagnostic::Error(first.span, first.message);
    for (size_t i = 1; i < decls.export_diagnostics.size(); ++i) {
      diag = std::move(diag).WithNote(
          decls.export_diagnostics[i].span,
          decls.export_diagnostics[i].message);
    }
    return std::unexpected(std::move(diag));
  }

  // (Per-body site registries are allocated below, after spec_map is defined.)

  mir::Design result;
  result.num_design_slots = decls.num_design_slots;
  result.slots = decls.slots;
  // max_body_local_events is populated after all module bodies are lowered
  // (see Phase 2 below).
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

  // DPI export wrapper descriptors are built after Phase 2 (body lowering)
  // because module-scoped exports need body-local function maps to resolve
  // their target FunctionId.
  std::vector<mir::DpiExportWrapperDesc> dpi_export_wrappers;

  // Phase 0: Lower package init processes into design arena.
  // Design-global origin storage for design-global MIR.
  DeclView init_view{
      .design_places = &decls.design_places,
      .functions = &decls.functions,
      .slots = &decls.slots,
      .dpi_imports = &decls.dpi_imports};
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      if (pkg->init_process) {
        hir::ProcessId hir_proc_id = pkg->init_process;
        const hir::Process& proc = (*input.hir_arena)[hir_proc_id];
        Result<mir::ProcessId> mir_proc_result = LowerProcess(
            hir_proc_id, proc, input, mir_arena, init_view, origin_map,
            &result.generated_functions);
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
  // Each call produces an isolated MirBodyLoweringResult with body-local
  // MIR arena and body-local origins. No body-lowering path writes to the
  // design arena.
  // Also retain body-local place maps for InstanceSlotResolver construction.
  std::unordered_map<uint32_t, std::vector<BodyLocalSlotEntry>>
      body_local_slots_by_body;

  std::vector<Result<MirBodyLoweringResult>> body_results;
  body_results.reserve(spec_map.groups.size());

  // Per-body site registries. Each body gets a fresh registry so site IDs
  // are body-local (0-based per body). Design-global tables are built by
  // concatenation in Phase 2.
  auto num_groups = spec_map.groups.size();
  std::vector<mir::ImmediateCoverSiteRegistry> cover_site_registries(
      num_groups);
  std::vector<mir::DeferredAssertionSiteRegistry> deferred_site_registries(
      num_groups);

  auto spec_groups =
      BuildMirSpecGroups(spec_map, static_cast<uint32_t>(hir_modules.size()));
  // Phase 1 loop index g becomes the MIR ModuleBodyId for this body
  // (Phase 2 assembles module_bodies by sequential push_back in the
  // same group order). All per-body maps built here are keyed by g
  // (i.e. the future mir::ModuleBodyId.value), not by hir::ModuleBodyId.
  for (size_t g = 0; g < spec_groups.size(); ++g) {
    const auto& group = spec_groups[g];
    uint32_t rep_idx = group.representative_module_index;
    if (rep_idx >= hir_modules.size()) {
      throw common::InternalError(
          "LowerDesign", "representative module index out of range");
    }
    const hir::Module& rep_mod = *hir_modules[rep_idx];
    const hir::ModuleBody& hir_body =
        design.module_bodies[rep_mod.body_id.value];

    // Enter body-local artifact domain: HIR, constants, and MIR all
    // switch to body-owned storage for this specialization group.
    LoweringInput body_input = input;
    body_input.hir_arena = &hir_body.arena;
    body_input.active_constant_arena = &hir_body.constant_arena;

    mir::Arena body_arena;
    BodyLocalDecls body_decls = CollectBodyLocalDecls(
        rep_mod, *input.symbol_table, *input.type_arena, body_arena);

    // Capture body-local slot mappings for InstanceSlotResolver.
    // Keyed by MIR body group index (== future mir::ModuleBodyId.value).
    {
      auto& entries = body_local_slots_by_body[static_cast<uint32_t>(g)];
      for (const auto& [sym, place_id] : body_decls.places) {
        const auto& place = body_arena[place_id];
        if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot) {
          entries.push_back(
              {.sym = sym,
               .local_slot =
                   common::LocalSlotId{static_cast<uint32_t>(place.root.id)},
               .type = place.root.type});
        }
      }
    }

    body_results.push_back(LowerModule(
        hir_body, body_input, std::move(body_arena), mir_arena, decls,
        body_decls, &cover_site_registries[g], &deferred_site_registries[g]));
  }

  // Phase 2: Assemble body results in stable group order.
  // Merge body units and origins.
  std::unordered_map<common::ModuleSpecId, mir::ModuleBodyId, ModuleSpecIdHash>
      spec_to_body;
  std::vector<std::vector<OriginEntry>> body_origins;
  body_origins.reserve(body_results.size());
  // Retain body-local symbol-to-function maps for module-scoped DPI export
  // target resolution. Keyed by ModuleBodyId.
  std::unordered_map<uint32_t, SymbolToMirFunctionMap> body_function_maps;

  // B2: Extract provisional targets before body moves consume the results.
  std::unordered_map<uint32_t, std::vector<ProvisionalNonLocalTarget>>
      provisionals_by_body;

  for (size_t g = 0; g < body_results.size(); ++g) {
    if (!body_results[g]) {
      return std::unexpected(body_results[g].error());
    }
    auto& product = *body_results[g];

    mir::ModuleBodyId body_id{
        static_cast<uint32_t>(result.module_bodies.size())};
    if (!product.provisional_targets.empty()) {
      provisionals_by_body[body_id.value] =
          std::move(product.provisional_targets);
    }
    product.body.external_refs = std::move(product.external_refs);
    if (input.body_timescales != nullptr && g < input.body_timescales->size()) {
      const auto& ts = (*input.body_timescales)[g];
      product.body.time_unit_power = ts.unit_power;
      product.body.time_precision_power = ts.precision_power;
    }
    result.module_bodies.push_back(std::move(product.body));
    body_origins.push_back(std::move(product.origins));
    body_function_maps[body_id.value] = std::move(product.symbol_to_function);
    spec_to_body[spec_groups[g].spec_id] = body_id;
  }

  // Track max body-local event count (for ABI reserved field).
  size_t max_events = 0;
  for (const auto& body : result.module_bodies) {
    max_events = std::max(max_events, body.events.size());
  }
  result.max_body_local_events = max_events;

  // Build design-global site tables by concatenating per-body registries.
  // Site IDs in MIR effects are body-local (0-based per body). The design-
  // global tables are the concatenation of all body-local tables in body
  // order, used only at assembly/emission time for runtime table building.
  for (size_t g = 0; g < result.module_bodies.size(); ++g) {
    auto& body = result.module_bodies[g];
    body.immediate_cover_sites = cover_site_registries[g].TakeSites();
    body.deferred_assertion_sites = deferred_site_registries[g].TakeSites();
    result.immediate_cover_sites.insert(
        result.immediate_cover_sites.end(), body.immediate_cover_sites.begin(),
        body.immediate_cover_sites.end());
    result.deferred_assertion_sites.insert(
        result.deferred_assertion_sites.end(),
        body.deferred_assertion_sites.begin(),
        body.deferred_assertion_sites.end());
  }

  // Build DPI export wrapper descriptors now that body-local function maps
  // are available. Module-scoped exports need these maps to resolve their
  // target FunctionId.
  {
    // Collect module-scoped export targets with multi-body collision
    // detection. For each module-scoped export symbol, collect all
    // (body pointer, function_id) pairs across specialization groups.
    struct ModuleExportResolution {
      std::string c_name;
      SourceSpan span;
      std::vector<mir::ModuleExportCalleeKey> targets;
    };
    std::unordered_map<SymbolId, ModuleExportResolution, SymbolIdHash>
        module_export_resolutions;
    for (size_t g = 0; g < spec_groups.size(); ++g) {
      uint32_t rep_idx = spec_groups[g].representative_module_index;
      const hir::Module& rep_mod = *hir_modules[rep_idx];
      const hir::ModuleBody& hir_body =
          design.module_bodies[rep_mod.body_id.value];
      mir::ModuleBodyId body_id{static_cast<uint32_t>(g)};
      auto map_it = body_function_maps.find(body_id.value);
      if (map_it == body_function_maps.end()) continue;
      const auto& func_map = map_it->second;
      for (const auto& exp : hir_body.dpi_exports) {
        auto func_it = func_map.find(exp.symbol);
        if (func_it == func_map.end()) continue;
        auto& resolution = module_export_resolutions[exp.symbol];
        if (resolution.c_name.empty()) {
          resolution.c_name = exp.c_name;
          resolution.span = exp.span;
        }
        resolution.targets.push_back(
            mir::ModuleExportCalleeKey{
                .body = &result.module_bodies[g],
                .function_id = func_it->second});
      }
    }

    std::vector<const DpiExportInfo*> sorted_exports;
    sorted_exports.reserve(decls.dpi_exports.Size());
    for (const auto& [_, info] : decls.dpi_exports.Entries()) {
      sorted_exports.push_back(&info);
    }
    std::ranges::sort(sorted_exports, [](const auto* a, const auto* b) {
      if (a->c_name != b->c_name) return a->c_name < b->c_name;
      return a->symbol.value < b->symbol.value;
    });
    std::vector<DesignDeclarations::ExportDiagnostic> wrapper_diagnostics;
    for (const auto* exp : sorted_exports) {
      mir::DpiExportTarget target{};
      if (exp->is_module_scoped) {
        target.scope_kind = mir::DpiExportScopeKind::kModule;
        auto res_it = module_export_resolutions.find(exp->symbol);
        if (res_it == module_export_resolutions.end() ||
            res_it->second.targets.empty()) {
          throw common::InternalError(
              "LowerDesign",
              std::format(
                  "module-scoped DPI export '{}' has no resolved body-local "
                  "target",
                  exp->c_name));
        }
        const auto& resolution = res_it->second;
        if (resolution.targets.size() > 1) {
          wrapper_diagnostics.push_back(
              DesignDeclarations::ExportDiagnostic{
                  .span = exp->span,
                  .message = std::format(
                      "DPI-C export '{}': module-scoped export appears in {} "
                      "distinct specialization bodies; multi-body callable "
                      "dispatch not yet supported",
                      exp->c_name, resolution.targets.size()),
              });
          continue;
        }
        target.module_target = resolution.targets[0];
      } else {
        target.scope_kind = mir::DpiExportScopeKind::kPackage;
        auto it = decls.functions.find(exp->symbol);
        if (it == decls.functions.end()) {
          throw common::InternalError(
              "LowerDesign",
              std::format(
                  "package-scoped DPI export '{}' has no resolved "
                  "design-global FunctionId",
                  exp->c_name));
        }
        target.package_symbol = exp->symbol;
      }
      dpi_export_wrappers.push_back(
          mir::DpiExportWrapperDesc{
              .c_name = exp->c_name,
              .signature = exp->signature,
              .target = target,
              .routine_kind = exp->is_task ? mir::DpiRoutineKind::kTask
                                           : mir::DpiRoutineKind::kFunction,
          });
    }
    if (!wrapper_diagnostics.empty()) {
      const auto& first = wrapper_diagnostics[0];
      auto diag = Diagnostic::Error(first.span, first.message);
      for (size_t i = 1; i < wrapper_diagnostics.size(); ++i) {
        diag = std::move(diag).WithNote(
            wrapper_diagnostics[i].span, wrapper_diagnostics[i].message);
      }
      return std::unexpected(std::move(diag));
    }
  }

  // B2 Phase 3a: Build CompiledModuleHeaders for each specialization group.
  // Headers provide the child-facing contract (port -> LocalSlotId mapping)
  // used by parent-side connection compilation.
  //
  // Port entries are built from declared port data (hir::Module::ports),
  // not from the binding plan. This ensures headers reflect the source-level
  // module interface regardless of which ports have bindings.
  mir::HeaderDatabase header_database;
  std::unordered_map<
      common::ModuleSpecId, mir::CompiledModuleHeaderId,
      common::ModuleSpecIdHash>
      header_ids_by_spec;

  for (const auto& group : spec_groups) {
    uint32_t rep_idx = group.representative_module_index;
    const hir::Module& rep_mod = *hir_modules[rep_idx];
    auto mir_body_id = spec_to_body.at(group.spec_id);
    const auto& body_slots_for_header =
        body_local_slots_by_body.at(mir_body_id.value);

    // Build sym -> (LocalSlotId, TypeId) lookup from body-local slots.
    std::unordered_map<SymbolId, common::LocalSlotId, SymbolIdHash> sym_to_slot;
    std::unordered_map<SymbolId, TypeId, SymbolIdHash> sym_to_type;
    for (const auto& entry : body_slots_for_header) {
      sym_to_slot[entry.sym] = entry.local_slot;
      sym_to_type[entry.sym] = entry.type;
    }

    // Build port entries from declared ports on the representative module.
    std::vector<mir::PortEntry> port_entries;
    for (const auto& port : rep_mod.ports) {
      auto slot_it = sym_to_slot.find(port.sym);
      if (slot_it == sym_to_slot.end()) continue;
      auto type_it = sym_to_type.find(port.sym);
      if (type_it == sym_to_type.end()) continue;
      mir::PortDirection mir_dir{};
      switch (port.dir) {
        case hir::HirPortDirection::kInput:
          mir_dir = mir::PortDirection::kInput;
          break;
        case hir::HirPortDirection::kOutput:
          mir_dir = mir::PortDirection::kOutput;
          break;
        case hir::HirPortDirection::kInOut:
          mir_dir = mir::PortDirection::kInOut;
          break;
      }
      port_entries.push_back(
          mir::PortEntry{
              .sym = port.sym,
              .slot = slot_it->second,
              .type = type_it->second,
              .dir = mir_dir});
    }

    // Sort by slot value for deterministic ordering.
    std::ranges::sort(port_entries, [](const auto& a, const auto& b) {
      return a.slot.value < b.slot.value;
    });

    auto header = mir::CompiledModuleHeader::Create(
        group.spec_id, group.spec_id.def_id, std::move(port_entries));
    auto header_id = header_database.Add(std::move(header));
    header_ids_by_spec[group.spec_id] = header_id;
  }

  // Phase 2b: Emit one mir::Module per HIR module instance and build placement.
  // Walk design elements in original order to preserve instance ordering.
  // Build ConstructionInput directly -- the single source of truth for
  // per-object constructor-owned data.
  mir::ConstructionInput construction;
  if (input.instance_table != nullptr) {
    construction.instance_table = *input.instance_table;
  }

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
      // Safe pointer capture: Phase 2 finished pushing to module_bodies
      // before Phase 2b began, so &module_bodies[i] is stable for the
      // lifetime of the design.
      const auto& body = result.module_bodies.at(it->second.value);
      result.elements.emplace_back(
          mir::Module{.instance_sym = mod.symbol, .body = &body});
      auto slot_count = static_cast<uint32_t>(body.slots.size());

      // Build InstanceConstBlock directly from HIR param values.
      // Body-local slot indices: params start after variables + nets.
      mir::InstanceConstBlock const_block;
      {
        auto param_slot_base =
            static_cast<uint32_t>(mod.variables.size() + mod.nets.size());
        for (size_t pi = 0; pi < mod.param_init_values.size(); ++pi) {
          const_block.slot_inits.push_back(
              mir::ConstSlotInit{
                  .body_local_slot =
                      param_slot_base + static_cast<uint32_t>(pi),
                  .value = mod.param_init_values[pi],
              });
        }
      }
      construction.const_blocks.push_back(std::move(const_block));

      construction.objects.push_back(
          mir::ObjectRecord{
              .body_group = it->second.value,
              .path_index = module_index,
              .design_state_base_slot = next_placement_base,
              .slot_count = slot_count,
              .spec_id = spec_id,
              .instance_sym = mod.symbol,
          });

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

  // Enforce objects/const-blocks parallelism invariant.
  if (construction.objects.size() != construction.const_blocks.size()) {
    throw common::InternalError(
        "LowerDesign",
        std::format(
            "construction objects ({}) and const_blocks ({}) size mismatch",
            construction.objects.size(), construction.const_blocks.size()));
  }

  // Build InstanceSlotResolver from canonical body-local slot data.
  // Maps (instance_sym, variable_sym) -> BoundEndpoint.
  // LocalSlotId is derived from body_local_slots_by_body, not from manual
  // ordering assumptions. This ensures the resolver stays consistent if
  // CollectBodyLocalDecls ordering changes.
  //
  // Build body_group -> representative path_index for per-instance mapping.
  std::unordered_map<uint32_t, uint32_t> body_to_representative;
  for (const auto& group : spec_groups) {
    auto body_id = spec_to_body.at(group.spec_id);
    body_to_representative[body_id.value] = group.representative_module_index;
  }

  InstanceSlotResolver resolver;
  for (size_t oi = 0; oi < construction.objects.size(); ++oi) {
    const auto& obj = construction.objects[oi];
    uint32_t body_group = obj.body_group;

    // Build representative sym -> LocalSlotId from canonical body-local data.
    const auto& body_slots = body_local_slots_by_body.at(body_group);
    std::unordered_map<SymbolId, common::LocalSlotId, SymbolIdHash>
        rep_sym_to_slot;
    for (const auto& entry : body_slots) {
      rep_sym_to_slot[entry.sym] = entry.local_slot;
    }

    uint32_t rep_path = body_to_representative.at(body_group);
    const auto& rep_mod = *hir_modules[rep_path];
    const auto& inst_mod = *hir_modules[obj.path_index];

    auto map_by_position = [&](const std::vector<SymbolId>& inst_syms,
                               const std::vector<SymbolId>& rep_syms) {
      for (size_t i = 0; i < inst_syms.size(); ++i) {
        auto it = rep_sym_to_slot.find(rep_syms[i]);
        if (it == rep_sym_to_slot.end()) continue;
        resolver.Insert(
            obj.instance_sym, inst_syms[i],
            mir::BoundEndpoint{
                .object_index = common::ObjectIndex{static_cast<uint32_t>(oi)},
                .local_slot = it->second});
      }
    };

    map_by_position(inst_mod.variables, rep_mod.variables);
    map_by_position(inst_mod.nets, rep_mod.nets);
    map_by_position(inst_mod.param_slots, rep_mod.param_slots);
  }

  // B2 Phase 3b: Build ChildInstantiationSites for each parent body.
  // Uses construction.objects to determine parent-child relationships:
  // binding plan's parent_instance_sym is the child instance's SymbolId.
  // The parent is found by looking up the child instance's module_index
  // and checking which parent body's scope contains it.
  //
  // instance_sym -> object_index map for child lookup.
  std::unordered_map<SymbolId, uint32_t, SymbolIdHash> sym_to_object_index;
  for (uint32_t i = 0; i < construction.objects.size(); ++i) {
    sym_to_object_index[construction.objects[i].instance_sym] = i;
  }

  // Collect unique child instance syms per parent body_group.
  // parent_instance_sym in bindings IS the child instance's SymbolId.
  // We find the child's object, then find its parent by looking at the
  // hierarchy (the parent is the object whose scope contains the child).
  //
  // For now, we use a heuristic: in the sorted BFS order, each child
  // instance was added when scanning its parent's body. The binding plan
  // groups bindings by parent_instance_sym (child instance). The parent
  // of a child instance can be found from the instance_table hierarchical
  // paths, but that's complex. Instead, we derive it from the binding
  // plan: for each unique parent_instance_sym that has bindings, that's
  // a child instance. We group them by the parent body that should own
  // them. The parent body is identified by looking at which spec group
  // contains the parent instance that is an ancestor of this child.
  //
  // For this phase, we build child_sites per spec group by scanning
  // ALL bindings. The parent body for a binding is determined by which
  // object owns the child: the child's object_index leads to its
  // body_group, but that's the CHILD's body, not the parent's.
  // Instead, we need to know which parent contains this child instance.
  //
  // The simplest approach: use the resolver's FindOwnerInstance. But the
  // owner of child_port_sym is the child instance itself, not the parent.
  // We need the parent of the child instance.
  //
  // Instance hierarchy is encoded in instance_table paths:
  // "top" -> "top.u_child" -> "top.u_child.u_grandchild"
  // The parent of "top.u_child" is "top".
  //
  // Build parent-child relationships from instance_table paths.
  // parent_module_index -> list of child instances.
  std::unordered_map<uint32_t, std::vector<ChildSiteInfo>> parent_to_children;

  if (input.instance_table != nullptr) {
    // Build instance_sym -> path for all instances.
    std::unordered_map<SymbolId, std::string_view, SymbolIdHash> sym_to_path;
    for (const auto& entry : input.instance_table->entries) {
      sym_to_path[entry.instance_sym] = entry.full_path;
    }

    // Build path -> module_index for parent lookup.
    std::unordered_map<std::string_view, uint32_t> path_to_module_index;
    for (uint32_t i = 0; i < hir_modules.size(); ++i) {
      auto it = sym_to_path.find(hir_modules[i]->symbol);
      if (it != sym_to_path.end()) {
        path_to_module_index[it->second] = i;
      }
    }

    // For each module instance (child), find its parent by stripping
    // path components until a module instance is found. This handles
    // children inside generate blocks (e.g., "Top.gen_blk[0].child"
    // has parent "Top", skipping the generate scope).
    for (uint32_t ci = 0; ci < hir_modules.size(); ++ci) {
      auto path_it = sym_to_path.find(hir_modules[ci]->symbol);
      if (path_it == sym_to_path.end()) continue;
      std::string_view remaining = path_it->second;
      uint32_t parent_module_index = 0;
      bool found_parent = false;
      while (!remaining.empty()) {
        auto last_dot = remaining.rfind('.');
        if (last_dot == std::string_view::npos) break;
        remaining = remaining.substr(0, last_dot);
        auto parent_it = path_to_module_index.find(remaining);
        if (parent_it != path_to_module_index.end()) {
          parent_module_index = parent_it->second;
          found_parent = true;
          break;
        }
      }
      if (!found_parent) continue;
      auto child_spec = spec_map.spec_id_by_instance[ci];
      parent_to_children[parent_module_index].push_back(
          ChildSiteInfo{
              .child_instance_sym = hir_modules[ci]->symbol,
              .child_module_index = ci,
              .child_spec = child_spec});
    }
  }

  // Now build child_sites for each spec group's representative.
  // All instances in a spec group have structurally identical children.
  // We use the representative's children and assign ChildBindingSiteIds.
  //
  // Also builds oi_to_durable_child at the same moment each DurableChildId
  // is created for a specific child object. This is the canonical bridge
  // from topology object identity to durable repertoire identity, used by
  // BoundHierarchyIndex and CanonicalizeExternalRefPaths.
  std::unordered_map<uint32_t, std::vector<mir::ChildInstantiationSite>>
      child_sites_by_body;
  std::unordered_map<uint32_t, mir::DurableChildId> oi_to_durable_child;

  for (const auto& group : spec_groups) {
    uint32_t rep_idx = group.representative_module_index;
    auto body_id = spec_to_body.at(group.spec_id);

    auto children_it = parent_to_children.find(rep_idx);
    if (children_it == parent_to_children.end()) continue;

    auto& sites = child_sites_by_body[body_id.value];
    uint32_t next_site_id = 0;

    // Get repertoire entries for this parent definition (ordered list of
    // child instance {inst_name, coord} from the definition repertoire).
    const hir::Module& rep_parent = *hir_modules[rep_idx];
    const std::vector<common::ChildCoordEntry>* repertoire_entries = nullptr;
    if (input.child_coord_map != nullptr) {
      auto coord_it = input.child_coord_map->find(rep_parent.module_def_id);
      if (coord_it != input.child_coord_map->end()) {
        repertoire_entries = &coord_it->second;
      }
    }

    // Track name occurrences for generate-scoped disambiguation.
    std::unordered_map<std::string_view, uint32_t> name_occurrences;

    for (const auto& child_info : children_it->second) {
      mir::ChildBindingSiteId site{next_site_id++};
      const auto& child_name =
          (*input.symbol_table)[child_info.child_instance_sym].name;
      uint32_t occurrence = name_occurrences[child_name]++;

      auto id =
          BuildChildSiteIdentity(child_name, occurrence, repertoire_entries);
      mir::DurableChildId durable{
          .coord = std::move(id.coord), .child_ordinal = id.child_ordinal};

      // Record oi -> DurableChildId at creation site. Each child object
      // must map to exactly one durable identity.
      uint32_t child_oi = child_info.child_module_index;
      auto [ins_it, inserted] = oi_to_durable_child.emplace(child_oi, durable);
      if (!inserted) {
        throw common::InternalError(
            "LowerDesign",
            std::format(
                "child oi {} already has a DurableChildId mapping", child_oi));
      }

      sites.push_back(
          mir::ChildInstantiationSite{
              .site = site,
              .id = durable,
              .child_spec = child_info.child_spec,
              .debug_instance_sym = child_info.child_instance_sym,
              .origin = common::OriginId::Invalid()});
    }

    // Register non-representative parents' children with the same
    // DurableChildIds by positional mapping. Each member of the spec
    // group has structurally identical children.
    for (uint32_t member_idx : group.member_module_indices) {
      if (member_idx == rep_idx) continue;
      auto member_children_it = parent_to_children.find(member_idx);
      if (member_children_it == parent_to_children.end()) continue;
      const auto& rep_children = children_it->second;
      const auto& member_children = member_children_it->second;
      for (size_t ci = 0;
           ci < member_children.size() && ci < rep_children.size(); ++ci) {
        uint32_t member_child_oi = member_children[ci].child_module_index;
        oi_to_durable_child.emplace(member_child_oi, sites[ci].id);
      }
    }
  }

  // B2 Phase 4: Build ConnectionRecipes alongside the old path.
  // For each binding, create a ConnectionRecipe using the header database
  // and child_sites. The new path runs in parallel with the old
  // recipe path (currently only handles kLocalSlot sources).
  std::unordered_map<uint32_t, std::vector<mir::ConnectionRecipe>>
      connection_recipes_by_body;

  if (input.binding_plan != nullptr) {
    // Build child_instance_sym -> (body_group, ChildBindingSiteId) map.
    // Maps any child instance SymbolId (from any member of a spec group)
    // to its parent body's child_sites entry. Non-representative children
    // are mapped by finding their parent spec group's representative and
    // matching by child position.
    struct ChildSiteLookup {
      uint32_t parent_body_group = 0;
      mir::ChildBindingSiteId site;
      uint32_t rep_child_module_index = 0;
    };
    std::unordered_map<SymbolId, ChildSiteLookup, SymbolIdHash>
        child_sym_to_site;

    // First: register representative children directly.
    for (const auto& [body_group, sites] : child_sites_by_body) {
      for (const auto& site : sites) {
        // Find the child's module_index from parent_to_children.
        uint32_t rep_child_idx = 0;
        for (const auto& group : spec_groups) {
          auto body_id = spec_to_body.at(group.spec_id);
          if (body_id.value != body_group) continue;
          auto children_it =
              parent_to_children.find(group.representative_module_index);
          if (children_it == parent_to_children.end()) continue;
          for (const auto& ci : children_it->second) {
            if (ci.child_instance_sym == site.debug_instance_sym) {
              rep_child_idx = ci.child_module_index;
              break;
            }
          }
          break;
        }
        child_sym_to_site[site.debug_instance_sym] = ChildSiteLookup{
            .parent_body_group = body_group,
            .site = site.site,
            .rep_child_module_index = rep_child_idx};
      }
    }

    // Second: register non-representative children by positional mapping.
    // For each spec group, map non-representative parent's children to
    // the representative parent's child_sites by position.
    for (const auto& group : spec_groups) {
      auto body_id = spec_to_body.at(group.spec_id);
      auto rep_children_it =
          parent_to_children.find(group.representative_module_index);
      if (rep_children_it == parent_to_children.end()) continue;
      const auto& rep_children = rep_children_it->second;

      for (uint32_t member_idx : group.member_module_indices) {
        if (member_idx == group.representative_module_index) continue;
        auto member_children_it = parent_to_children.find(member_idx);
        if (member_children_it == parent_to_children.end()) continue;
        const auto& member_children = member_children_it->second;

        for (size_t ci = 0;
             ci < member_children.size() && ci < rep_children.size(); ++ci) {
          auto& sites = child_sites_by_body[body_id.value];
          if (ci >= sites.size()) break;
          child_sym_to_site[member_children[ci].child_instance_sym] =
              ChildSiteLookup{
                  .parent_body_group = body_id.value,
                  .site = sites[ci].site,
                  .rep_child_module_index =
                      rep_children[ci].child_module_index};
        }
      }
    }

    for (const auto& binding : input.binding_plan->bindings) {
      // Step 1: Classify child port. Must be resolver-addressable
      // (body-local). Non-body-local ports are unsupported.
      if (!resolver.Contains(binding.child_port_sym)) continue;

      // Step 2: Find child instance and site from port variable.
      SymbolId child_instance_sym =
          resolver.FindOwnerInstance(binding.child_port_sym);
      auto site_it = child_sym_to_site.find(child_instance_sym);
      if (site_it == child_sym_to_site.end()) continue;
      const auto& site_lookup = site_it->second;

      // Step 3: Get child port slot (guaranteed by Contains check).
      auto child_endpoint = resolver.ResolveByVariable(binding.child_port_sym);
      common::LocalSlotId child_slot = child_endpoint.local_slot;
      TypeId result_type = (*input.symbol_table)[binding.child_port_sym].type;

      auto kind =
          (binding.kind == ast_to_hir::PortBinding::Kind::kDriveParentToChild)
              ? mir::PortConnection::Kind::kDriveParentToChild
              : mir::PortConnection::Kind::kDriveChildToParent;

      // Step 4: Resolve parent-side expression.
      // Only simple NameRef with resolver-addressable (body-local)
      // symbols are in the supported subset. Non-NameRef (complex
      // expressions) and non-body-local NameRef (design-global,
      // hierarchical) are unsupported and left to the old path.
      mir::ConnectionSourceRecipe source;
      mir::TriggerRecipe trigger;

      if (kind == mir::PortConnection::Kind::kDriveParentToChild) {
        const hir::Expression& expr = (*input.hir_arena)[binding.parent_rvalue];
        if (expr.kind == hir::ExpressionKind::kNameRef) {
          // Simple NameRef: kLocalSlot source.
          const auto& name_data =
              std::get<hir::NameRefExpressionData>(expr.data);
          if (!resolver.Contains(name_data.symbol)) continue;
          auto parent_endpoint = resolver.ResolveByVariable(name_data.symbol);
          source = mir::ConnectionSourceRecipe::FromLocalSlot(
              parent_endpoint.local_slot);
          trigger = mir::TriggerRecipe::FromLocalSlot(
              parent_endpoint.local_slot, common::EdgeKind::kAnyChange);
        } else {
          // Complex expression: compile as body-local function.
          auto any_ref =
              FindAnyNameRef(binding.parent_rvalue, *input.hir_arena);
          if (!any_ref || !resolver.Contains(*any_ref)) continue;
          auto parent_endpoint = resolver.ResolveByVariable(*any_ref);
          auto parent_oi = parent_endpoint.object_index;
          uint32_t body_group =
              construction.objects[parent_oi.value].body_group;
          uint32_t rep_path = body_to_representative.at(body_group);
          auto& parent_body = result.module_bodies[body_group];
          auto per_instance_places = BuildPerInstancePlaces(
              *hir_modules[construction.objects[parent_oi.value].path_index],
              *hir_modules[rep_path], body_local_slots_by_body.at(body_group),
              parent_body.arena);
          auto func_result = LowerExprAsBodyFunction(
              binding.parent_rvalue, result_type, input, decls,
              parent_body.arena, per_instance_places);
          if (!func_result) return std::unexpected(func_result.error());
          parent_body.functions.push_back(*func_result);
          source = mir::ConnectionSourceRecipe::FromFunction(*func_result);
          trigger = mir::TriggerRecipe::FromLocalSlot(
              parent_endpoint.local_slot, common::EdgeKind::kAnyChange);
        }
      } else {
        const hir::Expression& lvalue_expr =
            (*input.hir_arena)[binding.parent_lvalue];
        if (lvalue_expr.kind != hir::ExpressionKind::kNameRef) continue;
        const auto& lvalue_data =
            std::get<hir::NameRefExpressionData>(lvalue_expr.data);
        if (!resolver.Contains(lvalue_data.symbol)) continue;
        auto parent_endpoint = resolver.ResolveByVariable(lvalue_data.symbol);
        source = mir::ConnectionSourceRecipe::FromLocalSlot(
            parent_endpoint.local_slot);
        trigger = mir::TriggerRecipe::FromChildSlot(
            child_slot, common::EdgeKind::kAnyChange);
      }

      connection_recipes_by_body[site_lookup.parent_body_group].push_back(
          mir::ConnectionRecipe{
              .kind = kind,
              .child_site = site_lookup.site,
              .child_slot = child_slot,
              .source = source,
              .trigger = trigger,
              .result_type = result_type,
              .origin = common::OriginId::Invalid()});
    }
  }

  // B2: Propagate child_sites and connection_recipes into bodies.
  for (auto& [body_id_val, sites] : child_sites_by_body) {
    result.module_bodies[body_id_val].child_sites = std::move(sites);
  }
  for (auto& [body_id_val, recipes] : connection_recipes_by_body) {
    result.module_bodies[body_id_val].connection_recipes = std::move(recipes);
  }

  // B2: Build topology index and resolve external ref recipes.
  // Ordering: FinalizeExternalRefTargetSlots fills target_slot (needs
  // provisionals for target_sym). CanonicalizeExternalRefPaths fills
  // target.path with DurableChildId entries (needs provisionals for
  // topology walk, but resolves through oi_to_durable_child, not
  // debug_instance_sym). BuildPerInstanceExtRefBindings walks
  // each recipe from each instance's actual position to compute
  // per-instance resolved ext-ref runtime bindings.
  auto topo = BuildBoundHierarchyIndex(
      construction, parent_to_children, body_to_representative,
      oi_to_durable_child);

  // Copy parent topology into construction input for constructor use.
  construction.parent_instance_indices = topo.parent_of;

  // Build per-instance structural child identity from oi_to_durable_child.
  auto instance_count = construction.objects.size();
  construction.child_durable_ids.resize(instance_count);
  for (const auto& [oi, durable_id] : oi_to_durable_child) {
    if (oi < instance_count) {
      construction.child_durable_ids[oi] = durable_id;
    }
  }

  FinalizeExternalRefTargetSlots(
      result, provisionals_by_body, body_local_slots_by_body, topo,
      construction,
      std::span<const hir::Module* const>(
          hir_modules.data(), hir_modules.size()));

  CanonicalizeExternalRefPaths(
      result, provisionals_by_body, topo, oi_to_durable_child);

  // After canonicalization, provisionals_by_body is scratch-only.
  // Build per-instance resolved ext-ref runtime bindings from canonical
  // recipes.
  BuildPerInstanceExtRefBindings(result, construction, topo);

  // Propagate decision owner acceptance through design-global call graph.
  mir_arena.PropagateDeferredOwnerAbi();

  // Bind connection recipes against topology.
  // kLocalSlot-sourced recipes -> BoundConnection (kernel entries).
  // kFunction-sourced recipes -> CompiledConnectionExpr (process cloning).
  std::vector<mir::BoundConnection> bound_connections;
  std::vector<mir::CompiledConnectionExpr> expr_connections;
  for (uint32_t oi = 0; oi < construction.objects.size(); ++oi) {
    uint32_t body_idx = construction.objects[oi].body_group;
    const auto& body = result.module_bodies[body_idx];
    if (body.connection_recipes.empty()) continue;
    for (uint32_t r = 0; r < body.connection_recipes.size(); ++r) {
      const auto& recipe = body.connection_recipes[r];
      if (IsFullyBindableRecipe(recipe)) {
        bound_connections.push_back(BindConnectionRecipe(
            recipe, r, body, common::ObjectIndex{oi}, topo, construction));
      } else if (
          recipe.source.kind == mir::ConnectionSourceRecipe::Kind::kFunction) {
        // This adapter only supports kDriveParentToChild with
        // kLocalSlot trigger. Other shapes are not yet lowerable.
        if (recipe.kind != mir::PortConnection::Kind::kDriveParentToChild ||
            recipe.trigger.kind != mir::TriggerRecipe::Kind::kLocalSlot) {
          throw common::InternalError(
              "LowerDesign", std::format(
                                 "kFunction recipe has unsupported shape "
                                 "(kind={}, trigger kind={})",
                                 static_cast<uint8_t>(recipe.kind),
                                 static_cast<uint8_t>(recipe.trigger.kind)));
        }
        if (recipe.child_site.value >= body.child_sites.size()) continue;
        const auto& site = body.child_sites[recipe.child_site.value];
        uint32_t parent_bg = construction.objects[oi].body_group;
        uint32_t child_oi = topo.ResolveChildByDurableId(parent_bg, site.id);
        // Resolve trigger endpoint (parent-local slot).
        auto trigger_ep = mir::BoundEndpoint{
            .object_index = common::ObjectIndex{oi},
            .local_slot = recipe.trigger.local_slot};
        expr_connections.push_back(
            mir::CompiledConnectionExpr{
                .kind = recipe.kind,
                .parent_arena = &body.arena,
                .expr_function = recipe.source.function,
                .parent_object_index = common::ObjectIndex{oi},
                .child_object_index = common::ObjectIndex{child_oi},
                .child_local_slot = recipe.child_slot,
                .result_type = recipe.result_type,
                .trigger = trigger_ep,
                .trigger_edge = recipe.trigger.edge,
                .child_port_sym = {},
                .parent_instance_sym = {}});
      }
    }
  }

  return DesignLoweringResult{
      .design = std::move(result),
      .construction = std::move(construction),
      .bound_connections = std::move(bound_connections),
      .expr_connections = std::move(expr_connections),
      .body_origins = std::move(body_origins),
      .dpi_export_wrappers = std::move(dpi_export_wrappers),
      .dpi_header = (!decls.dpi_exports.Empty() || !decls.dpi_imports.Empty())
                        ? RenderDpiHeader(decls.dpi_exports, decls.dpi_imports)
                        : std::string{},
  };
}

}  // namespace lyra::lowering::hir_to_mir
