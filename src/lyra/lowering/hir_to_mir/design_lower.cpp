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

  mir::ImmediateCoverSiteRegistry cover_site_registry;
  mir::DeferredAssertionSiteRegistry deferred_assertion_site_registry;

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
  // Each call produces an isolated MirBodyLoweringResult with body-local
  // MIR arena and body-local origins. No body-lowering path writes to the
  // design arena.
  std::vector<Result<MirBodyLoweringResult>> body_results;
  body_results.reserve(spec_map.groups.size());

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

    // Enter body-local artifact domain: HIR, constants, and MIR all
    // switch to body-owned storage for this specialization group.
    LoweringInput body_input = input;
    body_input.hir_arena = &hir_body.arena;
    body_input.active_constant_arena = &hir_body.constant_arena;

    mir::Arena body_arena;
    BodyLocalDecls body_decls = CollectBodyLocalDecls(
        rep_mod, *input.symbol_table, *input.type_arena, body_arena);

    body_results.push_back(LowerModule(
        hir_body, body_input, std::move(body_arena), mir_arena, decls,
        body_decls, rep_mod.body_id, &cover_site_registry,
        &deferred_assertion_site_registry));
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

  for (size_t g = 0; g < body_results.size(); ++g) {
    if (!body_results[g]) {
      return std::unexpected(body_results[g].error());
    }
    auto& product = *body_results[g];

    mir::ModuleBodyId body_id{
        static_cast<uint32_t>(result.module_bodies.size())};
    result.module_bodies.push_back(std::move(product.body));
    body_origins.push_back(std::move(product.origins));
    body_function_maps[body_id.value] = std::move(product.symbol_to_function);
    spec_to_body[spec_groups[g].spec_id] = body_id;
  }

  result.immediate_cover_sites = cover_site_registry.TakeSites();
  result.deferred_assertion_sites =
      deferred_assertion_site_registry.TakeSites();

  // Build DPI export wrapper descriptors now that body-local function maps
  // are available. Module-scoped exports need these maps to resolve their
  // target FunctionId.
  {
    // Collect module-scoped export targets with multi-body collision
    // detection. For each module-scoped export symbol, collect all
    // (body_id, function_id) pairs across specialization groups.
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
                .body_id = body_id, .function_id = func_it->second});
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
      result.elements.emplace_back(
          mir::Module{.instance_sym = mod.symbol, .body_id = it->second});

      const auto& body = result.module_bodies.at(it->second.value);
      auto slot_count = static_cast<uint32_t>(body.slots.size());

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

  // Compile port bindings into assembly-ready artifacts (no design mutation).
  mir::CompiledBindingPlan compiled_bindings;
  if (input.binding_plan != nullptr) {
    auto binding_result =
        CompileBindings(*input.binding_plan, decls, input, mir_arena);
    if (!binding_result) return std::unexpected(binding_result.error());
    compiled_bindings = std::move(*binding_result);
  }

  // Propagate decision owner acceptance through design-global call graph.
  mir_arena.PropagateDeferredOwnerAbi();

  return DesignLoweringResult{
      .design = std::move(result),
      .construction = std::move(construction),
      .compiled_bindings = std::move(compiled_bindings),
      .body_origins = std::move(body_origins),
      .dpi_export_wrappers = std::move(dpi_export_wrappers),
      .dpi_header = (!decls.dpi_exports.Empty() || !decls.dpi_imports.Empty())
                        ? RenderDpiHeader(decls.dpi_exports, decls.dpi_imports)
                        : std::string{},
  };
}

}  // namespace lyra::lowering::hir_to_mir
