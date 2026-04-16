#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

// Collect design-global placement-facing declarations only.
// Responsible for package/global storage, per-instance design-global places,
// instance_slot_ranges, and design-global descriptor tables.
// instance_param_inits are transient: consumed during lowering to build
// InstanceConstBlock in PlacementMap, not stored in mir::Design.
// Does NOT produce specialization-local body declarations.
auto CollectDesignDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations;

// Collect specialization-local storage for one module.
// Allocates kModuleSlot places (0-based) independently of design-global
// state. No access to design-global slot counters or instance ordering.
auto CollectBodyLocalDecls(
    const hir::Module& module, const SymbolTable& symbol_table,
    const TypeArena& type_arena, mir::Arena& mir_arena) -> BodyLocalDecls;

struct DesignLoweringResult {
  mir::Design design;
  mir::ConstructionInput construction;
  std::vector<mir::BoundConnection> bound_connections;
  // Per-body origin entries, indexed by ModuleBodyId.
  // Body-local MIR origins stay body-local -- not merged into the
  // design-global origin map.
  std::vector<std::vector<OriginEntry>> body_origins;
  // DPI export wrapper descriptors for LLVM backend emission.
  // Deterministically sorted by c_name. Built from DesignDeclarations
  // export registry, not from mir::Design.
  std::vector<mir::DpiExportWrapperDesc> dpi_export_wrappers;
  // Generated DPI header content. Empty if no DPI symbols exist.
  std::string dpi_header;
};

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map)
    -> Result<DesignLoweringResult>;

}  // namespace lyra::lowering::hir_to_mir
