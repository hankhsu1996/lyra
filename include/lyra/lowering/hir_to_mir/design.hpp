#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/compiled_bindings.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::hir_to_mir {

// Collect design-global placement-facing declarations only.
// Responsible for package/global storage, per-instance design-global places,
// instance_slot_ranges, instance_param_inits, and design-global descriptor
// tables. Does NOT produce specialization-local body declarations.
auto CollectDesignDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations;

// Collect specialization-local storage for one module.
// Allocates kModuleSlot places (0-based) independently of design-global
// state. No access to design-global slot counters or instance ordering.
auto CollectBodyLocalDecls(
    const hir::Module& module, const SymbolTable& symbol_table,
    mir::Arena& mir_arena) -> BodyLocalDecls;

struct DesignLoweringResult {
  mir::Design design;
  mir::CompiledBindingPlan compiled_bindings;
};

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map)
    -> Result<DesignLoweringResult>;

}  // namespace lyra::lowering::hir_to_mir
