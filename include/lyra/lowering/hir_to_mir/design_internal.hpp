#pragma once

#include <functional>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

// Create a design-level context for port binding lowering.
// Port bindings only reference design-level symbols, so module_places
// points to design_places.
auto MakeDesignContext(
    const LoweringInput& input, mir::Arena& mir_arena,
    const DesignDeclarations& decls) -> Context;

// Callback type that builds the source operand inside a provided builder.
// This ensures any temps/instructions are emitted into the correct builder.
using BuildSrcFn =
    std::function<Result<mir::Operand>(MirBuilder&, const Context&)>;

// Create a synthetic always_comb process for port connection.
// Uses callback pattern to build source operand inside the process's builder.
auto CreateConnectionProcess(
    mir::PlaceId dst, BuildSrcFn build_src, const LoweringInput& input,
    mir::Arena& mir_arena, const DesignDeclarations& decls)
    -> Result<mir::ProcessId>;

}  // namespace lyra::lowering::hir_to_mir

namespace lyra::lowering::ast_to_hir {
struct DesignBindingPlan;
}  // namespace lyra::lowering::ast_to_hir

namespace lyra::mir {
struct Design;
}  // namespace lyra::mir

namespace lyra::lowering::hir_to_mir {

// Apply port bindings to the design.
// Creates connection processes and populates alias_map.
auto ApplyBindings(
    const ast_to_hir::DesignBindingPlan& plan, const DesignDeclarations& decls,
    const LoweringInput& input, mir::Arena& mir_arena, mir::Design& design)
    -> Result<void>;

}  // namespace lyra::lowering::hir_to_mir
