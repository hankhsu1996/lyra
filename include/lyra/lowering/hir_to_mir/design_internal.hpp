#pragma once

#include <functional>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/design_assembly/compiled_bindings.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/routine.hpp"

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
// Returns a Process value (not registered in arena - assembly does that).
auto CreateConnectionProcess(
    mir::PlaceId dst, BuildSrcFn build_src, const LoweringInput& input,
    mir::Arena& mir_arena, const DesignDeclarations& decls)
    -> Result<mir::Process>;

}  // namespace lyra::lowering::hir_to_mir

namespace lyra::lowering::ast_to_hir {
struct DesignBindingPlan;
}  // namespace lyra::lowering::ast_to_hir

namespace lyra::lowering::hir_to_mir {

// Compile port bindings into assembly-ready artifacts.
// Does NOT mutate mir::Design -- assembly attachment is separate.
auto CompileBindings(
    const ast_to_hir::DesignBindingPlan& plan, const DesignDeclarations& decls,
    const LoweringInput& input, mir::Arena& mir_arena)
    -> Result<design_assembly::CompiledBindingPlan>;

}  // namespace lyra::lowering::hir_to_mir
