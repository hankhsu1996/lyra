#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

// Build a frozen FunctionSignature from HIR function metadata.
// Must be called at pre-allocation time (Phase 1).
auto BuildFunctionSignature(
    const hir::Function& function, const SymbolTable& symbol_table)
    -> mir::FunctionSignature;

auto LowerFunctionBody(
    const hir::Function& function, const LoweringInput& input,
    mir::Arena& mir_arena, const DeclView& decl_view, OriginMap* origin_map)
    -> Result<mir::Function>;

}  // namespace lyra::lowering::hir_to_mir
