#pragma once

#include <span>
#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_scope.hpp"

namespace lyra::lowering::hir_to_mir {

// One structural parameter the child scope receives at construction. The
// `param` is the MIR decl that will be installed on the child scope; the
// `source_loop_var` records which HIR loop variable (if any) maps to it.
// Today only generate-for child scopes use this; future HIR
// `parameter`/`localparam` will reuse the same struct with a different
// source mapping.
struct ScopeEntryStructuralParamBinding {
  mir::StructuralParamDecl param;
  hir::LoopVarDeclId source_loop_var = {};
};

auto LowerStructuralScope(
    UnitLoweringState& unit_state,
    const StructuralScopeLoweringState* parent_scope_state, ScopeStack& stack,
    const hir::StructuralScope& scope, std::string name,
    std::span<const ScopeEntryStructuralParamBinding> entry_bindings = {})
    -> diag::Result<mir::StructuralScope>;

}  // namespace lyra::lowering::hir_to_mir
