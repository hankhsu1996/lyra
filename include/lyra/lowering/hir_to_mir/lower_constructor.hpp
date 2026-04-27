#pragma once

#include <string>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/class_decl.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerScopeAsClass(
    UnitLoweringState& unit_state, const ClassLoweringState* parent_class_state,
    ScopeStack& stack, const hir::StructuralScope& scope, std::string name)
    -> diag::Result<mir::ClassDecl>;

}  // namespace lyra::lowering::hir_to_mir
