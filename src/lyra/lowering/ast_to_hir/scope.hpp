#pragma once

#include <slang/ast/Scope.h>

#include "lyra/hir/structural_scope.hpp"
#include "state.hpp"

namespace lyra::lowering::ast_to_hir {

void LowerScope(
    UnitLoweringState& unit, hir::StructuralScope& scope,
    const slang::ast::Scope& slang_scope, ScopeStack& stack);

}  // namespace lyra::lowering::ast_to_hir
