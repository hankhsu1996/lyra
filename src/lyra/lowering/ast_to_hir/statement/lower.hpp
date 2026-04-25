#pragma once

#include <slang/ast/Statement.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerStatement(
    const UnitLoweringFacts& unit_facts, ProcessLoweringState& proc_state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Statement& stmt) -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
