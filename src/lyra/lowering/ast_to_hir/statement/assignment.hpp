#pragma once

#include <slang/ast/expressions/AssignmentExpressions.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentExpression(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::AssignmentExpression& assign) -> hir::StmtId;

}  // namespace lyra::lowering::ast_to_hir
