#pragma once

#include <slang/ast/Expression.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/expr.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerExpression(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Expression& expr) -> hir::ExprId;

auto LowerStructuralExpression(
    ScopeLoweringState& scope_state, ScopeStack& stack,
    const slang::ast::Expression& expr) -> hir::ExprId;

}  // namespace lyra::lowering::ast_to_hir
