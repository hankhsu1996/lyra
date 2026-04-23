#pragma once

#include <slang/ast/Expression.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/expr.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerExpression(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    const ModuleLoweringState& module_state, const slang::ast::Expression& expr)
    -> hir::ExprId;

}  // namespace lyra::lowering::ast_to_hir
