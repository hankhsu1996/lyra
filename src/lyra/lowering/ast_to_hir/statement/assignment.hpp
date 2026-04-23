#pragma once

#include <slang/ast/expressions/AssignmentExpressions.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentExpression(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    const ModuleLoweringState& module_state,
    const slang::ast::AssignmentExpression& assign) -> hir::StmtId;

}  // namespace lyra::lowering::ast_to_hir
