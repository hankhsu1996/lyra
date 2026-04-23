#pragma once

#include <slang/ast/Statement.h>

#include "../facts.hpp"
#include "../state.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerStatement(
    const ProcessLoweringFacts& facts, ProcessLoweringState& state,
    const ModuleLoweringState& module_state, const slang::ast::Statement& stmt)
    -> hir::StmtId;

}  // namespace lyra::lowering::ast_to_hir
