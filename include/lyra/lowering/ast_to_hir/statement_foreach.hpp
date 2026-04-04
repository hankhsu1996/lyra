#pragma once

#include <optional>

#include <slang/ast/statements/LoopStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

// Lower a foreach loop statement by desugaring into nested for loops.
// Handles both fixed-size and dynamic arrays, as well as associative array
// snapshot-based iteration.
auto LowerForeachStatement(
    const slang::ast::ForeachLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
