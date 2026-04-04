#pragma once

#include <optional>

#include <slang/ast/statements/MiscStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

// Lower a timed statement (delay control, signal event, event list).
auto LowerTimedStatement(
    const slang::ast::TimedStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
