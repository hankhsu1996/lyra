#pragma once

#include <optional>

#include <slang/ast/Statement.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

// Lowers a slang statement to HIR.
// Returns:
//   - std::nullopt: empty statement (caller should skip)
//   - StatementId with !id: lowering failed (error was reported)
//   - StatementId with id: success
auto LowerStatement(const slang::ast::Statement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
