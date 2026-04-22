#pragma once

#include <optional>

#include <slang/ast/statements/MiscStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

// Try to recognize and lower a termination system call ($finish, $stop, $exit,
// $fatal). Returns nullopt if the expression is not a termination call,
// allowing the caller to fall through to generic expression lowering.
auto TryLowerTerminationCall(
    const slang::ast::ExpressionStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
