#pragma once

#include <optional>

#include <slang/ast/statements/MiscStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

// Lower a plain non-deferred immediate assert statement.
// Rejects deferred/final forms and non-assert kinds (assume, cover).
auto LowerImmediateAssertionStatement(
    const slang::ast::ImmediateAssertionStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
