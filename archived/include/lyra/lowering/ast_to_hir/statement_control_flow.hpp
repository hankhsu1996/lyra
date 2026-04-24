#pragma once

#include <optional>

#include <slang/ast/statements/ConditionalStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

// Lower an if/else statement with optional unique/priority qualifier.
auto LowerConditionalStatement(
    const slang::ast::ConditionalStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

// Lower a case/casez/casex/case-inside statement with optional qualifier.
auto LowerCaseStatement(
    const slang::ast::CaseStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
