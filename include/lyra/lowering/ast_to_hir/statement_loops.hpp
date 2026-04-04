#pragma once

#include <optional>

#include <slang/ast/statements/LoopStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

auto LowerForLoopStatement(
    const slang::ast::ForLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

auto LowerWhileLoopStatement(
    const slang::ast::WhileLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

auto LowerDoWhileLoopStatement(
    const slang::ast::DoWhileLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

auto LowerForeverLoopStatement(
    const slang::ast::ForeverLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

auto LowerRepeatLoopStatement(
    const slang::ast::RepeatLoopStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
