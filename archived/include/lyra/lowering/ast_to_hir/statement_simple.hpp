#pragma once

#include <optional>

#include <slang/ast/Statement.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

auto LowerListStatement(
    const slang::ast::StatementList& list, const slang::ast::Statement& stmt,
    ScopeLowerer& lowerer) -> std::optional<hir::StatementId>;

auto LowerVariableDeclarationStatement(
    const slang::ast::VariableDeclStatement& stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

auto LowerExpressionStatement(
    const slang::ast::ExpressionStatement& expr_stmt, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

auto LowerReturnStatement(
    const slang::ast::ReturnStatement& ret, ScopeLowerer& lowerer)
    -> std::optional<hir::StatementId>;

}  // namespace lyra::lowering::ast_to_hir
