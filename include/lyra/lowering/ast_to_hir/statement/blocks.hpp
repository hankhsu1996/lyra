#pragma once

// Lowering of sequential / parallel block statements: BlockStatement
// (LRM 9.3.1 begin-end / 9.3.2 fork-join), StatementList (the embedded list
// form), and the fork-join branch construction shared by both.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class BlockStatement;
class StatementList;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerStatementListStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::StatementList& list, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

auto LowerBlockStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
