#pragma once

// Lowering of looping statements (LRM 12.7):
//   - for loop (12.7.1)
//   - while / do-while loops (12.7.2)
//   - repeat loop (12.7.2)
//   - forever loop (12.7.2)
// foreach is handled separately in foreach.cpp because of its closure-shaped
// rewrite into a counter-driven for loop.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class DoWhileLoopStatement;
class ForLoopStatement;
class ForeverLoopStatement;
class RepeatLoopStatement;
class WhileLoopStatement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerForLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ForLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;
auto LowerWhileLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::WhileLoopStatement& ws, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;
auto LowerRepeatLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::RepeatLoopStatement& rs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;
auto LowerDoWhileLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::DoWhileLoopStatement& ds, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;
auto LowerForeverLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ForeverLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
