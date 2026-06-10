#pragma once

// Lowering of branching statements:
//   - Conditional / if-else (LRM 12.4)
//   - Case (LRM 12.5)
//   - CaseInside (LRM 12.5.4)

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class CaseStatement;
class ConditionalStatement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerConditionalStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ConditionalStatement& cs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

auto LowerCaseStmt(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::CaseStatement& cs,
    diag::SourceSpan span) -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
