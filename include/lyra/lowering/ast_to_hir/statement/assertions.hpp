#pragma once

// Lowering of assertion statements:
//   - Simple immediate assert / assume / cover (LRM 16.3)

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class ImmediateAssertionStatement;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerImmediateAssertionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ImmediateAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
