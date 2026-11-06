#pragma once

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class AssignmentExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether `as` is a synchronous drive: a nonblocking assignment whose target
// names an output clockvar (LRM 14.16). It lowers to a nonblocking assignment
// of where the output writes, carrying the drive's control.
[[nodiscard]] auto IsClockingDrive(const slang::ast::AssignmentExpression& as)
    -> bool;

auto LowerClockingDrive(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt>;

}  // namespace lyra::lowering::ast_to_hir
