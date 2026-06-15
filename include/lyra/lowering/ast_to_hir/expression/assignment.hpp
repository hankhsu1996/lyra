#pragma once

// Lowering of assignment-shaped expressions: Assignment (LRM 11.4.1),
// IncDec (LRM 11.4.2 increment/decrement), plus the shared lvalue-assignability
// validation walker (LRM 10.3, 11.4.12 destructuring).

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace slang::ast {
class AssignmentExpression;
class Expression;
class UnaryExpression;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

auto LowerAssignmentExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::AssignmentExpression& as, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

auto LowerIncDecExprProc(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::UnaryExpression& un, diag::SourceSpan span)
    -> diag::Result<hir::Expr>;

// Walks a slang assignment-target expression and rejects any form that is
// not addressable. `procedural_context` distinguishes the rule for procedural
// (process / subroutine body) targets from continuous-assign targets
// (LRM 10.3 -- structural-var only).
auto ValidateAssignableImpl(
    ModuleLowerer& module, bool procedural_context,
    const slang::ast::Expression& expr) -> diag::Result<void>;

}  // namespace lyra::lowering::ast_to_hir
