#pragma once

// Lowering of `AssignExpr` (LRM 10.4) plus the closure-build helpers a
// non-blocking assignment needs (LRM 9.2.3 NBA region snapshot).
// `AssignExpr` has no structural form -- continuous assignment is its own
// scope-level construct, not an expression -- so this family is procedural
// only.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirAssignExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Builds an NBA-region closure: snapshots `rhs_id_in_outer` by value into
// the body and writes it through `lhs_in_outer`. The returned Expr has type
// `void`. `lhs_in_outer` must be addressable and rooted at a structural var
// (procedural-local NBA is not supported). Used by the LHS-destructuring
// desugar, which submits each part as its own NBA closure.
auto BuildNbaSubmitClosureExpr(
    ModuleLowerer& module, WalkFrame frame, mir::ExprId lhs_in_outer,
    mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
