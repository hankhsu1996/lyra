#pragma once

// Lowering of access primitives (LRM 11.5 select expressions): element-,
// range-, and member-select. Member-access on a packed struct / union
// resolves to a `RangeSelectExpr` over the aggregate's bit plane (LRM
// 7.2.1) -- MIR carries no struct-specific node.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerHirElementSelectExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirRangeSelectExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirMemberAccessExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

auto LowerHirElementSelectExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ElementSelectExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirRangeSelectExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::RangeSelectExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirMemberAccessExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::MemberAccessExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
