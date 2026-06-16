#pragma once

// Lowering of operator-family expressions (LRM 11.4): unary, binary,
// conditional (`?:`), conversion, and increment / decrement.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Translation table from HIR binary-op enum to MIR binary-op enum. Shared
// with compound assignment lowering, which routes its `compound_op` field
// through this same mapping (the AST-to-HIR validator rejects comparison /
// logical compound ops upstream, so by the time we get here every `BinaryOp`
// is a legitimate target).
auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp;

// Procedural-context handlers.
auto LowerHirUnaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirBinaryExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirConditionalExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirIncDecExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;
auto LowerHirConversionExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

// Structural-context handlers (constructor / generate-control / continuous
// assign). `IncDecExpr` and `AssignExpr` have no structural form -- those
// kinds are diagnosed at the dispatcher.
auto LowerHirUnaryExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::UnaryExpr& u, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirBinaryExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::BinaryExpr& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirConditionalExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ConditionalExpr& c, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;
auto LowerHirConversionExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ConversionExpr& cv, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
