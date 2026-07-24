#pragma once

#include <optional>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Walks through container-access CallExprs and member-ref projections to the
// LHS chain's root primary. A projection whose own type is already an
// observable cell is a captured carrier (a closure-record field holding a
// `Ref`), which is itself the root: the walk stops there rather than descending
// through the field access as if the field were a struct member.
[[nodiscard]] auto FindLhsRootId(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId lhs_id) -> mir::ExprId;

// Whether the LHS chain's root is an observable storage cell. This is the
// condition under which a write notifies subscribers, so it decides whether
// the write needs a runtime handle at all: a write to a plain (non-cell)
// local has no subscribers and reaches no runtime. A caller supplies the
// write's runtime handle exactly when this holds.
[[nodiscard]] auto LhsRootIsObservableCell(
    const mir::CompilationUnit& unit, const mir::Block& block,
    mir::ExprId lhs_id) -> bool;

// Rebuilds the LHS chain with its root replaced by
// `DerefExpr(CallExpr(ObservableMethod{kMutate}, [cell, runtime]))`. The
// returned chain is value-typed; the `ScopedMutation` destructor commits via
// `Var::Set` at full-expression end. Precondition: the LHS root MIR type
// satisfies `IsObservableCellType`.
[[nodiscard]] auto RewriteLhsRootWithMutate(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId lhs_id,
    mir::ExprId runtime_id) -> mir::ExprId;

// Builds an `lhs op= rhs` (or simple `lhs = rhs`) expression, routing
// through the observable wrapper API when the LHS root is an observable
// cell. Result shape:
//   non-observable                    -> `AssignExpr{lhs, rhs, op?}`
//   observable bare cell, simple `=`  -> `Call(ObservableMethod{kSet}, ...)`
//   observable, anything else         -> `AssignExpr{rewritten_lhs, rhs, op?}`
//                                        with the root wrapped in
//                                        `Deref(Call(kMutate))`.
// `runtime_id` is the write's runtime handle, present exactly on the
// observable paths (`LhsRootIsObservableCell`): a plain-local write carries
// none, which is what lets a caller unable to reach the runtime -- a
// receiver-less namespace callable, whose locals are never observable --
// lower its assignments. Passing no runtime handle for an observable target
// is a caller error.
[[nodiscard]] auto BuildObservableAssignExpr(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::optional<mir::ExprId> runtime_id, mir::ExprId lhs_id,
    mir::ExprId rhs_id, std::optional<mir::BinaryOp> compound_op,
    mir::TypeId result_type, mir::TypeId void_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
