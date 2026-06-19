#pragma once

#include <optional>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Walks through container-access CallExprs to the LHS chain's root primary.
[[nodiscard]] auto FindLhsRootId(
    const mir::ProceduralScope& scope, mir::ExprId lhs_id) -> mir::ExprId;

// Rebuilds the LHS chain with its root replaced by
// `DerefExpr(CallExpr(ObservableMethod{kMutate}, [cell, services]))`. The
// returned chain is value-typed; the `ScopedMutation` destructor commits via
// `Var::Set` at full-expression end. Precondition: the LHS root MIR type
// satisfies `IsObservableCellType`.
[[nodiscard]] auto RewriteLhsRootWithMutate(
    const mir::CompilationUnit& unit, mir::ProceduralScope& scope,
    mir::ExprId lhs_id, mir::ExprId services_id) -> mir::ExprId;

// Builds an `lhs op= rhs` (or simple `lhs = rhs`) expression, routing
// through the observable wrapper API when the LHS root is an observable
// cell. Result shape:
//   non-observable                    -> `AssignExpr{lhs, rhs, op?}`
//   observable bare cell, simple `=`  -> `Call(ObservableMethod{kSet}, ...)`
//   observable, anything else         -> `AssignExpr{rewritten_lhs, rhs, op?}`
//                                        with the root wrapped in
//                                        `Deref(Call(kMutate))`.
// `services_id` is consumed only on the observable paths.
[[nodiscard]] auto BuildObservableAssignExpr(
    const mir::CompilationUnit& unit, mir::ProceduralScope& scope,
    mir::ExprId services_id, mir::ExprId lhs_id, mir::ExprId rhs_id,
    std::optional<mir::BinaryOp> compound_op, mir::TypeId result_type,
    mir::TypeId void_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
