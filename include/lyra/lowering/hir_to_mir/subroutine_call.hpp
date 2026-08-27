#pragma once

#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::hir {
struct CallExpr;
}  // namespace lyra::hir

namespace lyra::lowering::hir_to_mir {

// Lowers a call to a user subroutine -- an intra-unit structural one, a
// cross-unit package / `$unit` one (LRM 26.3), or a class method whether it
// carries a receiver or not (LRM 8.6, 8.10) -- to the expression it yields
// (LRM 13.4, 13.5). The four differ in how the callee is named and in nothing
// after that. A call is an expression wherever it is written: statement
// position is that expression under an expression statement, and
// `lhs = f(...)` is an assignment over it, so neither is a shape of its own.
//
// The expression binds the actuals and calls the callee, typed with the
// protocol the callee states: a task's completion is a coroutine its caller
// awaits, a function's is its payload outright. A callee that writes values
// back needs those writes sequenced after completion, so the call, the writes,
// and the yield become an immediately-invoked closure; one that writes nothing
// back has nothing to sequence and is the call itself.
//
// Returns nullopt for a callee that is not a user subroutine -- a system,
// builtin, imported, or foreign one -- each of which has its own boundary.
// `result_type` is the call's own result type (the enclosing expression's
// type); it is the completion payload's result component unless it is void, in
// which case the callee is a task or void function that contributes none.
template <ExprLowerer Lowerer>
auto LowerSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    mir::TypeId result_type) -> std::optional<diag::Result<mir::Expr>>;

}  // namespace lyra::lowering::hir_to_mir
