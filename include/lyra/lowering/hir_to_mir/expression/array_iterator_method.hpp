#pragma once

// Lowering of the LRM 7.12 array methods that evaluate an expression at each
// entry. Each is a loop over the array's entries with that expression
// evaluated at every one, so what a `with` clause answers is ordinary code of
// the body the call is in, of the type the clause has, and nothing is handed
// to a library to call back. A sort is the one that leaves its last step to
// the library: putting the entries in an order, which it is told by a
// comparison of two entries the program answers.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The call `c` of the method `ref` names, answering at `result_type`. The
// meaning is independent of the enclosing scope, so one template over the pass
// class serves both contexts; explicit instantiations live in the
// implementation file.
template <ExprLowerer Lowerer>
auto LowerArrayIteratorMethod(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ArrayIteratorMethodRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
