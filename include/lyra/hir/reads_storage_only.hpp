#pragma once

#include "lyra/base/arena.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"

namespace lyra::hir {

// Whether evaluating `id` does nothing but read storage: it calls nothing,
// writes nothing, and reaches nothing through a class handle or a virtual
// interface, so it can neither change what anything reads nor fail, and nothing
// in it depends on which process runs it. That is what makes where it runs
// unobservable: a wait on such an expression may be decided where a write lands
// rather than by resuming the waiting process (LRM 4.7). Any call counts, since
// what a system function or a built-in method does is not stated here.
[[nodiscard]] auto ReadsStorageOnly(
    const base::Arena<Expr, ExprId>& exprs, ExprId id) -> bool;

// The same, and every place it reads exists from elaboration on: none is a
// variable or a pattern binding a body declares. Such an expression means the
// same wherever in the body it is evaluated, ahead of any declaration the body
// makes included.
[[nodiscard]] auto ReadsElaboratedStorageOnly(
    const base::Arena<Expr, ExprId>& exprs, ExprId id) -> bool;

}  // namespace lyra::hir
