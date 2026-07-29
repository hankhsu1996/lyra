#pragma once

// Lowering of tagged-union expressions (LRM 7.3.2 / 11.9) and pattern-matching
// conditional expressions (LRM 12.6.3). Statement-form pattern matching lives
// in `statement/branches.hpp`; both share the recursive pattern desugar
// helper in the implementation.

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// `tagged Member primary` (LRM 11.9). Independent of the enclosing scope, so
// one template over the pass class serves both procedural and structural
// contexts. `union_type` is the target's HIR type: the two representations
// build a value differently, and only the source-level type says which one is
// in play.
template <ExprLowerer Lowerer>
auto LowerHirTaggedUnionExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::TaggedUnionExpr& t,
    hir::TypeId union_type, mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
