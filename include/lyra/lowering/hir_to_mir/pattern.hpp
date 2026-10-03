#pragma once

// Realization of HIR patterns (LRM 12.6) as MIR. Shared by every construct that
// matches a value against a pattern -- a clause of a conditional statement or
// expression, and an item of a pattern-matching case statement.

#include "lyra/hir/pattern_id.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/predicate.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Whether the value the local `subject` holds matches `pattern`, which is never
// unknown (LRM 12.6). Evaluating it assigns the identifiers the pattern
// introduces, where it matched. They are declared in `declared_in`, which has
// to enclose whatever reads them: assignment belongs where the pattern has
// already matched, while the declaration has to reach the clauses after this
// one and the arm the predicate selects, and those are not the same block.
template <ExprLowerer Lowerer>
auto PatternPredicate(
    Lowerer& lowerer, const WalkFrame& declared_in, mir::LocalId subject,
    mir::TypeId subject_type, hir::PatternId pattern) -> Predicate;

}  // namespace lyra::lowering::hir_to_mir
