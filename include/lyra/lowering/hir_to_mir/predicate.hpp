#pragma once

// What a conditional construct decides on (LRM 11.4.11, 12.4, 12.6): a value
// read for its truth, which a selection then chooses an arm by. One vocabulary
// serves every construct whose operands are evaluated only where the standard
// calls for them -- the conditional operator, the logical operators that may
// skip an operand, and the conditional and case statements.

#include <functional>
#include <span>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Lowers one expression into the block of the frame it is handed and answers
// with it there. Whatever steps evaluating it takes -- a value held for reuse,
// a check before an access -- go into that block too, so an expression the run
// evaluates only on some runs is handed a block of its own.
using Evaluation = std::function<diag::Result<mir::ExprId>(const WalkFrame&)>;

// Lowers `operand` into a block expression of its own and adds that to
// `frame`'s block. A construct calls this for an operand it evaluates on only
// some runs -- an arm of `?:`, the second operand of `&&` -- so that a local
// the operand binds, or a check it makes, sits inside the operand and runs
// only when the operand does (LRM 11.3.5), as each arm of a Rust `if` is a
// block.
[[nodiscard]] auto ConditionallyEvaluated(
    const WalkFrame& frame, const Evaluation& operand)
    -> diag::Result<mir::ExprId>;

// A predicate as a value read for its truth: the type that value has, which
// says whether it can be unknown, and what evaluates it. The type is known
// without evaluating anything, so a construct settles its own shape from it
// first.
struct Predicate {
  mir::TypeId type;
  Evaluation evaluate;
};

// A value as the one-bit value of its truth (LRM 11.4.7). An integral value is
// true when a bit of it is known to be 1, false when every bit is known to be
// 0, and unknown otherwise. Any other value is read the way a condition reads
// it -- a real is true when it is nonzero (LRM 11.3.1), a handle when it names
// an object (LRM 8.4) -- and that reading cannot be unknown.
[[nodiscard]] auto BuildTruth(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId;

// An expression of the source as a predicate: its own value.
template <ExprLowerer Lowerer>
auto ExpressionPredicate(Lowerer& lowerer, hir::ExprId id) -> Predicate;

// A series of predicates as one: a sequential conjunction (LRM 12.6.2, 12.6.3).
// A term is evaluated only once every one before it was true, so every term
// after the first is conditionally evaluated, and the series is the truth of
// the first that is not -- false, or unknown, which makes the series unknown as
// a whole. The standard leaves open whether an unknown term ends the series;
// ending it is what the front end's own constant evaluation does, so an
// expression means the same folded or run. A lone term is itself.
[[nodiscard]] auto SeriesPredicate(
    const mir::CompilationUnit& unit, std::vector<Predicate> terms)
    -> Predicate;

// A predicate written as clauses joined by `&&&`: the series of them, a clause
// that matches a pattern being the match of its expression's value. The
// identifiers its patterns introduce are declared in `declared_in`, which has
// to enclose whatever reads them.
template <ExprLowerer Lowerer>
auto ClauseSeriesPredicate(
    Lowerer& lowerer, const WalkFrame& declared_in,
    std::span<const hir::ConditionClause> clauses) -> Predicate;

// The selection between two arms by one predicate (LRM 11.4.11), each arm
// answering at `result_type`. A predicate that is true or false evaluates the
// arm it selects and never the other, so each arm is conditionally evaluated.
// One that is unknown selects neither: both are evaluated and their results
// combined bit by bit, and only a predicate whose type can hold an unknown has
// that outcome.
[[nodiscard]] auto BuildSelection(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const Predicate& predicate, mir::TypeId result_type,
    const Evaluation& then_arm, const Evaluation& else_arm)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
