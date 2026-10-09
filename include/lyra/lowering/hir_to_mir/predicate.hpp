#pragma once

// What a conditional construct decides on (LRM 11.4.11, 12.4, 12.6): a value
// read for its truth, which a selection then chooses an arm by. One vocabulary
// serves every construct whose operands are evaluated only where the standard
// calls for them -- the conditional operator, the logical operators that may
// skip an operand, and the conditional and case statements.

#include <cstdint>
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

// The predicate that is true where `predicate` is false, false where it is
// true, and unknown where it is (LRM 11.4.7).
[[nodiscard]] auto Negated(
    const mir::CompilationUnit& unit, Predicate predicate) -> Predicate;

// What ends a search through terms before its last one.
enum class SettledBy : std::uint8_t {
  // `||`: a true term makes the answer 1, and an unknown one leaves it to the
  // terms after it (LRM 11.4.7).
  kTrueTerm,
  // `&&`: a false term makes the answer 0, and an unknown one leaves it to the
  // terms after it (LRM 11.4.7).
  kFalseTerm,
  // `&&&`: a term that is not true is the answer, false or unknown (LRM
  // 12.6.2). The standard leaves open whether an unknown term ends the search;
  // ending it is what the front end's own constant evaluation does, so an
  // expression means the same folded or run.
  kTermNotTrue,
};

// The truth, at `type`, of `terms` searched in order: a term is evaluated only
// where the ones before it left the answer open (LRM 11.3.5), and the answer is
// what the operator's table makes of the terms that were. A search of any
// length is a run of steps one after another, never a step inside the one
// before it. At least two terms: a search of one settles nothing.
[[nodiscard]] auto BuildSearch(
    const mir::CompilationUnit& unit, const WalkFrame& frame, SettledBy rule,
    std::span<const Predicate> terms, mir::TypeId type)
    -> diag::Result<mir::ExprId>;

// A series of predicates as one: a sequential conjunction (LRM 12.6.2, 12.6.3),
// the search its terms make. A lone term is itself.
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

// One arm of a selection: the predicate that selects it and the value it
// answers with.
struct SelectionArm {
  Predicate predicate;
  Evaluation value;
};

// The selection among `arms`, tried in order, and `otherwise` where none is
// selected (LRM 11.4.11), every value answering at `result_type`. A true
// predicate selects its arm and ends the selection; a false one passes it on,
// so a predicate and a value are evaluated only where the selection reaches
// them. An unknown one selects neither its arm nor what follows: the selection
// goes on, and its arm's value is combined bit by bit with whatever the rest
// answers. Only a predicate whose type can hold an unknown has that outcome.
// A selection of any number of arms is as deep as one of a single arm. At
// least one arm.
[[nodiscard]] auto BuildSelection(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::span<const SelectionArm> arms, const Evaluation& otherwise,
    mir::TypeId result_type) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
