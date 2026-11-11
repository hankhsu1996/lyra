#pragma once

#include <optional>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::lowering::hir_to_mir {

// One piece of a sampled value function's operand whose past values a history
// keeps, a value per tick of its clock (LRM 16.9.3). A read of the operand as
// of an earlier tick evaluates the operand where the call stands, and answers
// each such piece with what the history kept.
//
// `through` is what the piece is read through when it stands inside further
// sampled value functions of the operand, outermost first, each named by its
// call: what is kept at each tick is then the piece's value as of the inner
// function's own earlier tick, which that function's history holds.
struct KeptPiece {
  hir::ExprId piece;
  std::vector<hir::ExprId> through;

  auto operator==(const KeptPiece&) const -> bool = default;
};

// Whether `kind` is a constant cast, whose sampled value is its argument's
// current value (LRM 16.5.1) and whose value a procedural concurrent assertion
// saves (LRM 16.14.6.1).
[[nodiscard]] auto IsConstCast(hir::ConversionKind kind) -> bool;

// The variable `expr` reads, where it is a read of a variable a body declares.
[[nodiscard]] auto ProceduralVarRead(const hir::Expr& expr)
    -> std::optional<hir::ProceduralVarId>;

// Whether `expr` reads a variable a body declares with automatic lifetime -- a
// `ref` formal among them -- whose sampled value is its current one (LRM
// 16.5.1). `body` is null among a scope's own expressions, which name none.
[[nodiscard]] auto ReadsAutomaticVariable(
    const hir::ProceduralBody* body, const hir::Expr& expr) -> bool;

// What the history of the sampled value function `call` keeps. A piece is a
// largest part of the operand that reads a variable the standard samples, and
// nothing read as it is now -- an automatic variable, a constant cast, a value
// an iteration or a pattern binds
// -- and makes no call. Operators and selects compute the same over the values
// of an earlier tick whenever they run, so their result is what is kept; a
// call is made where the operand is read, over its arguments' past values
// (LRM 16.5.1), since what it answers may follow what its arguments do not
// name.
[[nodiscard]] auto KeptPieces(const hir::ProceduralBody& body, hir::ExprId call)
    -> std::vector<KeptPiece>;
// The same, for a function written among a scope's own expressions.
[[nodiscard]] auto KeptPieces(
    const hir::StructuralScope& scope, hir::ExprId call)
    -> std::vector<KeptPiece>;

// The event whose ticks `call` counts, where it is a sampled value function
// that reaches across them (LRM 16.9.3), and nothing for any other expression.
[[nodiscard]] auto SampledClockOf(const hir::Expr& call)
    -> const hir::EventControl*;

// The operand of the sampled value function `call`.
[[nodiscard]] auto SampledOperandOf(const hir::Expr& call) -> hir::ExprId;

// Every sampled value function among `exprs` that keeps a history, in the
// order the arena holds them, which puts each after those in its own operand.
[[nodiscard]] auto SampledValueCalls(
    const base::Arena<hir::Expr, hir::ExprId>& exprs)
    -> std::vector<hir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
