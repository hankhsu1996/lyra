#pragma once

#include <concepts>
#include <span>

#include "lyra/backend/cpp/precedence.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

// An expression, in a position that needs at least `at_least` precedence. The
// arm that picks a node's C++ form is the one that knows that form's
// precedence, so it is also the one that adds parentheses when the position
// needs them.
void RenderExpr(
    const ScopeView& view, const mir::Expr& expr, Precedence at_least,
    TargetText& out);

// An operand and the precedence its position needs. It is parenthesized only
// where its own form binds less tightly: `a - (b - c)` keeps them and
// `a - b - c` needs none, which is what keeps a long chain from nesting one
// level per link.
struct Operand {
  mir::ExprId expr;
  Precedence at_least = Precedence::kAssignment;
};

// A type, as the type mapping spells it.
void WriteType(const ScopeView& view, TargetText& out, mir::TypeId type);

// One piece of a write that lists punctuation and what it wraps in the order
// they are read. An expression id is rendered as a whole expression, an
// `Operand` at the precedence it states, a type through the type mapping, and
// anything else is target text written as it stands.
template <typename Piece>
void WritePiece(const ScopeView& view, TargetText& out, const Piece& piece) {
  if constexpr (std::same_as<Piece, mir::ExprId>) {
    RenderExpr(view, view.Expr(piece), Precedence::kAssignment, out);
  } else if constexpr (std::same_as<Piece, Operand>) {
    RenderExpr(view, view.Expr(piece.expr), piece.at_least, out);
  } else if constexpr (std::same_as<Piece, mir::TypeId>) {
    WriteType(view, out, piece);
  } else {
    WriteOne(out, piece);
  }
}

template <typename... Pieces>
void Write(const ScopeView& view, TargetText& out, const Pieces&... pieces) {
  (WritePiece(view, out, pieces), ...);
}

// The operands, in order, separated by commas, each as a whole expression.
void WriteCommaSeparated(
    const ScopeView& view, TargetText& out,
    std::span<const mir::ExprId> operands);

}  // namespace lyra::backend::cpp
