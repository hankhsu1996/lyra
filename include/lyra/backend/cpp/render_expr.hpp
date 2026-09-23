#pragma once

#include <concepts>
#include <span>

#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

void RenderExpr(const ScopeView& view, const mir::Expr& expr, TargetText& out);

// A type written where the program names one, which is the spelling the type
// mapping answers with.
void WriteType(const ScopeView& view, TargetText& out, mir::TypeId type);

// One piece of a contribution. An operand's render and a type's spelling land
// where the piece sits; everything else is target syntax and is written as it
// stands. Listing the two together is what keeps punctuation and what it wraps
// in one place and in the order they are read.
template <typename Piece>
void WritePiece(const ScopeView& view, TargetText& out, const Piece& piece) {
  if constexpr (std::same_as<Piece, mir::ExprId>) {
    RenderExpr(view, view.Expr(piece), out);
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

// The operands, in order, with the separator an argument list puts between
// them. The empty list writes nothing and asks nobody whether it is empty.
void WriteCommaSeparated(
    const ScopeView& view, TargetText& out,
    std::span<const mir::ExprId> operands);

}  // namespace lyra::backend::cpp
