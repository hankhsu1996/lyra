#pragma once

// What a write names (LRM A.8.5): one place, or a join of lvalues. A join is
// written by giving each place it is made of its share of the one value
// written, and read by joining what those places hold, so every kind of write
// -- an assignment now or later, an assignment operator, a continuous
// assignment, an output actual, a takeover -- asks this for the shares and
// makes its own kind of write to each.

#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The members share the bits of one packed vector, the first member most
// significant, each as many as it is wide (LRM 11.4.12).
//
//   {a, b[3:0], {c, d}} = v;
struct PackedJoin {};

// Member `i` takes element `i` of an array or member `i` of a structure (LRM
// 10.9).
//
//   pair_t'{a, b} = v;
struct UnpackedJoin {};

// The value is read as a stream of bits, the order of its `block_bits`-wide
// blocks is reversed, and the members are filled in order from its most
// significant end (LRM 11.4.14.3). A `block_bits` of zero re-orders nothing.
//
//   {<< 8 {a, b}} = v;
struct StreamedJoin {
  std::uint64_t block_bits = 0;
};

using JoinKind = std::variant<PackedJoin, UnpackedJoin, StreamedJoin>;

struct Lvalue;

struct LvalueConcat {
  std::vector<Lvalue> elems;
  JoinKind kind;
};

// `type` is the type of the value the lvalue holds: the place's own, or what
// the join's members make together. `source_type` is that type as the source
// declared it, which is what a value of it is built from.
struct Lvalue {
  std::variant<AccessPath, LvalueConcat> form;
  mir::TypeId type;
  hir::TypeId source_type;
  diag::SourceSpan span;
};

// One place and the value a write gives it.
struct Share {
  AccessPath place;
  mir::ExprId value;
};

// An lvalue a construct reads and then writes: every place of it settled, and
// the value those places hold going in.
struct LvalueReadThenWritten {
  Lvalue lvalue;
  mir::ExprId incoming;
};

// What `expr` names as the target of a write, with the checks each place owes
// before a write lands appended where the statement is reached.
template <ExprLowerer L>
auto LowerLvalue(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<Lvalue>;

// Each place `lvalue` is made of, in the order the source wrote them.
template <typename Visit>
void ForEachPlace(Lvalue& lvalue, Visit visit) {
  std::visit(
      Overloaded{
          [&](AccessPath& place) { visit(place); },
          [&](LvalueConcat& concat) {
            for (Lvalue& elem : concat.elems) {
              ForEachPlace(elem, visit);
            }
          }},
      lvalue.form);
}

template <typename Visit>
void ForEachPlace(const Lvalue& lvalue, Visit visit) {
  std::visit(
      Overloaded{
          [&](const AccessPath& place) { visit(place); },
          [&](const LvalueConcat& concat) {
            for (const Lvalue& elem : concat.elems) {
              ForEachPlace(elem, visit);
            }
          }},
      lvalue.form);
}

// The share of `value` each place of `lvalue` takes. One place takes the value
// as it is. A join holds the value once, as a step of `frame`'s block, and
// cuts it by its kind down to the places.
[[nodiscard]] auto Shares(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<std::vector<Share>>;

// Writes `value` to `lvalue` where `frame` stands: one store per place, each
// of its share, appended to the frame's block. Every place of a join is
// located before the first is written, so a member whose index another
// member's store would change names what it named when the write began.
[[nodiscard]] auto AppendStores(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<void>;

// `lvalue` as an operand that is read and then written (LRM 11.4.1, 13.5
// `inout`): every place settled here, and the value they hold going in, joined
// by kind.
[[nodiscard]] auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, Lvalue lvalue)
    -> diag::Result<LvalueReadThenWritten>;

// Whether `expr` in target position is a join of lvalues rather than one place.
[[nodiscard]] auto IsJoin(const hir::Expr& expr) -> bool;

}  // namespace lyra::lowering::hir_to_mir
