#pragma once

#include <cstdint>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// How a select's receiver numbers what the select reaches, read off its static
// type. A source index is written in the coordinates the declaration chose
// (LRM 7.4.5, 11.5.1); what the value below is reached by is its own numbering
// from zero, so this is the whole of what turns one into the other. A packed
// value counts bits from its least significant end, and an index into its
// outermost dimension moves one element's width of bits per step; an unpacked
// array counts elements from its left bound (LRM 7.6); an array sized while the
// program runs, and a string, are declared zero-based, so an index into one is
// already its position.
struct PositionMap {
  // The declared index that stands at position zero.
  std::int64_t origin;
  // Whether positions grow as indices fall, as they do toward the right bound
  // of an ascending packed range and toward the right of a descending unpacked
  // one.
  bool reversed;
  // How many positions one index step covers.
  std::int64_t step;
  // Whether position zero is at the declaration's right bound -- a packed
  // value's least significant end -- rather than its left. A constant range
  // runs in the declaration's own direction (the front end holds it to that),
  // so this is also which of its two bounds names the run's lowest position.
  bool from_right;
};

[[nodiscard]] auto PositionMapOf(
    const mir::CompilationUnit& unit, mir::TypeId receiver) -> PositionMap;

// The position `index` names under `map`, moved by `shift` positions. Where the
// map is the identity and nothing is moved, the index is the position as it
// stands, whatever its width, since reading a position already answers for a
// value too wide to name one; otherwise the index is brought to the position
// type and moved there, where no shift a declaration can ask for wraps it. A
// constant index yields a constant position, which the unit states rather than
// the run computing it.
[[nodiscard]] auto WrapIndexAsPosition(
    mir::CompilationUnit& unit, mir::Block& block, const PositionMap& map,
    mir::ExprId index, std::int64_t shift) -> mir::ExprId;

// The far end of a run of `count` parts that starts at position `start` and
// grows toward higher positions (`up`) or lower ones: `start + count - 1` or
// `start - count + 1`, in the position type. What a zero-based container's
// indexed slice spans (LRM 7.10.1), where the count is a constant the source
// wrote rather than a width a result type states.
[[nodiscard]] auto BuildSpanEnd(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId start,
    mir::ExprId count, bool up) -> mir::ExprId;

// A position the lowering computed itself -- a packed member's place in its
// aggregate, the tag above them -- as a constant of the position type.
[[nodiscard]] auto BuildConstantPosition(
    mir::CompilationUnit& unit, mir::Block& block, std::int64_t position)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
