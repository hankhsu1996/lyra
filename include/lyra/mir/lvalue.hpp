#pragma once

#include <variant>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/range_bounds.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::mir {

struct ElementLvalueSelector {
  ExprId index;
};

struct RangeLvalueSelector {
  RangeBounds bounds;
};

using LvalueSelector = std::variant<ElementLvalueSelector, RangeLvalueSelector>;

// Writable place: a root storage (var ref) plus a chain of selectors that
// project into it. Empty chain = whole-var write; one or more selectors =
// partial write at the accumulated bit offset. Chain order is outer-first
// (closest to the root). Render walks the chain alongside the operand's
// PackedArrayType dim stack to translate source-form positions into the
// storage bit address space.
struct Lvalue {
  std::variant<StructuralVarRef, ProceduralVarRef> root;
  std::vector<LvalueSelector> selectors;
};

}  // namespace lyra::mir
