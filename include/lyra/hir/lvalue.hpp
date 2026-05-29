#pragma once

#include <variant>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/range_bounds.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

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
//
// `LoopVarRef` is reachable only via constructor-side lowering of the
// generate-for header (where the induction variable is the only legal write
// target). Process-side AST->HIR rejects LoopVarRef as an assignment target,
// and HIR->MIR consequently never sees one in a process body.
struct Lvalue {
  std::variant<StructuralVarRef, ProceduralVarRef, LoopVarRef> root;
  std::vector<LvalueSelector> selectors;
};

}  // namespace lyra::hir
