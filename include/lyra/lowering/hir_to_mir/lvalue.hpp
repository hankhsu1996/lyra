#pragma once

#include "lyra/hir/fwd.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

// Lower an HIR expression as an lvalue, returning the writable PlaceId.
// This is the single source of truth for lvalue lowering - both assignment
// targets and increment/decrement operands use this path.
//
// Current support: NameRef only (simple variable references).
// Future: Array indexing and struct fields will require lowering index
// expressions to temps first, then building projected Places.
auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> mir::PlaceId;

}  // namespace lyra::lowering::hir_to_mir
