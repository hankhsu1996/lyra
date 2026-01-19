#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

// Result of lvalue lowering: place to write and validity predicate.
// For packed element select with dynamic index, validity may be false if:
// - Index is out of bounds
// - Index contains X/Z bits
// For simple lvalues (name refs, constant indices), validity is always true.
struct LvalueResult {
  mir::PlaceId place;
  mir::Operand validity;  // 1-bit 2-state bool, or constant 1 if always valid
};

// Check if validity is constant 1 (always valid).
// Used by assignment lowering to skip guarded store when unnecessary.
inline auto IsAlwaysValid(const mir::Operand& validity) -> bool {
  if (validity.kind != mir::Operand::Kind::kConst) {
    return false;
  }
  const auto& constant = std::get<Constant>(validity.payload);
  const auto* ic = std::get_if<IntegralConstant>(&constant.value);
  if (ic == nullptr || ic->value.empty()) {
    return false;
  }
  return ic->value[0] == 1;
}

// Lower an HIR expression as an lvalue, returning the writable place and
// validity predicate. This is the single source of truth for lvalue lowering -
// both assignment targets and increment/decrement operands use this path.
//
// For writes with potentially invalid indices, callers should check validity
// and emit guarded stores (no-op if invalid).
auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> LvalueResult;

}  // namespace lyra::lowering::hir_to_mir
