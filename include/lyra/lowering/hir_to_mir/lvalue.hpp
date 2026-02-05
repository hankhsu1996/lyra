#pragma once

#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

// Check if validity operand is constant 1 (always valid).
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

// Result of lvalue lowering: place to write and validity predicate.
//
// Per IEEE 1800-2023, OOB array access semantics:
// - OOB read -> returns unknown value (X for 4-state, 0 for 2-state)
// - OOB write -> no-op
//
// For packed element select with dynamic index, validity may be false if:
// - Index is out of bounds
// - Index contains X/Z bits
// Callers use GuardedUse/GuardedAssign to implement OOB-safe operations.
//
// INVARIANT: validity is always block-stable (kConst or kUse, never kUseTemp).
// UseTemps are block-local SSA; callers may lower expressions that create
// blocks (e.g., ternary), which would invalidate UseTemp operands. Lvalue
// lowering materializes non-constant validity to a place to prevent this.
//
// TODO(hankhsu): Extend validity tracking to unpacked/dynamic arrays and
// queues. Currently only packed arrays track validity; other array types need
// OOB handling added in lowering (emit GuardedUse/GuardedAssign).
struct LvalueResult {
  mir::PlaceId place;
  mir::Operand validity;  // 1-bit 2-state bool: kConst(1) or kUse(place)

  // Check if this lvalue is always valid (no runtime guarding needed).
  [[nodiscard]] auto IsAlwaysValid() const -> bool {
    return hir_to_mir::IsAlwaysValid(validity);
  }
};

// Lower an HIR expression as an lvalue, returning the writable place and
// validity predicate. This is the single source of truth for lvalue lowering -
// both assignment targets and increment/decrement operands use this path.
//
// For writes with potentially invalid indices, callers should check validity
// and emit guarded stores (no-op if invalid).
//
// Returns Diagnostic::Unsupported on unsupported expression types.
auto LowerLvalue(hir::ExpressionId expr_id, MirBuilder& builder)
    -> Result<LvalueResult>;

// Normalize a declaration-space index to a 0-based storage offset.
// Ascending [L:U]: offset = index - L
// Descending [U:L]: offset = U - index
// Dynamic arrays and queues are always 0-based (returned unchanged).
auto NormalizeUnpackedIndex(
    mir::Operand index_operand, TypeId index_type, const Type& base_type,
    MirBuilder& builder) -> mir::Operand;

struct Context;

// Lower an HIR lvalue expression as a pure place (no instruction emission).
// This function has no ability to emit instructions - purity is by
// construction.
//
// Supports:
// - Name references and hierarchical references
// - Static member access (unpacked struct fields)
// - Union member access
// - Packed field access with constant offsets
// - Element access with CONSTANT indices only
//
// Rejects (returns diagnostic at point of detection):
// - Dynamic array indices (non-constant)
// - Packed element select with dynamic index
// - Indexed part-select with dynamic index
// - Any expression requiring runtime computation
//
// Used for port connection targets where only design-level storage is valid.
auto LowerPureLvaluePlace(hir::ExpressionId expr_id, const Context& ctx)
    -> Result<mir::PlaceId>;

}  // namespace lyra::lowering::hir_to_mir
