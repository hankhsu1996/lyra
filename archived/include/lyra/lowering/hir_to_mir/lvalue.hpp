#pragma once

#include <memory>
#include <variant>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/statement.hpp"

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
// Writeback token for associative array copy-in/copy-out.
// Produced by lvalue lowering when target is `aa[k].field` (compound access).
// Consumed by assignment lowering to write back the modified element.
struct AssocWriteBack {
  mir::PlaceId aa_place;
  mir::Operand key;
  mir::PlaceId temp_place;
};

struct LvalueResult {
  // Destination for writes: either a local PlaceId or an ExternalRefId
  // (for non-local hierarchical references during body lowering).
  mir::WriteTarget dest;
  mir::Operand validity;  // 1-bit 2-state bool: kConst(1) or kUse(place)
  std::unique_ptr<AssocWriteBack> writeback = nullptr;

  // Check if this lvalue is always valid (no runtime guarding needed).
  [[nodiscard]] auto IsAlwaysValid() const -> bool {
    return hir_to_mir::IsAlwaysValid(validity);
  }

  // Extract PlaceId from dest. Throws InternalError if dest is ExternalRefId.
  // Use for operations that require a local place (reads, projections).
  [[nodiscard]] auto GetLocalPlace() const -> mir::PlaceId;
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

// Emit bounds validity check for unpacked/dynamic/queue array element access.
// Returns 2-state bool: 1 if index is in-bounds (and known for 4-state), 0
// otherwise. Dispatches by array kind:
// - kUnpackedArray: check original index against declared range
// - kDynamicArray/kQueue: runtime size query + signed/unsigned checks
// Takes original index (before normalization).
auto EmitUnpackedIndexValidity(
    mir::Operand index, TypeId index_type, mir::PlaceId base_place,
    TypeId base_type_id, MirBuilder& builder) -> mir::Operand;

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
