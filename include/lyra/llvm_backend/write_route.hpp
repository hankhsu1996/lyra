#pragma once

#include <cstdint>

#include "lyra/common/type.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct CuFacts;
class SlotAccessResolver;

// Commit route kind: where a write is routed, independent of lifecycle.
// Computed once by RouteWriteTarget and consumed by execution functions.
enum class CommitRouteKind : uint8_t {
  // Direct write through DispatchWrite (local alloca, temp alloca,
  // canonical design slot, external ref, design-global, object-local).
  kDirect,
  // Shadow alloca protocol via resolver.CommitSlotValue.
  // Only for activation-local managed slots (kModuleSlot with shadow storage).
  kResolverRouted,
  // Packed sub-field read-modify-write via StoreBitRange.
  // Only for PlaceId destinations with bit-range projections.
  kBitRange,
};

// Routed write target: carries routing decision + resolved destination type.
// Does NOT carry OwnershipPolicy -- lifecycle is orthogonal to routing.
struct RoutedWriteTarget {
  CommitRouteKind kind;
  TypeId dest_type;
  mir::WriteTarget dest;
};

// Compute the write route for a destination. Resolves dest_type once.
// Does not inspect OwnershipPolicy or call DispatchWrite.
auto RouteWriteTarget(
    Context& ctx, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest) -> RoutedWriteTarget;

}  // namespace lyra::lowering::mir_to_llvm
