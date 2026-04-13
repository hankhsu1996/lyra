#pragma once

#include "lyra/common/type_arena.hpp"
#include "lyra/common/type_utils.hpp"
#include "lyra/mir/arena.hpp"

namespace lyra {
class SourceManager;
}

namespace lyra::lowering::mir_to_llvm {

struct Layout;

// Read-only fact bundle for one compilation unit.
// Contains only stable, immutable data that does not change during lowering.
// Mutable per-body state (arena_, diag_ctx_) stays on Context.
struct CuFacts {
  const mir::Arena* design_arena = nullptr;
  const TypeArena* types = nullptr;
  const Layout* layout = nullptr;
  bool force_two_state = false;
  const SourceManager* source_manager = nullptr;
};

// Pure fact queries -- free functions, not methods on CuFacts.

[[nodiscard]] inline auto IsFourState(const CuFacts& facts, TypeId type_id)
    -> bool {
  if (facts.force_two_state) return false;
  return IsIntrinsicallyFourState(type_id, *facts.types);
}

[[nodiscard]] inline auto IsPackedFourState(
    const CuFacts& facts, const Type& type) -> bool {
  if (facts.force_two_state) return false;
  return IsIntrinsicallyPackedFourState(type, *facts.types);
}

}  // namespace lyra::lowering::mir_to_llvm
