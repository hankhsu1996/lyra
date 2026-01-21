#pragma once

#include <memory>

#include "lyra/common/type_arena.hpp"
#include "lyra/hir/arena.hpp"
#include "lyra/lowering/origin_map.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering {

// Bundled output from HIR→MIR lowering.
// Owns the MIR artifacts and origin map; holds non-owning references
// to HIR/type arena (caller must guarantee lifetime).
struct LoweringArtifacts {
  // Owned: MIR output
  mir::Design design;
  std::unique_ptr<mir::Arena> mir_arena;
  OriginMap origin_map;

  // Non-owning references (caller guarantees lifetime)
  const hir::Arena* hir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
};

}  // namespace lyra::lowering
