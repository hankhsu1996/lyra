#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

// Lower a HIR pattern to MIR effects.
// Pattern lowering emits effect operations (e.g., FillPacked) that store
// to the target place based on the pattern's fill/override rules.
auto LowerPattern(
    hir::PatternId pattern_id, mir::PlaceId target, MirBuilder& builder)
    -> Result<void>;

}  // namespace lyra::lowering::hir_to_mir
