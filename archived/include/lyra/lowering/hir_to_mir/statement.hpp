#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

auto LowerStatement(hir::StatementId stmt_id, MirBuilder& builder)
    -> Result<void>;

}  // namespace lyra::lowering::hir_to_mir
