#pragma once

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

void LowerStatement(hir::StatementId stmt_id, MirBuilder& builder);

}  // namespace lyra::lowering::hir_to_mir
