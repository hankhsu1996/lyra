#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::hir_to_mir {

class MirBuilder;

auto LowerExpression(hir::ExpressionId expr_id, MirBuilder& builder)
    -> Result<mir::Operand>;

}  // namespace lyra::lowering::hir_to_mir
