#pragma once

#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTypeData(const hir::TypeData& data, const UnitLoweringState& state)
    -> mir::TypeData;

}  // namespace lyra::lowering::hir_to_mir
