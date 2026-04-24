#pragma once

#include "lyra/hir/type.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerTypeData(const hir::TypeData& data) -> mir::TypeData;

}  // namespace lyra::lowering::hir_to_mir
