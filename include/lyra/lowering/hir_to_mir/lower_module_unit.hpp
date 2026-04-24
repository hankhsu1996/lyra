#pragma once

#include "lyra/hir/module_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::lowering::hir_to_mir {

auto LowerModuleUnit(const hir::ModuleUnit& unit) -> mir::CompilationUnit;

}  // namespace lyra::lowering::hir_to_mir
