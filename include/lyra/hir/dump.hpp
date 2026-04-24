#pragma once

#include <string>
#include <vector>

#include "lyra/hir/module_unit.hpp"

namespace lyra::hir {

auto DumpHir(const std::vector<ModuleUnit>& units) -> std::string;

}  // namespace lyra::hir
