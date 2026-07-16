#pragma once

#include <string>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"

namespace lyra::hir {

auto DumpHir(const std::vector<CompilationUnit>& units) -> std::string;

}  // namespace lyra::hir
