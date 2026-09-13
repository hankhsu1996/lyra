#pragma once

#include <string>

#include "lyra/hir/compilation_unit.hpp"

namespace lyra::hir {

auto DumpHir(const CompilationUnit& unit) -> std::string;

}  // namespace lyra::hir
