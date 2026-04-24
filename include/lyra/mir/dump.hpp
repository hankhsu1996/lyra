#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

auto DumpMir(const CompilationUnit& unit) -> std::string;

}  // namespace lyra::mir
