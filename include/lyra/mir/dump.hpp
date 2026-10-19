#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

auto DumpMir(const CompilationUnit& unit) -> std::string;

// One type of `unit`, as the dump lists it among the unit's types: its kind and
// what it is made of, each type it names by its id.
auto DumpType(const CompilationUnit& unit, TypeId type) -> std::string;

}  // namespace lyra::mir
