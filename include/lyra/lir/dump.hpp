#pragma once

#include <string>

#include "lyra/lir/compilation_unit.hpp"

namespace lyra::lir {

// Serialize a LIR compilation unit to readable text for inspection and golden
// testing. Pure over its input; introduces no meaning not present in the LIR.
auto DumpLir(const CompilationUnit& unit) -> std::string;

}  // namespace lyra::lir
