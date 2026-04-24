#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Emit a MIR compilation unit as C++ code. Each class in unit.Classes()
// becomes one C++ class declaration in order: members emit as fields, the
// class constructor body emits as an inline constructor, and each process
// emits as a method.
auto EmitCpp(const mir::CompilationUnit& unit) -> std::string;

}  // namespace lyra::backend::cpp
