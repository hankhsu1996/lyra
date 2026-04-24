#pragma once

#include <string>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::projection::cpp {

// Render a MIR compilation unit as a structural C++ projection. Each class in
// unit.Classes() becomes one C++ class declaration in order: members emit as
// fields, the class constructor body emits as an inline constructor, and each
// process emits as a method. It is a readable view over MIR, not an
// executable artifact.
auto ProjectCompilationUnitToCpp(const mir::CompilationUnit& unit)
    -> std::string;

}  // namespace lyra::projection::cpp
