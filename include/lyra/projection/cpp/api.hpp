#pragma once

#include <string>

#include "lyra/mir/module_unit.hpp"

namespace lyra::projection::cpp {

// Render a MIR module unit as a structural C++ projection.
//
// The output is a single C++ class declaration that mirrors the MIR shape:
// members become fields, initial processes become methods, and statements and
// expressions become their direct C++ counterparts. It is a readable view over
// MIR, not an executable artifact -- there is no runtime, no engine wiring,
// and no host entry point.
auto ProjectModuleUnitToCpp(const mir::ModuleUnit& unit) -> std::string;

}  // namespace lyra::projection::cpp
