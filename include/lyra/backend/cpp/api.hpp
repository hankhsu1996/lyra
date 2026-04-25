#pragma once

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Emit a MIR compilation unit as a set of C++ artifact files. The unit must
// contain exactly one class for this cut; the result contains:
//   <ClassName>.hpp -- the class declaration inheriting lyra::runtime::Module
//   main.cpp        -- a host driver that constructs the class and runs it
auto EmitCpp(const mir::CompilationUnit& unit) -> CppArtifactSet;

}  // namespace lyra::backend::cpp
