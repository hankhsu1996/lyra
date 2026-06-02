#pragma once

#include <span>
#include <string>
#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// A top-level block the emitted program constructs under $root: its instance
// name and the compiled unit whose class is instantiated for it.
struct TopInstance {
  std::string name;
  const mir::CompilationUnit* unit;
};

auto EmitCppDeclarations(const mir::CompilationUnit& unit)
    -> diag::Result<std::vector<CppArtifact>>;

auto EmitCppHostMain(std::span<const TopInstance> tops) -> CppArtifact;

auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    std::span<const TopInstance> tops) -> diag::Result<CppArtifactSet>;

}  // namespace lyra::backend::cpp
