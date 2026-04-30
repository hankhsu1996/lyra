#pragma once

#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/structural_scope.hpp"

namespace lyra::backend::cpp {

auto EmitCppDeclarations(const mir::CompilationUnit& unit)
    -> diag::Result<std::vector<CppArtifact>>;

auto EmitCppHostMain(const mir::StructuralScope& entry_scope) -> CppArtifact;

auto EmitCpp(const mir::CompilationUnit& unit) -> diag::Result<CppArtifactSet>;

}  // namespace lyra::backend::cpp
