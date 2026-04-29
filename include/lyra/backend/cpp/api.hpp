#pragma once

#include <vector>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

auto EmitCppDeclarations(const mir::CompilationUnit& unit)
    -> diag::Result<std::vector<CppArtifact>>;

auto EmitCppHostMain(const mir::ClassDecl& entry_class) -> CppArtifact;

auto EmitCpp(
    const mir::CompilationUnit& unit, const mir::ClassDecl& entry_class)
    -> diag::Result<CppArtifactSet>;

}  // namespace lyra::backend::cpp
