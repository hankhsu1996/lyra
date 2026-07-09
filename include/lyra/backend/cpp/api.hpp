#pragma once

#include <span>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

auto EmitCppDeclarations(const mir::CompilationUnit& unit) -> CppArtifact;

// The host `main` constructs the design-root unit, whose constructor builds the
// top-level units as its owned children; the engine then walks that tree. The
// design has one root, not a list of tops, so the host takes the single root
// unit.
auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact;

// Emit a translation unit per source unit in `units`, plus the design-root
// unit `root` and the host `main` that constructs it.
auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> CppArtifactSet;

}  // namespace lyra::backend::cpp
