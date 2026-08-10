#pragma once

#include <span>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Emit a translation unit per source unit in `units`, plus the design-root unit
// `root` and the host `main` that constructs it. The design has one root, not a
// list of tops, so the root is named on its own; the full `units` list is still
// needed because the host must include the header of any unit contributing a
// DPI-C export wrapper reached only from foreign C (LRM 35.7), which no SV
// referrer would pull in.
auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> CppArtifactSet;

}  // namespace lyra::backend::cpp
