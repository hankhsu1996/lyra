#pragma once

#include <span>

#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::backend::cpp {

// Emit a translation unit per source unit in `units`, plus the design-root unit
// `root` and the host `main` that constructs it. The design has one root, not a
// list of tops, so the root is named on its own; the full `units` list is still
// needed because the host must include the header of any unit contributing a
// DPI-C export wrapper reached only from foreign C (LRM 35.7), which no SV
// referrer would pull in.
// A unit reaching a property or a behavior through a reference with no class
// view is refused whole rather than emitted with a gap. This backend realizes
// an object as a target-language class and reaches a member by writing its
// name, so a position settled while the design elaborates is one it has no
// spelling for; what it cannot realize it declines, and never falls back to
// another form.
auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> diag::Result<CppArtifactSet>;

}  // namespace lyra::backend::cpp
