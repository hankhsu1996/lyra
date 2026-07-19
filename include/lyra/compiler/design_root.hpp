#pragma once

#include <optional>
#include <span>
#include <string>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// The synthesized design-root unit, lowered as far as `StopAfter` asks: its
// MIR, and when LIR is requested its executable body and definition metadata.
// It is a distinct compiler output, not one of the source units.
struct DesignRootArtifacts {
  mir::CompilationUnit mir;
  std::optional<lir::CompilationUnit> lir;
  std::optional<ElaboratedUnitMetadata> metadata;
};

// Links the compiled units into the design: synthesizes the design-root unit
// whose constructor elaborates the design (it builds the top-level units as its
// owned children) and whose Initialize phase brings up the packages' variables
// (LRM 26.2 / 10.5). This is the one whole-design step -- it reads across the
// units to resolve the package-initialization plan and to name the tops -- so
// it is held apart from the per-unit lowering, which reads a single unit. The
// units are consumed only through their interfaces (name, root presence,
// exported callables and variables), never their bodies.
auto LinkDesign(
    std::span<const mir::CompilationUnit> units,
    std::span<const std::string> top_names, StopAfter stop_after,
    const diag::SourceManager& source_manager)
    -> diag::Result<DesignRootArtifacts>;

}  // namespace lyra::compiler
