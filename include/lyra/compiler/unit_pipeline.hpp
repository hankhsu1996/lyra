#pragma once

#include <optional>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// The lowered outputs of one compilation unit's vertical: its MIR, and for an
// executable unit its LIR body and definition metadata. Each optional is
// std::nullopt when the requested `StopAfter` stops before that stage, and the
// LIR / metadata pair is always absent for a package (a namespace has no
// executable body).
struct UnitArtifacts {
  std::optional<mir::CompilationUnit> mir;
  std::optional<lir::CompilationUnit> lir;
  std::optional<ElaboratedUnitMetadata> metadata;
};

// Lowers one HIR unit down its whole vertical (HIR -> MIR -> LIR) as far as
// `stop_after` asks. It reads only this unit and the shared frontend, never
// another unit's lowered artifacts, so units may be lowered in any order and in
// parallel. The unit's `UnitKind` selects the MIR lowering: a module composes a
// top class and continues to LIR; a package lowers to a namespace of callables
// and stops at MIR, having no executable body.
auto LowerUnitPipeline(
    const hir::CompilationUnit& unit, StopAfter stop_after,
    const diag::SourceManager& source_manager) -> diag::Result<UnitArtifacts>;

}  // namespace lyra::compiler
