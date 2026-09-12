#pragma once

#include <optional>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/compiler/unit_program_record.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// The lowered outputs of one compilation unit's vertical: its MIR, the
// program-level facts assembling the design reads about it, and for an
// executable unit its LIR body and definition metadata. The LIR / metadata
// pair is absent for a package (a namespace has no executable body) and for a
// request that stops at MIR.
//
// The program record outlives the MIR beside it, which is what lets the
// whole-design step run on a name and a prototype per unit rather than on the
// units themselves.
struct UnitArtifacts {
  mir::CompilationUnit mir;
  UnitProgramRecord program_record;
  std::optional<lir::CompilationUnit> lir;
  std::optional<ElaboratedUnitMetadata> metadata;
};

// Lowers one HIR unit down its whole vertical (HIR -> MIR -> LIR) as far as
// `stop_after` asks. It reads only this unit and the shared frontend, never
// another unit's lowered artifacts, so units may be lowered in any order and in
// parallel. Whether the unit's instances exist as objects selects the MIR
// lowering: a unit that roots one composes a top class and continues to LIR;
// one that names only declarations lowers to a namespace of callables and stops
// at MIR, having no executable body.
auto LowerUnitPipeline(
    const hir::CompilationUnit& unit, StopAfter stop_after,
    const diag::SourceManager& source_manager) -> diag::Result<UnitArtifacts>;

}  // namespace lyra::compiler
