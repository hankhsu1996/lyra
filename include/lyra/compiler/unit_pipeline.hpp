#pragma once

#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/compiler/unit_program_record.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// One compilation unit modelled semantically: its MIR, and the program-level
// facts assembling the design reads about it.
struct SemanticUnit {
  mir::CompilationUnit mir;
  UnitProgramRecord program_record;
};

// Models one HIR unit semantically. It reads only this unit and the shared
// frontend, never another unit's lowered artifacts, so units may be lowered in
// any order and in parallel. Whether the unit's instances exist as objects
// selects the lowering: a unit that roots one composes a top class, one that
// names only declarations composes a namespace of callables.
auto LowerUnitToSemantic(
    const hir::CompilationUnit& unit, const diag::SourceManager& source_manager)
    -> diag::Result<SemanticUnit>;

// Takes one unit from its semantic model into the form a session loads or a
// target compiles. Every unit has one, a namespace included: a package's
// variable initializers and its subroutines are code like any other.
auto LowerUnitToExecutable(const mir::CompilationUnit& unit)
    -> diag::Result<ExecutableUnit>;

}  // namespace lyra::compiler
