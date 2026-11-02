#pragma once

#include <functional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::compiler {

// Models one body semantically. It reads only this unit and the
// shared frontend, never another unit's lowered artifacts, so units may be
// lowered in any order and in parallel. Whether the unit's instances exist as
// objects selects the lowering: a unit that roots one composes a top class,
// one that names only declarations composes a namespace of callables. The
// model is rewritten as `passes` asks before anything reads it.
auto LowerUnitToSemantic(
    const hir::CompilationUnit& unit, const mir_transform::PassOptions& passes,
    const diag::SourceManager& sources) -> diag::Result<mir::CompilationUnit>;

// Tells a run that records itself what `unit` holds as a semantic model. A
// unit modelled from its HIR is told of where it is modelled; the one composed
// for the whole design is modelled from no unit of the design, so whoever
// composes it tells of it.
void RecordSemanticForm(const mir::CompilationUnit& unit);

// What a target finds a unit uses of the operations of types no declaration
// states them for, read off LIR as the unit grows: each call answers what the
// unit gained since the last. Which of them a realization needs is the target's
// to say, so the pipeline is handed this, as rustc's driver is handed its
// `CodegenBackend`, and names no target.
using TypeOperationCollection =
    std::function<lir::UsedTypeOperations(const lir::CompilationUnit&)>;

// Takes one unit from its semantic model into the form a session loads or a
// target compiles. Every unit has one, a namespace included: a package's
// variable initializers and its subroutines are code like any other. What the
// unit's bodies come to need of types no declaration states operations for is
// built here, rewritten as `passes` asks of a body and lowered with them, so
// the form that leaves is complete. It joins the lowering above to what
// `collection` finds, which is why it stands above the lowerings.
auto LowerUnitToExecutable(
    const mir::CompilationUnit& unit, const mir_transform::PassOptions& passes,
    const TypeOperationCollection& collection,
    const diag::SourceManager& sources) -> diag::Result<lir::CompilationUnit>;

}  // namespace lyra::compiler
