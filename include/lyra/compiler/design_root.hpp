#pragma once

#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/declared_design.hpp"
#include "lyra/lowering/hir_to_mir/design_instantiations.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir_transform/pass.hpp"

namespace lyra::compiler {

// Writes the design-root unit: a compilation unit the compiler synthesizes
// rather than finds in the source, whose constructor elaborates the design (it
// builds the top-level units as its owned children) and installs the
// packages' storage, and which runs their initializers in its Initialize phase
// (LRM 26.2, 10.5). Being a unit, it goes on down the same vertical as the
// rest, so what it answers with is its semantic model and a caller that wants
// it executable takes it the rest of the way.
//
// It is a referrer like any other, and what the units published is the whole
// of what it reads of another unit's declarations: each top states the class
// its instances are, which types the root's handle to one, and a unit that
// roots no object is a namespace to bring up. No unit's lowered form is among
// its inputs, so a unit is finished with as soon as a backend has written it,
// and symbol resolution proper happens where it does for any program -- at link
// time.
//
// Being the unit the whole design links through, it also states every unit's
// construction table, from what `instantiations` says each unit elaborated at
// its instantiations, as a link states where each symbol resolves.
auto SynthesizeDesignRoot(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& published,
    lowering::hir_to_mir::DesignInstantiations instantiations,
    const mir_transform::PassOptions& passes,
    const diag::SourceManager& sources) -> diag::Result<mir::CompilationUnit>;

}  // namespace lyra::compiler
