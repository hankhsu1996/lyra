#pragma once

#include <span>
#include <string_view>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/lowering/ast_to_hir/lower.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::compiler {

// The name of the synthesized design-root unit. Its constructor elaborates the
// design -- it builds the top-level units as its owned children -- so the host
// constructs this one unit and the runtime walks the tree it builds. A leading
// `$` keeps it distinct from every source unit (an SV identifier cannot begin
// with `$`); a backend maps it to a target-language identifier when emitting.
inline constexpr std::string_view kDesignRootUnitName = "$root";

// Writes the design-root unit: a compilation unit the compiler synthesizes
// rather than finds in the source, whose constructor elaborates the design (it
// builds the top-level units as its owned children) and whose Initialize phase
// brings up the packages' variables (LRM 26.2 / 10.5). Being a unit, it goes on
// down the same vertical as the rest, so what it answers with is its semantic
// model and a caller that wants it executable takes it the rest of the way.
//
// It is a referrer like any other, and `signatures` is the whole of what it
// reads: each top states the class its instances are, which types the root's
// handle to one, and a unit that roots no object is a namespace to bring up.
// No unit's lowered form is among its inputs, so a unit is finished with as
// soon as a backend has written it, and symbol resolution proper happens where
// it does for any program -- at link time.
auto SynthesizeDesignRoot(
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures,
    const diag::SourceManager& source_manager)
    -> diag::Result<mir::CompilationUnit>;

}  // namespace lyra::compiler
