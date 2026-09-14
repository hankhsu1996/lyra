#pragma once

#include <span>
#include <string_view>

#include "lyra/compiler/unit_program_record.hpp"
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
// This is the one whole-design step -- it resolves the package-initialization
// plan, defines the program-global foreign symbols, and names the tops -- so it
// is held apart from the per-unit lowering, which reads a single unit. What it
// takes from the units is what each of them published, which is what a
// hand-written top would take from a header. A unit's own lowered form is not
// among these inputs, so a unit is finished with as soon as a backend has
// written it. Symbol resolution proper still happens where it does for any
// program, at link time.
//
// Instantiating the tops makes this a referrer like any other, so `signatures`
// is what it reads about them: each states the class its instances are, which
// is what the root's handle to one is typed by.
auto SynthesizeDesignRoot(
    std::span<const UnitProgramRecord> records,
    std::span<const lowering::ast_to_hir::TopLevelUnit> tops,
    const hir::UnitSignatures& signatures,
    const diag::SourceManager& source_manager)
    -> diag::Result<mir::CompilationUnit>;

}  // namespace lyra::compiler
