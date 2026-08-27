#pragma once

#include <optional>
#include <span>
#include <string>

#include "lyra/compiler/compile.hpp"
#include "lyra/compiler/unit_metadata.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/hir/unit_signatures.hpp"
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

// Writes the design-root unit: a compilation unit the compiler synthesizes
// rather than finds in the source, whose constructor elaborates the design (it
// builds the top-level units as its owned children) and whose Initialize phase
// brings up the packages' variables (LRM 26.2 / 10.5). Being a unit, it goes on
// down the same MIR -> LIR -> backend vertical as the rest.
//
// This is the one whole-design step -- it reads across the units to resolve the
// package-initialization plan and to name the tops -- so it is held apart from
// the per-unit lowering, which reads a single unit. What it takes from them is
// what a hand-written top would take from a header: their interfaces (name,
// root presence, exported callables and variables), never their bodies. Symbol
// resolution proper still happens where it does for any program, at link time.
//
// Instantiating the tops makes this a referrer like any other, so `signatures`
// is what it reads about them: each states the class its instances are, which
// is what the root's handle to one is typed by.
auto SynthesizeDesignRoot(
    std::span<const mir::CompilationUnit> units,
    std::span<const std::string> top_names,
    const hir::UnitSignatures& signatures, StopAfter stop_after,
    const diag::SourceManager& source_manager)
    -> diag::Result<DesignRootArtifacts>;

}  // namespace lyra::compiler
