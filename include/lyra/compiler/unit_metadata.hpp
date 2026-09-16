#pragma once

#include "lyra/base/time.hpp"
#include "lyra/lir/compilation_unit.hpp"

namespace lyra::mir {
struct CompilationUnit;
}  // namespace lyra::mir

namespace lyra::compiler {

// The immutable, source-level metadata of one compiled design unit, held apart
// from its executable body. A compiled unit is two things: an executable body
// (its LIR / lowered code) and this definition metadata -- the facts a unit has
// before it runs, independent of any execution. LIR is the executable body
// only; it carries no source-language concept, so a unit's timescale (LRM
// Table 20-2) lives here, beside the code, never inside it. A host builds the
// runtime's definition from the executable body plus this metadata; it never
// reaches back into the earlier IR to recover these facts.
struct ElaboratedUnitMetadata {
  TimeResolution time_resolution;
};

// One compiled unit, which is those two things. Neither half means anything
// without the other, so they are produced and carried as one.
struct ExecutableUnit {
  lir::CompilationUnit body;
  ElaboratedUnitMetadata definition;
};

// A unit's definition metadata is a source-level fact known once elaboration
// fixes the unit's root scope: its timescale is the root's declared resolution,
// which every scope inside the unit runs at, a time scope being a design
// element or the compilation unit and nothing smaller (LRM 3.14.2.3). Derived
// from MIR so the executable body downstream never carries these
// source-language concepts. A unit whose object tree has no root (LRM 26) gives
// its scopes no timescale to state, and answers with the default.
auto BuildUnitMetadata(const mir::CompilationUnit& unit)
    -> ElaboratedUnitMetadata;

}  // namespace lyra::compiler
