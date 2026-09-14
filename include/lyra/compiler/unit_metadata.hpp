#pragma once

#include <cstdint>

#include "lyra/lir/compilation_unit.hpp"

namespace lyra::mir {
struct CompilationUnit;
}  // namespace lyra::mir

namespace lyra::compiler {

// The immutable, source-level metadata of one compiled design unit, held apart
// from its executable body. A compiled unit is two things: an executable body
// (its LIR / lowered code) and this definition metadata -- the facts a unit has
// before it runs, independent of any execution. LIR is the executable body
// only; it carries no source-language concept, so a unit's time precision (LRM
// Table 20-2) lives here, beside the code, never inside it. A host builds the
// runtime's definition from the executable body plus this metadata; it never
// reaches back into the earlier IR to recover these facts.
struct ElaboratedUnitMetadata {
  std::int8_t time_precision_power = 0;
};

// One compiled unit, which is those two things. Neither half means anything
// without the other, so they are produced and carried as one.
struct ExecutableUnit {
  lir::CompilationUnit body;
  ElaboratedUnitMetadata definition;
};

// A unit's definition metadata is a source-level fact known once elaboration
// fixes the unit's root scope: its precision is the root's declared resolution.
// Derived from MIR so the executable body downstream never carries these
// source-language concepts. A unit whose object tree has no root (LRM 26) gives
// its scopes no precision to state, and answers with the default.
auto BuildUnitMetadata(const mir::CompilationUnit& unit)
    -> ElaboratedUnitMetadata;

}  // namespace lyra::compiler
