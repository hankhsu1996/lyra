#pragma once

#include <cstdint>
#include <string>

namespace lyra::compiler {

// The immutable, source-level metadata of one compiled design unit, held apart
// from its executable body. A compiled unit is two things: an executable body
// (its LIR / lowered code) and this definition metadata -- the facts a unit has
// before it runs, independent of any execution. LIR is the executable body
// only; it carries no source-language concept, so a unit's time precision (LRM
// Table 20-2) and its def name (LRM 23.8) live here, beside the code, never
// inside it. A host builds the runtime's per-unit definition from the
// executable body plus this metadata; it never reaches back into the earlier IR
// to recover these facts.
struct ElaboratedUnitMetadata {
  std::string def_name;
  std::int8_t time_precision_power = 0;
};

}  // namespace lyra::compiler
