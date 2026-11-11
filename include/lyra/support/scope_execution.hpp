#pragma once

#include <cstdint>

namespace lyra::support {

// How the runtime runs what an instance of a class of the design hierarchy
// starts. Shared vocabulary: the runtime reads it off the kind of definition an
// instance comes from, and gives every execution registered in the instance the
// context it names. The design's code runs in the active region set; a
// program's runs in the reactive one, and its initial procedures are the
// program's own threads, which end together and which the run waits for (LRM
// 24.3, 24.3.1).
enum class ScopeExecution : std::uint8_t {
  kDesign,
  kProgram,
};

}  // namespace lyra::support
