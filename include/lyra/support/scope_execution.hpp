#pragma once

#include <cstdint>

namespace lyra::support {

// How the runtime runs what an instance of a class of the design hierarchy
// starts. Shared vocabulary: the compiler states it of a class, as it states
// the class's timescale, and the runtime gives every execution registered in an
// instance the context it names. The design's code runs in the active region
// set; a program's runs in the reactive one, and its initial procedures are the
// program's own threads, which end together and which the run waits for (LRM
// 24.3, 24.3.1).
enum class ScopeExecution : std::uint8_t {
  kDesign,
  kProgram,
};

}  // namespace lyra::support
