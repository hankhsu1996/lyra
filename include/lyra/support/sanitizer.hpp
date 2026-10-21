#pragma once

#include <cstdint>

namespace lyra::support {

// What a program is built to check of itself while it runs, as clang names its
// sanitizers. Shared vocabulary: the command line selects it, the execution
// backend instruments a design's code for it, and a build links what it needs.
// It changes what a build compiles and links and nothing the program computes.
enum class Sanitizer : std::uint8_t {
  kNone,
  // Every read and write the design's code and the runtime make is checked,
  // and the first of storage that has ended, or was never the program's, ends
  // the run with a report naming where it was made and where the storage was
  // freed.
  kAddress,
};

}  // namespace lyra::support
