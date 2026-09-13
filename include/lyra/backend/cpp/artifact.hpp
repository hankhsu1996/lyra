#pragma once

#include <string>

namespace lyra::backend::cpp {

// One emitted file, named relative to the project directory it belongs in. It
// is handed over as it is rendered rather than collected, so what an emit
// holds is one file's text and never the design's.
struct CppArtifact {
  std::string relpath;
  std::string content;
};

}  // namespace lyra::backend::cpp
