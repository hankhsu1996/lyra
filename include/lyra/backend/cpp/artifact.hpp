#pragma once

#include <string>

namespace lyra::backend::cpp {

// One emitted file, its path relative to the project directory. Files are
// handed over a unit at a time, so memory never holds the whole design's text.
struct CppArtifact {
  std::string relpath;
  std::string content;
};

}  // namespace lyra::backend::cpp
