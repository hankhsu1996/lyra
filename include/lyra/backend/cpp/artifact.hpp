#pragma once

#include <string>
#include <vector>

namespace lyra::backend::cpp {

// One emitted file, its path relative to the project directory, and each file
// of the project it includes, which is what compiling it reads beside the
// runtime's headers. Files are handed over a unit at a time, so memory never
// holds the whole design's text.
struct CppArtifact {
  std::string relpath;
  std::string content;
  std::vector<std::string> includes;
};

}  // namespace lyra::backend::cpp
