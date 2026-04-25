#pragma once

#include <string>
#include <vector>

namespace lyra::backend::cpp {

struct CppArtifact {
  std::string relpath;
  std::string content;
};

struct CppArtifactSet {
  std::vector<CppArtifact> files;
};

}  // namespace lyra::backend::cpp
