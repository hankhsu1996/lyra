#pragma once

#include <optional>
#include <string>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::test {

struct ExpectedDiag {
  explicit ExpectedDiag(diag::DiagCode c) : code(c) {
  }

  diag::DiagCode code;
  std::optional<diag::DiagKind> kind;
  std::optional<diag::UnsupportedCategory> category;
  std::optional<std::string> file;
  std::optional<int> line;
};

}  // namespace lyra::test
