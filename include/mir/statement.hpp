#pragma once

#include <memory>
#include <string>

#include "mir/expression.hpp"

namespace lyra::mir {

class Statement {
 public:
  enum class Kind { kAssign };

  Kind kind;
  std::string target;
  std::shared_ptr<Expression> value;
};

}  // namespace lyra::mir
