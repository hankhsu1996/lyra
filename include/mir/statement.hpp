#pragma once

#include <memory>
#include <string>

#include "mir/expression.hpp"

namespace volans::mir {

class Statement {
 public:
  enum class Kind { kAssign };

  Kind kind;
  std::string target;
  std::shared_ptr<Expression> value;
};

}  // namespace volans::mir
