#pragma once

#include <string>

#include "mir/type.hpp"

namespace volans::mir {

class Variable {
 public:
  std::string name;
  Type type;
};

}  // namespace volans::mir
