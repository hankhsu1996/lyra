#pragma once

#include <string>

#include "mir/type.hpp"

namespace lyra::mir {

class Variable {
 public:
  std::string name;
  Type type;
};

}  // namespace lyra::mir
