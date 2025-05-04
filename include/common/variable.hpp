#pragma once

#include <string>

#include "common/type.hpp"

namespace lyra::common {

struct Variable {
  std::string name;
  Type type;
};

}  // namespace lyra::common
