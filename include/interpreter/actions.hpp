#pragma once

#include <functional>
#include <string>

#include "runtime/runtime_value.hpp"

namespace lyra::interpreter {

struct NbaAction {
  std::string variable;
  RuntimeValue value;
};

struct PostponedAction {
  std::function<void()> action;
};

}  // namespace lyra::interpreter
