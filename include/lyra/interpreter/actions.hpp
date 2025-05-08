#pragma once

#include <functional>
#include <string>

#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

struct NbaAction {
  std::string variable;
  RuntimeValue value;

  [[nodiscard]] auto ToString() const -> std::string {
    return fmt::format("{} <= {}", variable, value.ToString());
  }
};

struct PostponedAction {
  std::function<void()> action;
};

}  // namespace lyra::interpreter
