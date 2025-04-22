#pragma once

#include "value.hpp"
#include <string>
#include <unordered_map>

namespace volans {

class SignalTable {
public:
  void Write(const std::string &name, const RuntimeValue &value);
  [[nodiscard]] auto Read(const std::string &name) const -> RuntimeValue;

private:
  std::unordered_map<std::string, RuntimeValue> signals_;
};

} // namespace volans
