#pragma once

#include <string>
#include <unordered_map>

#include "value.hpp"

namespace lyra {

class SignalTable {
 public:
  void Write(const std::string &name, const RuntimeValue &value);
  [[nodiscard]] auto Read(const std::string &name) const -> RuntimeValue;

 private:
  std::unordered_map<std::string, RuntimeValue> signals_;
};

}  // namespace lyra
