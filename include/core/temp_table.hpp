#pragma once

#include <string>
#include <unordered_map>

#include "core/value.hpp"

namespace lyra {

class TempTable {
 public:
  void Write(const std::string& name, const RuntimeValue& value);
  auto Read(const std::string& name) const -> RuntimeValue;

 private:
  std::unordered_map<std::string, RuntimeValue> registers_;
};

}  // namespace lyra
