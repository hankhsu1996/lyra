#pragma once

#include <string>
#include <unordered_map>

#include "value.hpp"

namespace lyra {

class SignalTable {
 public:
  void Write(const std::string &name, const RuntimeValue &value);
  auto Read(const std::string &name) const -> RuntimeValue;
  auto ReadPrevious(const std::string &name) const -> RuntimeValue;
  void UpdatePrevious(const std::string &name, const RuntimeValue &value);
  auto Exists(const std::string &name) const -> bool;
  void CreateSignal(const std::string &name, RuntimeValue initial_value);

 private:
  std::unordered_map<std::string, RuntimeValue> signals_;
  std::unordered_map<std::string, RuntimeValue> previous_signals_;
};

}  // namespace lyra
