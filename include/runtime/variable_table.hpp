#pragma once

#include <string>
#include <unordered_map>

#include "runtime/runtime_value.hpp"

namespace lyra {

class VariableTable {
 public:
  void Write(const std::string &name, const RuntimeValue &value);
  auto Read(const std::string &name) const -> RuntimeValue;
  auto ReadPrevious(const std::string &name) const -> RuntimeValue;
  void UpdatePrevious(const std::string &name, const RuntimeValue &value);
  auto Exists(const std::string &name) const -> bool;
  void CreateVariable(const std::string &name, RuntimeValue initial_value);
  void InitializeVariable(const std::string &name, const common::Type &type);

 private:
  std::unordered_map<std::string, RuntimeValue> variables_;
  std::unordered_map<std::string, RuntimeValue> previous_variables_;
};

}  // namespace lyra
