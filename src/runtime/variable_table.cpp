#include "runtime/variable_table.hpp"

#include <stdexcept>

namespace lyra {

void VariableTable::Write(const std::string& name, const RuntimeValue& value) {
  variables_[name] = value;
}

auto VariableTable::Read(const std::string& name) const -> RuntimeValue {
  auto it = variables_.find(name);
  if (it == variables_.end()) {
    throw std::runtime_error("Variable not found: " + name);
  }
  return it->second;
}

auto VariableTable::ReadPrevious(const std::string& name) const
    -> RuntimeValue {
  auto it = previous_variables_.find(name);
  if (it == previous_variables_.end()) {
    throw std::runtime_error(
        fmt::format("Cannot read from previous variables: {}", name));
  }
  return it->second;
}

void VariableTable::UpdatePrevious(
    const std::string& name, const RuntimeValue& value) {
  previous_variables_[name] = value;
}

auto VariableTable::Exists(const std::string& name) const -> bool {
  return variables_.find(name) != variables_.end();
}

void VariableTable::CreateVariable(
    const std::string& name, RuntimeValue initial_value) {
  variables_[name] = std::move(initial_value);
}

void VariableTable::InitializeVariable(
    const std::string& name, const common::Type& type) {
  if (!Exists(name)) {
    auto initial_value = RuntimeValue::DefaultValueForType(type);
    CreateVariable(name, initial_value);
    UpdatePrevious(name, initial_value);
  }
}

}  // namespace lyra
