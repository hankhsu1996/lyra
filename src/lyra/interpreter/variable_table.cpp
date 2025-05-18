#include "lyra/interpreter/variable_table.hpp"

#include <stdexcept>

namespace lyra::interpreter {

void ModuleVariableTable::Write(
    const SymbolRef& symbol, const RuntimeValue& value) {
  variables_[symbol] = value;
}

auto ModuleVariableTable::Read(const SymbolRef& symbol) const -> RuntimeValue {
  auto it = variables_.find(symbol);
  if (it == variables_.end()) {
    throw std::runtime_error(
        "Variable not found: " + std::string(symbol->name));
  }
  return it->second;
}

auto ModuleVariableTable::ReadFromName(const std::string& name) const
    -> RuntimeValue {
  for (const auto& [symbol, value] : variables_) {
    if (std::string(symbol->name) == name) {
      return value;
    }
  }
  throw std::runtime_error("Variable not found: " + name);
}

auto ModuleVariableTable::ReadPrevious(const SymbolRef& symbol) const
    -> RuntimeValue {
  auto it = previous_variables_.find(symbol);
  if (it == previous_variables_.end()) {
    throw std::runtime_error(
        fmt::format("Cannot read from previous variables: {}", symbol->name));
  }
  return it->second;
}

void ModuleVariableTable::UpdatePrevious(
    const SymbolRef& symbol, const RuntimeValue& value) {
  previous_variables_[symbol] = value;
}

auto ModuleVariableTable::Exists(const SymbolRef& symbol) const -> bool {
  return variables_.find(symbol) != variables_.end();
}

void ModuleVariableTable::CreateVariable(
    const SymbolRef& symbol, RuntimeValue initial_value) {
  variables_[symbol] = std::move(initial_value);
}

void ModuleVariableTable::InitializeVariable(const common::Variable& variable) {
  if (!Exists(variable.symbol)) {
    auto initial_value = RuntimeValue::DefaultValueForType(variable.type);
    CreateVariable(variable.symbol, initial_value);
    UpdatePrevious(variable.symbol, initial_value);
  }
}

void ProcessVariableTable::Write(
    const SymbolRef& symbol, const RuntimeValue& value) {
  variables_[symbol] = value;
}

auto ProcessVariableTable::Read(const SymbolRef& symbol) const -> RuntimeValue {
  auto it = variables_.find(symbol);
  if (it == variables_.end()) {
    throw std::runtime_error(
        "Variable not found: " + std::string(symbol->name));
  }
  return it->second;
}

auto ProcessVariableTable::ReadFromName(const std::string& name) const
    -> RuntimeValue {
  for (const auto& [symbol, value] : variables_) {
    if (std::string(symbol->name) == name) {
      return value;
    }
  }
  throw std::runtime_error("Variable not found: " + name);
}

auto ProcessVariableTable::Exists(const SymbolRef& symbol) const -> bool {
  return variables_.find(symbol) != variables_.end();
}

void ProcessVariableTable::CreateVariable(
    const SymbolRef& symbol, RuntimeValue initial_value) {
  variables_[symbol] = std::move(initial_value);
}

void ProcessVariableTable::InitializeVariable(
    const common::Variable& variable) {
  if (!Exists(variable.symbol)) {
    auto initial_value = RuntimeValue::DefaultValueForType(variable.type);
    CreateVariable(variable.symbol, initial_value);
  }
}

}  // namespace lyra::interpreter
