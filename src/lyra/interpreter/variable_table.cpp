#include "lyra/interpreter/variable_table.hpp"

#include <stdexcept>

namespace lyra::interpreter {

void VariableTable::Write(const SymbolRef& symbol, const RuntimeValue& value) {
  variables_[symbol] = value;
}

auto VariableTable::Read(const SymbolRef& symbol) const -> RuntimeValue {
  auto it = variables_.find(symbol);
  if (it == variables_.end()) {
    throw std::runtime_error(
        "Variable not found: " + std::string(symbol->name));
  }
  return it->second;
}

auto VariableTable::ReadFromName(const std::string& name) const
    -> RuntimeValue {
  for (const auto& [symbol, value] : variables_) {
    if (std::string(symbol->name) == name) {
      return value;
    }
  }
  throw std::runtime_error("Variable not found: " + name);
}

auto VariableTable::ReadPrevious(const SymbolRef& symbol) const
    -> RuntimeValue {
  auto it = previous_variables_.find(symbol);
  if (it == previous_variables_.end()) {
    throw std::runtime_error(
        fmt::format("Cannot read from previous variables: {}", symbol->name));
  }
  return it->second;
}

void VariableTable::UpdatePrevious(
    const SymbolRef& symbol, const RuntimeValue& value) {
  previous_variables_[symbol] = value;
}

auto VariableTable::Exists(const SymbolRef& symbol) const -> bool {
  return variables_.find(symbol) != variables_.end();
}

void VariableTable::CreateVariable(
    const SymbolRef& symbol, RuntimeValue initial_value) {
  variables_[symbol] = std::move(initial_value);
}

void VariableTable::InitializeVariable(
    const SymbolRef& symbol, const common::Type& type) {
  if (!Exists(symbol)) {
    auto initial_value = RuntimeValue::DefaultValueForType(type);
    CreateVariable(symbol, initial_value);
    UpdatePrevious(symbol, initial_value);
  }
}

}  // namespace lyra::interpreter
