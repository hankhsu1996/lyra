#include "lyra/interpreter/variable_table.hpp"

#include <utility>

#include <fmt/core.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

void ModuleVariableTable::Write(SymbolId symbol, const RuntimeValue& value) {
  variables_[symbol] = value;
}

auto ModuleVariableTable::Read(SymbolId symbol) const -> RuntimeValue {
  auto it = variables_.find(symbol);
  if (it == variables_.end()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, fmt::format("variable not found: sym#{}", symbol)));
  }
  return it->second;
}

auto ModuleVariableTable::ReadPrevious(SymbolId symbol) const -> RuntimeValue {
  auto it = previous_variables_.find(symbol);
  if (it == previous_variables_.end()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, fmt::format(
                    "cannot read from previous variables: sym#{}", symbol)));
  }
  return it->second;
}

void ModuleVariableTable::UpdatePrevious(
    SymbolId symbol, const RuntimeValue& value) {
  // Deep copy to ensure independent storage for trigger detection.
  // Arrays use shared_ptr internally, so shallow copy would alias.
  previous_variables_[symbol] = value.DeepCopy();
}

auto ModuleVariableTable::Exists(SymbolId symbol) const -> bool {
  return variables_.contains(symbol);
}

void ModuleVariableTable::CreateVariable(
    SymbolId symbol, RuntimeValue initial_value) {
  variables_[symbol] = std::move(initial_value);
}

void ModuleVariableTable::InitializeVariable(const common::Variable& variable) {
  if (!Exists(variable.symbol)) {
    auto initial_value = RuntimeValue::DefaultValueForType(variable.type);
    CreateVariable(variable.symbol, initial_value);
    UpdatePrevious(variable.symbol, initial_value);
  }
}

void ProcessVariableTable::Write(SymbolId symbol, const RuntimeValue& value) {
  variables_[symbol] = value;
}

auto ProcessVariableTable::Read(SymbolId symbol) const -> RuntimeValue {
  auto it = variables_.find(symbol);
  if (it == variables_.end()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, fmt::format("variable not found: sym#{}", symbol)));
  }
  return it->second;
}

auto ProcessVariableTable::Exists(SymbolId symbol) const -> bool {
  return variables_.contains(symbol);
}

void ProcessVariableTable::CreateVariable(
    SymbolId symbol, RuntimeValue initial_value) {
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
