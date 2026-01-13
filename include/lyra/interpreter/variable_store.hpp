#pragma once

#include <stdexcept>
#include <string>
#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

/// Flat variable storage for simulation runtime.
/// All module, package, and port variables are stored here with O(1) lookup.
/// Keyed by SymbolId (globally unique within a compilation).
/// Replaces hierarchical instance-based storage from pre-refactor design.
/// Supports previous-value tracking for edge-triggered sensitivity analysis.
class VariableStore {
 public:
  void Initialize(common::SymbolId symbol, RuntimeValue value) {
    previous_variables_[symbol] = value.DeepCopy();
    variables_[symbol] = std::move(value);
  }

  void Write(common::SymbolId symbol, const RuntimeValue& value) {
    variables_[symbol] = value;
  }

  [[nodiscard]] auto Read(common::SymbolId symbol) const
      -> const RuntimeValue& {
    return variables_.at(symbol);
  }

  [[nodiscard]] auto ReadPrevious(common::SymbolId symbol) const
      -> const RuntimeValue& {
    auto it = previous_variables_.find(symbol);
    if (it != previous_variables_.end()) {
      return it->second;
    }
    return Read(symbol);
  }

  void UpdatePrevious(common::SymbolId symbol) {
    previous_variables_[symbol] = variables_.at(symbol).DeepCopy();
  }

  [[nodiscard]] auto Exists(common::SymbolId symbol) const -> bool {
    return variables_.contains(symbol);
  }

  // Look up variable by name using symbol table for name resolution
  [[nodiscard]] auto ReadFromName(
      const std::string& name, const common::SymbolTable& symbol_table) const
      -> RuntimeValue {
    for (const auto& [symbol_id, value] : variables_) {
      if (symbol_table.GetInfo(symbol_id).name == name) {
        return value;
      }
    }
    throw std::runtime_error("Variable not found: " + name);
  }

 private:
  std::unordered_map<common::SymbolId, RuntimeValue> variables_;
  std::unordered_map<common::SymbolId, RuntimeValue> previous_variables_;
};

}  // namespace lyra::interpreter
