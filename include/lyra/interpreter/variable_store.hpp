#pragma once

#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

/// Flat variable storage for simulation runtime.
/// All module, package, and port variables are stored here with O(1) lookup.
/// Keyed by AST symbol pointers (globally unique via slang).
/// Replaces hierarchical instance-based storage from pre-refactor design.
/// Supports previous-value tracking for edge-triggered sensitivity analysis.
class VariableStore {
 public:
  void Initialize(common::SymbolRef symbol, RuntimeValue value) {
    previous_variables_[symbol] = value.DeepCopy();
    variables_[symbol] = std::move(value);
  }

  void Write(common::SymbolRef symbol, const RuntimeValue& value) {
    variables_[symbol] = value;
  }

  [[nodiscard]] auto Read(common::SymbolRef symbol) const
      -> const RuntimeValue& {
    return variables_.at(symbol);
  }

  [[nodiscard]] auto ReadPrevious(common::SymbolRef symbol) const
      -> const RuntimeValue& {
    auto it = previous_variables_.find(symbol);
    if (it != previous_variables_.end()) {
      return it->second;
    }
    return Read(symbol);
  }

  void UpdatePrevious(common::SymbolRef symbol) {
    previous_variables_[symbol] = variables_.at(symbol).DeepCopy();
  }

  [[nodiscard]] auto Exists(common::SymbolRef symbol) const -> bool {
    return variables_.contains(symbol);
  }

  [[nodiscard]] auto ReadFromName(const std::string& name) const
      -> RuntimeValue {
    for (const auto& [symbol, value] : variables_) {
      if (symbol->name == name) {
        return value;
      }
    }
    throw std::runtime_error("variable not found: " + name);
  }

 private:
  std::unordered_map<common::SymbolRef, RuntimeValue> variables_;
  std::unordered_map<common::SymbolRef, RuntimeValue> previous_variables_;
};

}  // namespace lyra::interpreter
