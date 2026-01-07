#pragma once

#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

// Forward declaration for PortBinding
struct InstanceContext;

// Binding from output port to parent signal (includes target instance)
struct PortBinding {
  common::SymbolRef target_symbol;
  std::shared_ptr<InstanceContext> target_instance;
};

// Represents a module instance's context (like C++ 'this' pointer).
// Each instance has:
// - Port bindings: maps port symbols to (parent_signal, parent_instance)
// - Variable storage: per-instance storage for module variables
// - Child instances: for hierarchical port access
struct InstanceContext {
  std::string instance_path;  // e.g., "top.counter1"

  // Port bindings: port symbol → (target_symbol, target_instance)
  std::unordered_map<common::SymbolRef, PortBinding> port_bindings;

  // Per-instance variable storage
  std::unordered_map<common::SymbolRef, RuntimeValue> variables;
  std::unordered_map<common::SymbolRef, RuntimeValue> previous_variables;

  // Child instances for hierarchical access (populated during elaboration)
  std::unordered_map<std::string, std::shared_ptr<InstanceContext>> children;

  // Symbol lookup by name (for hierarchical access)
  std::unordered_map<std::string, common::SymbolRef> symbol_by_name;

  InstanceContext(
      std::string path,
      std::unordered_map<common::SymbolRef, PortBinding> bindings)
      : instance_path(std::move(path)), port_bindings(std::move(bindings)) {
  }

  // Resolve symbol through port bindings.
  // Returns (target_symbol, target_instance) if bound, or (symbol, nullptr)
  // if not bound.
  [[nodiscard]] auto ResolveBinding(common::SymbolRef symbol) const
      -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>> {
    auto it = port_bindings.find(symbol);
    if (it != port_bindings.end()) {
      return {it->second.target_symbol, it->second.target_instance};
    }
    return {symbol, nullptr};
  }

  // Check if symbol has a binding
  [[nodiscard]] auto HasBinding(common::SymbolRef symbol) const -> bool {
    return port_bindings.contains(symbol);
  }

  // Variable storage methods
  void Write(common::SymbolRef symbol, const RuntimeValue& value) {
    variables[symbol] = value;
  }

  [[nodiscard]] auto Read(common::SymbolRef symbol) const -> RuntimeValue {
    auto it = variables.find(symbol);
    if (it == variables.end()) {
      throw std::runtime_error(
          "Variable not found in instance: " + instance_path);
    }
    return it->second;
  }

  [[nodiscard]] auto ReadPrevious(common::SymbolRef symbol) const
      -> RuntimeValue {
    auto it = previous_variables.find(symbol);
    if (it == previous_variables.end()) {
      // Fall back to current value if no previous recorded
      return Read(symbol);
    }
    return it->second;
  }

  void UpdatePrevious(common::SymbolRef symbol, const RuntimeValue& value) {
    previous_variables[symbol] = value;
  }

  [[nodiscard]] auto Exists(common::SymbolRef symbol) const -> bool {
    return variables.contains(symbol);
  }

  // Look up child instance by name (for hierarchical access)
  [[nodiscard]] auto LookupChild(const std::string& name) const
      -> std::shared_ptr<InstanceContext> {
    auto it = children.find(name);
    return it != children.end() ? it->second : nullptr;
  }

  // Look up symbol by name (for hierarchical access)
  [[nodiscard]] auto LookupSymbol(const std::string& name) const
      -> common::SymbolRef {
    auto it = symbol_by_name.find(name);
    return it != symbol_by_name.end() ? it->second : nullptr;
  }

  void CreateVariable(common::SymbolRef symbol, RuntimeValue initial_value) {
    variables[symbol] = std::move(initial_value);
  }

  void InitializeVariable(const common::Variable& variable) {
    auto initial_value = RuntimeValue::DefaultValueForType(variable.type);
    // Initialize both current and previous values so triggers work correctly
    // on first change (previous != current when first modified)
    previous_variables[variable.symbol] = initial_value;
    CreateVariable(variable.symbol, std::move(initial_value));
  }
};

}  // namespace lyra::interpreter
