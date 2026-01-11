#pragma once

#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <utility>

#include "lyra/common/symbol.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::lir {
class Module;
}

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
// - Module: the module this instance was created from (for function lookup)
// - Port bindings: maps port symbols to (parent_signal, parent_instance)
// - Variable storage: per-instance storage for module variables
// - Child instances: for hierarchical port access
struct InstanceContext {
  std::string instance_path;  // e.g., "top.counter1"

  // Module this instance was created from (non-owning; modules outlive
  // instances)
  const lir::Module* module = nullptr;

  // Port bindings: port symbol → (target_symbol, target_instance)
  std::unordered_map<common::SymbolRef, PortBinding> port_bindings;

  // Per-instance variable storage
  std::unordered_map<common::SymbolRef, RuntimeValue> variables;
  std::unordered_map<common::SymbolRef, RuntimeValue> previous_variables;

  // Child instances for hierarchical access (populated during elaboration)
  // Keyed by instance symbol for O(1) lookup without string comparison
  std::unordered_map<common::SymbolRef, std::shared_ptr<InstanceContext>>
      children;

  InstanceContext(
      std::string path,
      std::unordered_map<common::SymbolRef, PortBinding> bindings,
      const lir::Module* mod = nullptr)
      : instance_path(std::move(path)),
        module(mod),
        port_bindings(std::move(bindings)) {
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
    // Deep copy to ensure independent storage for trigger detection.
    // Arrays use shared_ptr internally, so shallow copy would alias.
    previous_variables[symbol] = value.DeepCopy();
  }

  [[nodiscard]] auto Exists(common::SymbolRef symbol) const -> bool {
    return variables.contains(symbol);
  }

  // Look up child instance by symbol (for interpreter trigger resolution)
  [[nodiscard]] auto LookupChild(common::SymbolRef instance_symbol) const
      -> std::shared_ptr<InstanceContext> {
    auto it = children.find(instance_symbol);
    return it != children.end() ? it->second : nullptr;
  }

  void CreateVariable(common::SymbolRef symbol, RuntimeValue initial_value) {
    variables[symbol] = std::move(initial_value);
  }

  void InitializeVariable(const common::Variable& variable) {
    auto initial_value = RuntimeValue::DefaultValueForType(variable.type);
    // Initialize both current and previous values so triggers work correctly
    // on first change (previous != current when first modified).
    // Use DeepCopy() to ensure independent storage for trigger detection.
    previous_variables[variable.symbol] = initial_value.DeepCopy();
    CreateVariable(variable.symbol, std::move(initial_value));
  }
};

}  // namespace lyra::interpreter
