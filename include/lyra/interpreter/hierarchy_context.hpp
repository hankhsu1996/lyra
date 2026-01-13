#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <utility>

#include "lyra/common/symbol.hpp"

namespace lyra::interpreter {

// Forward declaration for PortBinding
struct HierarchyContext;

// Binding from output port to parent signal (includes target instance)
struct PortBinding {
  common::SymbolId target_symbol;
  std::shared_ptr<HierarchyContext> target_instance;
};

// Represents a module instance's context for port binding resolution.
// After the flat VariableStore refactor, HierarchyContext no longer stores
// variables - it only handles port bindings and hierarchical structure.
struct HierarchyContext {
  std::string hierarchy_path;  // e.g., "top.counter1" (for debugging)

  // Port bindings: port symbol -> (target_symbol, target_instance)
  std::unordered_map<common::SymbolId, PortBinding> port_bindings;

  // Child instances for hierarchical access (populated during elaboration)
  // Keyed by instance symbol for O(1) lookup without string comparison
  std::unordered_map<common::SymbolId, std::shared_ptr<HierarchyContext>>
      children;

  HierarchyContext(
      std::string path,
      std::unordered_map<common::SymbolId, PortBinding> bindings)
      : hierarchy_path(std::move(path)), port_bindings(std::move(bindings)) {
  }

  // Resolve symbol through port bindings.
  // Returns (target_symbol, target_instance) if bound, or (symbol, nullptr)
  // if not bound.
  [[nodiscard]] auto ResolveBinding(common::SymbolId symbol) const
      -> std::pair<common::SymbolId, std::shared_ptr<HierarchyContext>> {
    auto it = port_bindings.find(symbol);
    if (it != port_bindings.end()) {
      return {it->second.target_symbol, it->second.target_instance};
    }
    return {symbol, nullptr};
  }

  // Check if symbol has a binding
  [[nodiscard]] auto HasBinding(common::SymbolId symbol) const -> bool {
    return port_bindings.contains(symbol);
  }

  // Look up child instance by symbol (for interpreter trigger resolution)
  [[nodiscard]] auto LookupChild(common::SymbolId instance_symbol) const
      -> std::shared_ptr<HierarchyContext> {
    auto it = children.find(instance_symbol);
    return it != children.end() ? it->second : nullptr;
  }
};

}  // namespace lyra::interpreter
