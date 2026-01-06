#pragma once

#include <memory>
#include <string>
#include <unordered_map>

#include "lyra/common/symbol.hpp"

namespace lyra::interpreter {

// Represents a module instance's symbol bindings (like C++ 'this' pointer).
// Each instance has a mapping from port symbols to parent signal symbols.
struct InstanceContext {
  std::string instance_path;  // e.g., "top.counter1"
  std::unordered_map<common::SymbolRef, common::SymbolRef>
      port_bindings;  // port → parent signal

  InstanceContext(
      std::string path,
      std::unordered_map<common::SymbolRef, common::SymbolRef> bindings)
      : instance_path(std::move(path)), port_bindings(std::move(bindings)) {
  }

  // Resolve symbol through port bindings (like this->member).
  // Returns the original symbol if no binding exists.
  [[nodiscard]] auto Resolve(common::SymbolRef symbol) const
      -> common::SymbolRef {
    auto it = port_bindings.find(symbol);
    return it != port_bindings.end() ? it->second : symbol;
  }
};

}  // namespace lyra::interpreter
