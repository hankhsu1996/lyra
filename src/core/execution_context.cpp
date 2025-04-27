#include "core/execution_context.hpp"

#include <stdexcept>

namespace lyra {

auto ExecutionContext::ReadSSA(const std::string& name) const -> RuntimeValue {
  auto it = ssa_table.find(name);
  if (it == ssa_table.end()) {
    throw std::runtime_error("Unknown value: " + name);
  }
  return it->second;
}

void ExecutionContext::WriteSSA(
    const std::string& name, const RuntimeValue& value) {
  ssa_table[name] = value;
}

}  // namespace lyra
