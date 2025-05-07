#include "interpreter/temp_table.hpp"

#include <fmt/core.h>

namespace lyra::interpreter {

void TempTable::Write(const std::string& name, const RuntimeValue& value) {
  registers_[name] = value;
}

auto TempTable::Read(const std::string& name) const -> RuntimeValue {
  auto it = registers_.find(name);
  if (it == registers_.end()) {
    throw std::runtime_error(
        fmt::format("Cannot read from temp table: {}", name));
  }
  return it->second;
}

}  // namespace lyra::interpreter
