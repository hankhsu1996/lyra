#include "core/temp_table.hpp"

namespace lyra {

void TempTable::Write(const std::string& name, const RuntimeValue& value) {
  registers_[name] = value;
}

auto TempTable::Read(const std::string& name) const -> RuntimeValue {
  auto it = registers_.find(name);
  if (it != registers_.end()) {
    return it->second;
  }
  return RuntimeValue::FromInt(0);
}

}  // namespace lyra
