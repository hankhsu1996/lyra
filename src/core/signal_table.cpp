#include "core/signal_table.hpp"

#include <stdexcept>

namespace volans {

void SignalTable::Write(const std::string &name, const RuntimeValue &value) {
  signals_[name] = value;
}

auto SignalTable::Read(const std::string &name) const -> RuntimeValue {
  auto it = signals_.find(name);
  if (it == signals_.end()) {
    throw std::runtime_error("Signal not found: " + name);
  }
  return it->second;
}

}  // namespace volans
