#include "core/signal_table.hpp"

#include <stdexcept>

namespace lyra {

void SignalTable::Write(const std::string& name, const RuntimeValue& value) {
  signals_[name] = value;
}

auto SignalTable::Read(const std::string& name) const -> RuntimeValue {
  auto it = signals_.find(name);
  if (it == signals_.end()) {
    throw std::runtime_error("Signal not found: " + name);
  }
  return it->second;
}

auto SignalTable::ReadPrevious(const std::string& name) const -> RuntimeValue {
  auto it = previous_signals_.find(name);
  if (it != previous_signals_.end()) {
    return it->second;
  }
  return RuntimeValue::FromInt(0);
}

void SignalTable::UpdatePrevious(
    const std::string& name, const RuntimeValue& value) {
  previous_signals_[name] = value;
}

}  // namespace lyra
