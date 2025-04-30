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

auto SignalTable::Exists(const std::string& name) const -> bool {
  return signals_.find(name) != signals_.end();
}

void SignalTable::CreateSignal(
    const std::string& name, RuntimeValue initial_value) {
  signals_[name] = std::move(initial_value);
  declared_signal_names_.push_back(name);
}

auto SignalTable::DeclaredSignalNames() const -> std::vector<std::string> {
  return declared_signal_names_;
}
}  // namespace lyra
