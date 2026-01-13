#pragma once

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace lyra::sdk {

// Default timescale constants
constexpr int8_t kDefaultUnitPower = -9;        // 1ns
constexpr int8_t kDefaultPrecisionPower = -12;  // 1ps

/// State for $timeformat system task (IEEE 1800-2017 §21.3)
struct TimeFormatState {
  int8_t units = kUnitsUnset;
  int precision = 0;
  std::string suffix;
  int min_width = 20;

  static constexpr int8_t kUnitsUnset = 127;

  [[nodiscard]] auto FormatModuleTime(
      uint64_t time_in_module_unit, int8_t module_unit_power,
      int8_t global_precision) const -> std::string {
    int8_t effective_units = (units == kUnitsUnset) ? global_precision : units;
    int exponent = module_unit_power - effective_units;
    auto scaled = static_cast<double>(time_in_module_unit);

    if (exponent > 0) {
      scaled *= std::pow(10.0, exponent);
    } else if (exponent < 0) {
      scaled /= std::pow(10.0, -exponent);
    }

    auto formatted = std::format("{:.{}f}{}", scaled, precision, suffix);
    if (std::cmp_less(formatted.size(), min_width)) {
      formatted =
          std::string(static_cast<size_t>(min_width) - formatted.size(), ' ') +
          formatted;
    }
    return formatted;
  }
};

/// Input to a test run - the clean interface between runner and generated code.
/// The runner constructs this from command-line arguments, and generated code
/// uses it to configure the simulation.
struct TestInvocation {
  std::string_view work_dir;
  std::span<const std::string_view> plusargs;
  std::string_view exe;  // For diagnostics
};

/// All mutable simulation state that needs to be scoped per-test.
/// This replaces the scattered global variables in the SDK.
struct RuntimeConfig {
  // Time precision for the simulation (default: 1ps = -12)
  int8_t global_precision_power = kDefaultPrecisionPower;

  // Simulation termination flags
  bool simulation_finished = false;
  bool simulation_stopped = false;

  // Plusargs from command line
  std::vector<std::string> plusargs;

  // $timeformat state (units, precision, suffix, min_width)
  TimeFormatState time_format_state;
};

/// RAII guard that installs a RuntimeConfig via thread-local storage.
/// Ensures proper cleanup and allows nested scopes (though typically
/// only one scope is active per simulation).
///
/// Usage:
///   RuntimeConfig config;
///   config.global_precision_power = -9;  // 1ns
///   RuntimeScope scope(config);
///   // ... run simulation - all SDK functions see this config ...
///   // destructor restores previous config
class RuntimeScope {
 public:
  explicit RuntimeScope(RuntimeConfig& config) : previous_(CurrentPtr()) {
    CurrentPtr() = &config;
  }

  ~RuntimeScope() {
    CurrentPtr() = previous_;
  }

  // Non-copyable, non-movable
  RuntimeScope(const RuntimeScope&) = delete;
  auto operator=(const RuntimeScope&) -> RuntimeScope& = delete;
  RuntimeScope(RuntimeScope&&) = delete;
  auto operator=(RuntimeScope&&) -> RuntimeScope& = delete;

  /// Get the current RuntimeConfig.
  /// Returns the active scope's config, or a thread-local default if no scope.
  static auto Current() -> RuntimeConfig& {
    if (CurrentPtr() != nullptr) {
      return *CurrentPtr();
    }
    return DefaultConfig();
  }

  /// Check if a RuntimeScope is currently active.
  static auto IsActive() -> bool {
    return CurrentPtr() != nullptr;
  }

 private:
  RuntimeConfig* previous_;

  // Thread-local pointer to current RuntimeConfig.
  // Using function-local static avoids global variable warnings.
  static auto CurrentPtr() -> RuntimeConfig*& {
    thread_local RuntimeConfig* ptr = nullptr;
    return ptr;
  }

  // Thread-local default config used when no RuntimeScope is active.
  static auto DefaultConfig() -> RuntimeConfig& {
    thread_local RuntimeConfig config;
    return config;
  }
};

/// Get the global precision power for time formatting.
/// Used by $printtimescale and %t format specifier.
inline auto GlobalPrecisionPower() -> int8_t {
  return RuntimeScope::Current().global_precision_power;
}

/// Set the global precision power.
inline void SetGlobalPrecisionPower(int8_t power) {
  RuntimeScope::Current().global_precision_power = power;
}

/// Check if simulation has been terminated ($finish, $stop, $exit, $fatal).
inline auto IsSimulationFinished() -> bool {
  return RuntimeScope::Current().simulation_finished;
}

/// Set simulation finished flag.
inline void SetSimulationFinished(bool finished) {
  RuntimeScope::Current().simulation_finished = finished;
}

/// Check if simulation was stopped ($stop, $fatal - non-zero exit).
inline auto IsSimulationStopped() -> bool {
  return RuntimeScope::Current().simulation_stopped;
}

/// Set simulation stopped flag.
inline void SetSimulationStopped(bool stopped) {
  RuntimeScope::Current().simulation_stopped = stopped;
}

/// Get the current time format state.
inline auto GetTimeFormatState() -> TimeFormatState& {
  return RuntimeScope::Current().time_format_state;
}

/// $timeformat - set format for %t display.
/// All arguments are optional (uses current values if not provided).
inline void TimeFormat(
    int8_t units = TimeFormatState::kUnitsUnset, int precision = 0,
    const std::string& suffix = "", int min_width = 20) {
  auto& state = RuntimeScope::Current().time_format_state;
  state.units = units;
  state.precision = precision;
  state.suffix = suffix;
  state.min_width = min_width;
}

/// FormatTimeValue - format a time value according to $timeformat settings.
/// time_value: time value in module's timeunit (e.g., from $time)
/// module_unit_power: the module's timeunit power (e.g., -9 for ns)
template <typename T>
inline auto FormatTimeValue(T time_value, int8_t module_unit_power)
    -> std::string {
  return RuntimeScope::Current().time_format_state.FormatModuleTime(
      static_cast<uint64_t>(time_value), module_unit_power,
      RuntimeScope::Current().global_precision_power);
}

}  // namespace lyra::sdk
