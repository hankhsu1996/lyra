#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/tracer.hpp"
#include "lyra/interpreter/variable_table.hpp"

namespace lyra::interpreter {

// Forward declaration
struct InstanceContext;

using SimulationTime = uint64_t;
using SymbolRef = common::SymbolRef;

/// Tracks a monitored expression for $monitor.
/// Index into Module::monitor_expression_blocks for re-evaluation at each time
/// slot.
struct MonitoredVariable {
  size_t expression_block_index;
};

/// State for active $monitor.
/// Only one monitor can be active at a time.
struct MonitorState {
  bool enabled = true;  // $monitoron/$monitoroff

  // Format string for output
  std::string format_string;

  // Display properties
  char default_format = 'd';  // 'd', 'b', 'o', or 'h'
  bool append_newline = true;

  // Variables to monitor for value changes
  std::vector<MonitoredVariable> variables;

  // Previous values for change detection (parallel to variables)
  std::vector<RuntimeValue> previous_values;

  // Instance context for reading variables
  std::shared_ptr<InstanceContext> instance;

  // Time format context for %t specifier
  common::TimeFormatState time_format;
  int8_t module_unit_power = 0;
  int8_t global_precision_power = 0;
};

class SimulationContext {
 public:
  SimulationContext() : tracer(current_time) {
  }

  ModuleVariableTable variable_table;
  SimulationTime current_time = 0;
  Tracer tracer;
  std::ostringstream display_output;

  // True if simulation terminated via $stop (non-zero exit code)
  bool stopped = false;

  // Timescale info for $time/$stime/$realtime scaling
  std::optional<common::TimeScale> timescale;
  int8_t global_precision_power = common::TimeScale::kDefaultPrecisionPower;

  // Module name for $printtimescale output
  std::string module_name;

  // $timeformat state for %t format specifier
  common::TimeFormatState time_format;

  // Active $monitor state (only one at a time per IEEE 1800)
  std::optional<MonitorState> active_monitor;
};

}  // namespace lyra::interpreter
