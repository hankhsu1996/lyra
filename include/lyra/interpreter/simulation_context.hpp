#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <sstream>
#include <string>

#include "lyra/common/symbol.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/tracer.hpp"
#include "lyra/interpreter/variable_table.hpp"

namespace lyra::interpreter {

// Forward declaration
struct InstanceContext;

using SimulationTime = uint64_t;
using SymbolRef = common::SymbolRef;

/// State for active $monitor.
/// Only one monitor can be active at a time.
/// The synthesized check process is a closure: it has captured variables
/// (previous values) that persist across calls, matching C++ mutable lambda
/// semantics.
struct MonitorState {
  bool enabled = true;  // $monitoron/$monitoroff

  // Instance context for reading variables in the synthesized process
  std::shared_ptr<InstanceContext> instance;

  // Name of synthesized check process
  std::string check_process_name;

  // Persistent closure frame. The frame's `captures` map holds previous values
  // of monitored expressions for change detection. This persists across check
  // process calls, matching codegen's mutable lambda capture semantics.
  // Accessed via kLoadCapture/kStoreCapture instructions.
  CallFrame closure;
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
