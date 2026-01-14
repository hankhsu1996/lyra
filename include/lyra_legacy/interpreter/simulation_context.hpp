#pragma once

#include <cstdint>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <unordered_map>

#include "lyra/common/symbol.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/tracer.hpp"
#include "lyra/interpreter/variable_store.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/sdk/file_io.hpp"
#include "lyra/sdk/plusargs.hpp"

namespace lyra::interpreter {

// Forward declaration
struct HierarchyContext;

using SimulationTime = uint64_t;

/// State for active $monitor.
/// Only one monitor can be active at a time.
/// The synthesized check process is a closure: it has captured variables
/// (previous values) that persist across calls, matching C++ mutable lambda
/// semantics.
struct MonitorState {
  bool enabled = true;  // $monitoron/$monitoroff

  // Instance context for reading variables in the synthesized process
  std::shared_ptr<HierarchyContext> instance;

  // Name of synthesized check process
  std::string check_process_name;

  // Persistent captures map holding previous values of monitored expressions
  // for change detection. This persists across check process calls, matching
  // codegen's mutable lambda capture semantics.
  // Accessed via kLoadCapture/kStoreCapture instructions.
  // For $fmonitor, also contains "__file_descriptor" for output routing.
  std::unordered_map<std::string, RuntimeValue> captures;

  // File descriptor for $fmonitor output (typed field for $fclose
  // cancellation). nullopt = stdout ($monitor), has value = file output
  // ($fmonitor)
  std::optional<uint32_t> file_descriptor;
};

class SimulationContext {
 public:
  SimulationContext() : tracer(current_time) {
  }

  ModuleVariableTable variable_table;
  VariableStore variable_store;
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

  sdk::PlusargsTable plusargs;

  // File I/O state for $fopen/$fclose
  sdk::FileManager file_manager;

  // Symbol table for resolving SymbolId to names (for diagnostics)
  common::SymbolTable symbol_table;
};

}  // namespace lyra::interpreter
