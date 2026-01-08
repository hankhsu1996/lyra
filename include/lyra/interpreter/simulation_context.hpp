#pragma once

#include <filesystem>
#include <optional>
#include <sstream>

#include "lyra/common/time_format.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/tracer.hpp"
#include "lyra/interpreter/variable_table.hpp"

namespace lyra::interpreter {

using SimulationTime = uint64_t;

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

  // Base directory for resolving relative file paths (e.g., $readmemh)
  std::optional<std::filesystem::path> base_dir;
};

}  // namespace lyra::interpreter
