#pragma once

namespace lyra::driver {

struct DriverOptions {
  bool dump_lir = false;
  bool trace_instruction = false;
  bool trace_basic_block = false;
  bool trace_process = false;
};

}  // namespace lyra::driver
