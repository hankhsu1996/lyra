#pragma once

#include <string>
#include <vector>

namespace lyra::interpreter {

struct InterpreterOptions {
  bool dump_lir = false;
  bool trace_instruction = false;
  bool trace_basic_block = false;
  bool trace_process = false;
  std::vector<std::string> plusargs;
};

}  // namespace lyra::interpreter
