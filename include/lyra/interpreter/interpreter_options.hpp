#pragma once

namespace lyra::interpreter {

struct InterpreterOptions {
  bool dump_lir = false;
  bool trace_instruction = false;
  bool trace_basic_block = false;
  bool trace_process = false;
};

}  // namespace lyra::interpreter
