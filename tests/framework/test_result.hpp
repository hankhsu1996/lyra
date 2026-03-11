#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <vector>

#include "lyra/common/mutation_event.hpp"
#include "tests/framework/test_value.hpp"

namespace lyra::test {

// Per-test phase timings (seconds). Zero means not measured.
struct TestTimings {
  double parse = 0.0;
  double hir_lower = 0.0;
  double mir_lower = 0.0;
  double llvm_lower = 0.0;
  // JIT compile, AOT emit+link, or MIR setup
  double backend = 0.0;
  double execute = 0.0;
  double total = 0.0;
};

// Result from running a test backend
struct TestResult {
  bool success = false;
  std::string error_message;
  std::string captured_output;
  std::string compiler_output;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
  std::vector<common::MutationEvent> mutation_events;
  TestTimings timings;
};

}  // namespace lyra::test
