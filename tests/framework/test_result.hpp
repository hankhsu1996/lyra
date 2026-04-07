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

// High-level classification of test execution outcome.
enum class ExecutionOutcome {
  kSuccess,            // Simulation ran and exited normally
  kFrontendError,      // AST/HIR/MIR/LLVM lowering failed
  kBackendSetupError,  // DPI compile, object emit, link, JIT compile
  kExecutionFailed,    // Clean nonzero exit from simulation
  kCrashed,            // Signal-terminated (e.g., SIGSEGV, SIGABRT)
  kTimedOut,           // Deadline exceeded
  kInfraError,         // Pipe/spawn/wait/filesystem failure
};

// Execution classification and error context.
struct ExecutionResult {
  ExecutionOutcome outcome = ExecutionOutcome::kInfraError;
  std::string error_message;
  std::string stderr_text;
  int exit_code = 0;
  int signal_number = 0;
};

// Simulation payload (only meaningful when outcome == kSuccess).
struct SimulationArtifacts {
  std::string captured_output;
  std::string compiler_output;
  std::map<std::string, TestValue> variables;
  uint64_t final_time = 0;
  std::vector<common::MutationEvent> mutation_events;
  std::vector<uint64_t> cover_hits;
  TestTimings timings;
};

// Combined result from executing one test case.
struct CaseExecutionResult {
  ExecutionResult execution;
  SimulationArtifacts artifacts;
};

}  // namespace lyra::test
