#pragma once

#include <cstdint>

// Process function signature for LLVM-generated code.
// - state: pointer to ProcessState (contains SuspendRecord at offset 0, then
// slots)
// - resume_block: which basic block to start execution from
using LyraProcessFunc = void (*)(void* state, uint32_t resume_block);

extern "C" {

// Run simulation with a single process.
// The process function is called repeatedly by the scheduler until completion.
// - process: pointer to the LLVM-generated process function
// - state: pointer to ProcessState (allocated by caller, contains suspend
// record + slots)
void LyraRunSimulation(LyraProcessFunc process, void* state);

// Reset runtime state (call before running processes).
void LyraInitRuntime();

// Print final simulation time as __LYRA_TIME__=<N> for test harness.
void LyraReportTime();
}
