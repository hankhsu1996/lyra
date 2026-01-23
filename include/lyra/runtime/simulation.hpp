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

// Run simulation with multiple processes sharing a single engine.
// All processes are scheduled at time=0, delta=0.
// - processes: array of process function pointers
// - states: array of state pointers (one per process)
// - num_processes: number of processes
void LyraRunSimulationMulti(
    LyraProcessFunc* processes, void** states, uint32_t num_processes);

// Design write helper for integer types.
// Compares old and new values, stores new value, notifies engine if changed.
// - engine_ptr: pointer to Engine (from state header)
// - slot_ptr: pointer to the design slot storage
// - new_value_ptr: pointer to the new value to store
// - byte_size: size of the value in bytes
// - signal_id: slot ID for notification
void LyraDesignStoreAndNotify(
    void* engine_ptr, void* slot_ptr, const void* new_value_ptr,
    uint32_t byte_size, uint32_t signal_id);

// Design write helper for string types.
// - engine_ptr: pointer to Engine
// - slot_ptr: pointer to the design slot (void* pointing to string storage)
// - new_str: pointer to the new string value
// - signal_id: slot ID for notification
void LyraDesignStoreStringAndNotify(
    void* engine_ptr, void* slot_ptr, void* new_str, uint32_t signal_id);

// Reset runtime state (call before running processes).
void LyraInitRuntime();

// Print final simulation time as __LYRA_TIME__=<N> for test harness.
void LyraReportTime();
}
