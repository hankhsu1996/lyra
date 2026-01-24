#pragma once

#include <cstdint>

// Process function signature for LLVM-generated code.
// - state: pointer to ProcessState (contains SuspendRecord at offset 0, then
// slots)
// - resume_block: which basic block to start execution from
using LyraProcessFunc = void (*)(void* state, uint32_t resume_block);

extern "C" {

// Run simulation with multiple processes sharing a single engine.
// All processes are scheduled at time=0, delta=0.
// - processes: array of process function pointers
// - states: array of state pointers (one per process)
// - num_processes: number of processes
void LyraRunSimulation(
    LyraProcessFunc* processes, void** states, uint32_t num_processes);

// Suspend helpers — own the SuspendRecord layout, called by LLVM-generated
// code. state: pointer to ProcessState (SuspendRecord is at offset 0).
void LyraSuspendDelay(void* state, uint64_t ticks, uint32_t resume_block);
void LyraSuspendWait(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers);
void LyraSuspendRepeat(void* state);

// Store a packed (integer/packed-array) value to a design slot with change
// notification.
// - engine_ptr: pointer to Engine (from state header)
// - slot_ptr: pointer to the design slot storage
// - new_value_ptr: pointer to the new value to store
// - byte_size: size of the value in bytes
// - signal_id: slot ID for notification
void LyraStorePacked(
    void* engine_ptr, void* slot_ptr, const void* new_value_ptr,
    uint32_t byte_size, uint32_t signal_id);

// Store a string value to a design slot with change notification.
// - engine_ptr: pointer to Engine
// - slot_ptr: pointer to the design slot (void* pointing to string storage)
// - new_str: pointer to the new string value
// - signal_id: slot ID for notification
void LyraStoreString(
    void* engine_ptr, void* slot_ptr, void* new_str, uint32_t signal_id);

// Schedule a non-blocking assignment for later commit in the NBA region.
// - engine_ptr: pointer to Engine (from state header)
// - write_ptr: exact write address for the target
// - notify_base_ptr: base design slot pointer (for registry)
// - value_ptr: pointer to value bytes to write
// - mask_ptr: pointer to mask bytes (per-byte mask)
// - byte_size: size of the write region in bytes
// - notify_slot_id: slot ID for trigger lookup
void LyraScheduleNba(
    void* engine_ptr, void* write_ptr, const void* notify_base_ptr,
    const void* value_ptr, const void* mask_ptr, uint32_t byte_size,
    uint32_t notify_slot_id);

// Stop the simulation ($finish semantics).
// Sets Engine::finished_ flag so the event loop terminates.
void LyraFinishSimulation(void* engine_ptr);

// Get current simulation time ($time semantics).
// Returns raw tick count from the Engine.
auto LyraGetTime(void* engine_ptr) -> uint64_t;

// Reset runtime state (call before running processes).
void LyraInitRuntime();

// Print final simulation time as __LYRA_TIME__=<N> for test harness.
void LyraReportTime();
}
