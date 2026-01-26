#pragma once

#include <cstdint>
#include <filesystem>

#include "lyra/runtime/string.hpp"

namespace lyra::runtime {

// Get the base directory for relative path resolution in file I/O.
auto GetFsBaseDir() -> const std::filesystem::path&;

}  // namespace lyra::runtime

// Process function signature for LLVM-generated code.
// - state: pointer to ProcessState (contains SuspendRecord at offset 0, then
// slots)
// - resume_block: which basic block to start execution from
using LyraProcessFunc = void (*)(void* state, uint32_t resume_block);

extern "C" {

// Run a process synchronously to completion (for init processes).
// The process must not suspend; if it does, this aborts.
// Encapsulates the entry block number (ABI detail).
void LyraRunProcessSync(LyraProcessFunc process, void* state);

// Run simulation with multiple processes sharing a single engine.
// All processes are scheduled at time=0, delta=0.
// - processes: array of process function pointers
// - states: array of state pointers (one per process)
// - num_processes: number of processes
// - plusargs: optional array of C strings for $plusargs (nullptr if none)
// - num_plusargs: number of plusargs (0 if none)
void LyraRunSimulation(
    LyraProcessFunc* processes, void** states, uint32_t num_processes,
    const char** plusargs, uint32_t num_plusargs);

// $test$plusargs: prefix match against plusargs.
// Query is LyraStringHandle (matches SV string operand lowering).
// Returns 1 if match found, 0 otherwise.
auto LyraPlusargsTest(void* engine_ptr, LyraStringHandle query) -> int32_t;

// $value$plusargs with integer output (%d format).
// Returns 1 if match found, 0 otherwise. Writes parsed value to output.
auto LyraPlusargsValueInt(
    void* engine_ptr, LyraStringHandle format, int32_t* output) -> int32_t;

// $value$plusargs with string output (%s format).
// Returns 1 if match found, 0 otherwise. Creates new string handle in output.
auto LyraPlusargsValueString(
    void* engine_ptr, LyraStringHandle format, LyraStringHandle* output)
    -> int32_t;

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

// Postponed callback type for $strobe and related TFs.
// Thunk matches user function ABI: void (DesignState*, Engine*)
using LyraPostponedCallback = void (*)(void*, void*);

// Schedule a callback to execute in the Postponed region.
// Used by $strobe to defer printing until end of time slot.
void LyraSchedulePostponed(
    void* engine_ptr, LyraPostponedCallback callback, void* design_state);

// Unified termination with kind/level/message support.
// kind: 0=finish, 1=fatal, 2=stop, 3=exit
// level: 0=silent, >=1=print. Negative level treated as 0.
// message: nullable string handle for $fatal (borrowed, not retained)
void LyraTerminate(
    void* engine, uint32_t kind, int32_t level, LyraStringHandle message);

// Get current simulation time ($time semantics).
// Returns raw tick count from the Engine.
auto LyraGetTime(void* engine_ptr) -> uint64_t;

// Set time format state ($timeformat semantics).
// Engine copies the suffix string; caller's pointer only valid for call
// duration.
void LyraSetTimeFormat(
    void* engine_ptr, int8_t units, int32_t precision, const char* suffix,
    int32_t min_width);

// Initialize runtime state (call before running processes).
// fs_base_dir: absolute path for relative file I/O resolution.
void LyraInitRuntime(const char* fs_base_dir);

// Print final simulation time as __LYRA_TIME__=<N> for test harness.
void LyraReportTime();
}
