#pragma once

#include <cstdint>

#include "lyra/runtime/observer.hpp"
#include "lyra/runtime/runtime_abi.hpp"
#include "lyra/runtime/string.hpp"

namespace lyra::runtime {

// Explicit process exit status returned by LLVM-generated process functions.
enum class ProcessExitCode : uint32_t {
  kOk = 0,    // Normal completion (suspend, finish, etc.)
  kTrap = 1,  // Terminal trap (loop budget, $fatal, etc.)
};

// Explicit process execution outcome returned directly by LLVM-generated
// process functions. POD struct of four uint32_t fields (16 bytes).
//
// For kOk: { kOk, 0, 0, 0 }
// For kTrap: { kTrap, reason, a, b }
struct ProcessOutcome {
  uint32_t tag;     // ProcessExitCode
  uint32_t reason;  // TrapReason as uint32_t; meaningful only for kTrap
  uint32_t a;       // trap payload field (e.g., back_edge_site_id)
  uint32_t b;       // trap payload field / spare
};

}  // namespace lyra::runtime

// Process function signature for LLVM-generated code.
// - state: pointer to ProcessState (SuspendRecord at offset 0, then frame)
// - resume_block: which basic block to start execution from
// - design_state: design state base pointer (for design-global slot access)
// - out: caller-owned buffer for the process outcome
// Pointer-out ABI: callee writes exactly one valid ProcessOutcome before
// returning. No struct return across JIT boundary, no TLS, no longjmp.
using LyraProcessFunc = void (*)(
    void* state, uint32_t resume_block, void* design_state,
    lyra::runtime::ProcessOutcome* out);

extern "C" {

// Run a process synchronously to completion (for init processes).
// The process must not suspend; if it does, this aborts.
// Encapsulates the entry block number (ABI detail).
void LyraRunProcessSync(
    LyraProcessFunc process, void* state, void* design_state);

// Run simulation with multiple processes sharing a single engine.
// All processes are scheduled at time=0, delta=0.
// - states: array of state pointers (one per process)
// - num_processes: total simulation processes
// - plusargs: optional array of C strings for $plusargs (nullptr if none)
// - num_plusargs: number of plusargs (0 if none)
// - abi: versioned runtime descriptor with metadata tables and feature flags
void LyraRunSimulation(
    void** states, uint32_t num_processes, const char** plusargs,
    uint32_t num_plusargs, const LyraRuntimeAbi* abi, void* run_session_ptr);

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

// $system shell command execution (IEEE 1800-2023 20.18.1).
// cmd_handle: LyraStringHandle for command string; nullptr = no-arg form.
// Returns -1 if system execution is disabled (FeatureFlag::kEnableSystem).
auto LyraSystemCmd(void* engine_ptr, LyraStringHandle cmd_handle) -> int32_t;

// Suspend helpers - own the SuspendRecord layout, called by LLVM-generated
// code. state: pointer to ProcessState (SuspendRecord is at offset 0).
void LyraSuspendDelay(void* state, uint64_t ticks, uint32_t resume_block);
void LyraSuspendWait(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers, uint32_t wait_site_id);
void LyraSuspendWaitWithLateBound(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers, const void* headers, uint32_t num_headers,
    const void* plan_ops, uint32_t num_plan_ops, const void* dep_slots,
    uint32_t num_dep_slots, uint32_t wait_site_id);
void LyraSuspendRepeat(void* state);
void LyraSuspendWaitEvent(
    void* state, uint32_t resume_block, uint32_t event_id);
void LyraTriggerEvent(
    void* engine_ptr, void* instance, uint32_t local_event_id);

// Strobe observer program type: uses canonical StrobeProgramFn from
// observer.hpp.
using LyraStrobeProgramFn = lyra::runtime::StrobeProgramFn;

// Register a strobe observer for the Postponed region.
// ObserverContext fields are passed flat for C ABI, reconstructed on the
// runtime side. instance is RuntimeInstance* (nullptr for design-global).
void LyraRegisterStrobe(
    void* engine_ptr, LyraStrobeProgramFn program, void* design_state,
    void* this_ptr, void* instance);

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
// iteration_limit: per-activation back-edge limit (0 = unlimited).
void LyraInitRuntime(uint32_t iteration_limit);

// Parse the launch-time argv of an emitted `main(argc, argv)` entry point.
//
// `--lyra-fs-root=<absolute path>` is an internal driver-to-child transport
// token, NOT a public CLI flag of the emitted binary. The Lyra driver
// prepends it when spawning AOT/LLI children so the child inherits the
// driver-selected fs_root. It is not documented as a user-facing override
// of the direct-run contract (fs_root = launch-time CWD).
//
// Parsing rules:
// - If the token is present, its value must be non-empty and absolute;
//   otherwise an InternalError is thrown. The token is consumed and never
//   appears in the plusargs array.
// - If the token is absent, fs_root resolves to the launch-time CWD. A
//   failure to read the CWD is fatal (InternalError), never a "." fallback.
//
// Output contract:
// - `*fs_root_out`   : nul-terminated absolute path, valid for the process
//                      lifetime (backed by internal static storage).
// - `plusargs_out`   : caller-provided array, space for at least
//                      max(argc - 1, 0) entries; populated with user plusargs
//                      only.
// - return value     : number of user plusargs written to plusargs_out.
auto LyraLaunchParseArgs(
    int argc, char** argv, const char** fs_root_out, const char** plusargs_out)
    -> uint32_t;

// Print final simulation time as __LYRA_TIME__=<N> for test harness.
// run_session_ptr: opaque pointer to lyra::runtime::RunSession.
void LyraReportTime(void* run_session_ptr);

// Monitor check program type: uses canonical MonitorCheckProgramFn from
// observer.hpp.
using LyraMonitorCheckProgramFn = lyra::runtime::MonitorCheckProgramFn;

// Register a new monitor, atomically replacing any existing one.
// The initial_prev buffer is copied to runtime-owned storage.
// ObserverContext fields are passed flat for C ABI, reconstructed on the
// runtime side. instance is RuntimeInstance* (nullptr for design-global).
void LyraMonitorRegister(
    void* engine_ptr, LyraMonitorCheckProgramFn program, void* design_state,
    void* this_ptr, void* instance, const void* initial_prev, uint32_t size);

// Enable/disable the active monitor. No-op if no active monitor.
// - engine_ptr: pointer to Engine
// - enabled: true to enable, false to disable
void LyraMonitorSetEnabled(void* engine_ptr, bool enabled);

// Apply 4-state X-encoding patches to unknown planes after memset(0).
// Each variant handles a different store width. Uses memcpy for alignment and
// strict-aliasing safety.
// - base: pointer to the base of the struct (DesignState or ProcessFrame)
// - offsets: array of byte offsets from base to the unknown plane
// - masks: array of masks to write at each offset
// - count: number of patches to apply
void LyraApply4StatePatches8(
    void* base, const uint64_t* offsets, const uint8_t* masks, uint64_t count);
void LyraApply4StatePatches16(
    void* base, const uint64_t* offsets, const uint16_t* masks, uint64_t count);
void LyraApply4StatePatches32(
    void* base, const uint64_t* offsets, const uint32_t* masks, uint64_t count);
void LyraApply4StatePatches64(
    void* base, const uint64_t* offsets, const uint64_t* masks, uint64_t count);
}
