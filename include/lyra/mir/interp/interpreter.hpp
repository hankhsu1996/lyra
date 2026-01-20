#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <ostream>
#include <variant>
#include <vector>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/interp/frame.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::mir::interp {

// Process execution status
enum class ProcessStatus {
  kRunning,
  kFinished,
};

// Suspension reasons returned by interpreter when process cannot continue.
// These are used by the simulation engine to reschedule the process.

// Process completed normally (reached kFinish or kReturn terminator).
struct SuspendFinished {};

// Process suspended on delay (#N or @(posedge clk) with implicit delay).
struct SuspendDelay {
  uint64_t ticks = 0;         // Number of simulation ticks to wait
  BasicBlockId resume_block;  // Block to resume at after delay
};

// Process suspended waiting for signal edge (@posedge, @negedge, @*).
struct SuspendWait {
  // TODO(hankhsu): Add signal ID and edge type when Wait terminator is fleshed
  // out
  BasicBlockId resume_block;
};

// Process completed one iteration and should repeat (always blocks).
struct SuspendRepeat {
  BasicBlockId resume_block;  // Usually entry block
};

// Result of running a process: either continues running or suspends.
using SuspendReason =
    std::variant<SuspendFinished, SuspendDelay, SuspendWait, SuspendRepeat>;

// Shared storage for module-level variables (simulation lifetime).
// One DesignState per module instance; all processes in that instance share it.
struct DesignState {
  std::vector<RuntimeValue> storage;

  explicit DesignState(size_t num_slots) : storage(num_slots) {
  }

  [[nodiscard]] auto Get(int id) -> RuntimeValue& {
    return storage[static_cast<size_t>(id)];
  }
  [[nodiscard]] auto Get(int id) const -> const RuntimeValue& {
    return storage[static_cast<size_t>(id)];
  }
};

// State of a single process execution
struct ProcessState {
  ProcessId process;
  BasicBlockId current_block;
  size_t instruction_index = 0;
  Frame frame;
  DesignState* design_state = nullptr;
  ProcessStatus status = ProcessStatus::kRunning;
};

// MIR Interpreter: executes a single process to completion.
//
// Supports single initial process, basic control flow, expressions, and
// local/temp storage. Hard-fails on suspension terminators (Delay, Wait,
// Repeat) as these require a scheduler/runtime.
class Interpreter {
 public:
  Interpreter(const Arena* arena, const TypeArena* types)
      : arena_(arena), types_(types) {
  }

  // Set output stream for $display (defaults to stdout if not set)
  void SetOutput(std::ostream* out) {
    output_ = out;
  }

  // Execute process to completion. Returns final status.
  // Throws on suspension terminators (Delay, Wait, Repeat).
  auto Run(ProcessState& state) -> ProcessStatus;

  // Execute process until it suspends or finishes.
  // Returns the suspension reason for the engine to handle scheduling.
  auto RunUntilSuspend(ProcessState& state) -> SuspendReason;

  // Execute a function call. Returns the return value (void for void
  // functions).
  auto RunFunction(
      FunctionId func_id, const std::vector<RuntimeValue>& args,
      DesignState* design_state) -> RuntimeValue;

 private:
  // Evaluate Operand to RuntimeValue (always deep copy)
  auto EvalOperand(const ProcessState& state, const Operand& op)
      -> RuntimeValue;

  // Evaluate Rvalue to RuntimeValue (may mutate for pop methods)
  auto EvalRvalue(ProcessState& state, const Rvalue& rv) -> RuntimeValue;

  // Resolve PlaceRoot to storage (handles Local/Temp/Design)
  static auto ResolveRoot(const ProcessState& state, const PlaceRoot& root)
      -> const RuntimeValue&;
  static auto ResolveRootMut(ProcessState& state, const PlaceRoot& root)
      -> RuntimeValue&;

  // Resolve Place for reading (returns copy)
  auto ReadPlace(const ProcessState& state, PlaceId place_id) -> RuntimeValue;

  // Resolve Place for writing (fails if place ends with BitRange)
  auto WritePlace(ProcessState& state, PlaceId place_id) -> RuntimeValue&;

  // Write to a place that ends with BitRange projection (read-modify-write)
  void WriteBitRange(ProcessState& state, PlaceId place_id, RuntimeValue value);

  // Store a value to a place (dispatches to WritePlace or WriteBitRange)
  void StoreToPlace(ProcessState& state, PlaceId place_id, RuntimeValue value);

  // Execute Assign instruction
  void ExecAssign(ProcessState& state, const Assign& assign);

  // Execute Compute instruction
  void ExecCompute(ProcessState& state, const Compute& compute);

  // Execute GuardedAssign instruction
  void ExecGuardedAssign(ProcessState& state, const GuardedAssign& guarded);

  // Execute Effect instruction
  void ExecEffect(ProcessState& state, const Effect& effect);

  // Execute DisplayEffect
  void ExecDisplayEffect(const ProcessState& state, const DisplayEffect& disp);

  // Execute SeverityEffect
  void ExecSeverityEffect(
      const ProcessState& state, const SeverityEffect& severity);

  // Execute instruction
  void ExecInstruction(ProcessState& state, const Instruction& inst);

  // Execute terminator, return next block or nullopt for completion.
  // Throws on suspension terminators (Delay, Wait, Repeat).
  auto ExecTerminator(ProcessState& state, const Terminator& term)
      -> std::optional<BasicBlockId>;

  const Arena* arena_;
  const TypeArena* types_;
  std::ostream* output_ = nullptr;
};

// Helper: Create ProcessState for a given process.
// Initializes locals and temps with default values based on their types.
auto CreateProcessState(
    const Arena& arena, const TypeArena& types, ProcessId process_id,
    DesignState* design_state) -> ProcessState;

// Info about the initial module to run
struct InitialModuleInfo {
  const Module* module = nullptr;
  std::vector<ProcessId> initial_processes;  // All kOnce processes in order
};

// Helper: Create DesignState with properly initialized storage.
// Scans all processes in the module to collect type information for slots.
auto CreateDesignState(
    const Arena& arena, const TypeArena& types, const Module& module)
    -> DesignState;

// Find the first module with a kOnce process (initial block).
// Returns nullopt if no initial process found.
auto FindInitialModule(const Design& design, const Arena& arena)
    -> std::optional<InitialModuleInfo>;

}  // namespace lyra::mir::interp
