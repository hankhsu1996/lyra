#pragma once

#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <optional>
#include <ostream>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/common/system_tf.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/interp/frame.hpp"
#include "lyra/mir/interp/location.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/runtime/file_manager.hpp"

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
  uint32_t instance_id = UINT32_MAX;  // For %m: index into instance paths
  // Set by ExecTerminator when process suspends (Delay, Wait, Repeat).
  // RunUntilSuspend checks this before returning.
  std::optional<SuspendReason> pending_suspend;
  // Set by ExecTerminator when handling Return with a value (function calls).
  // RunFunction reads this after execution completes.
  std::optional<RuntimeValue> function_return_value;
};

// MIR Interpreter: executes processes to completion.
//
// Supports multiple initial processes across modules, basic control flow,
// expressions, and local/temp storage. Suspension terminators (Delay, Wait,
// Repeat) are handled via the runtime Engine for scheduling.
class Interpreter {
 public:
  Interpreter(
      const Arena* arena, const TypeArena* types,
      const lowering::DiagnosticContext* diag_ctx = nullptr)
      : arena_(arena), types_(types), diag_ctx_(diag_ctx) {
  }

  // Set output stream for $display (defaults to stdout if not set)
  void SetOutput(std::ostream* out) {
    output_ = out;
  }

  // Set plusargs for $test$plusargs/$value$plusargs queries
  void SetPlusargs(std::vector<std::string> plusargs) {
    plusargs_ = std::move(plusargs);
  }

  // Set current simulation time (from Engine) for $finish/$time output.
  // Must be called before RunUntilSuspend to provide accurate time.
  void SetSimulationTime(uint64_t time) {
    simulation_time_ = time;
  }
  [[nodiscard]] auto GetSimulationTime() const -> uint64_t {
    return simulation_time_;
  }

  // Set base directory for relative file I/O resolution ($readmem, etc.)
  void SetFsBaseDir(std::filesystem::path path) {
    fs_base_dir_ = std::move(path);
  }

  // Set instance paths for %m formatting
  void SetInstancePaths(std::vector<std::string> paths) {
    instance_paths_ = std::move(paths);
  }

  // Security: Enable $system shell command execution (disabled by default)
  void SetEnableSystem(bool enable) {
    enable_system_ = enable;
  }

  // Execute process to completion. Returns final status.
  // Returns error on suspension terminators (Delay, Wait, Repeat).
  auto Run(ProcessState& state) -> Result<ProcessStatus>;

  // Execute process until it suspends or finishes.
  // Returns the suspension reason for the engine to handle scheduling.
  auto RunUntilSuspend(ProcessState& state) -> Result<SuspendReason>;

  // Result of executing a function call.
  // Contains return value and output parameter values (for kOut/kInOut params).
  struct FunctionCallResult {
    RuntimeValue return_value;
    // Output values indexed by parameter position (only kOut/kInOut params).
    // Key is the parameter index in signature.params.
    std::vector<std::pair<size_t, RuntimeValue>> outputs;
  };

  // Execute a function call. Returns return value and output param values.
  auto RunFunction(
      FunctionId func_id, const std::vector<RuntimeValue>& args,
      DesignState* design_state) -> Result<FunctionCallResult>;

 private:
  // Evaluate Operand to RuntimeValue (always deep copy)
  auto EvalOperand(const ProcessState& state, const Operand& op)
      -> Result<RuntimeValue>;

  // Evaluate Rvalue to RuntimeValue (may mutate for pop methods)
  auto EvalRvalue(ProcessState& state, const Rvalue& rv, TypeId result_type)
      -> Result<RuntimeValue>;
  // Rvalue evaluation helpers (one per Rvalue info kind)
  auto EvalBuiltinCall(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalNewArray(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalEnumNextPrev(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalEnumName(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalAggregate(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalConcat(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalReplicate(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalIndexValidity(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalSFormat(ProcessState& state, const Rvalue& rv)
      -> Result<RuntimeValue>;
  auto EvalTestPlusargs(
      ProcessState& state, const Rvalue& rv, const TestPlusargsRvalueInfo& info)
      -> Result<RuntimeValue>;
  auto EvalFopen(ProcessState& state, const FopenRvalueInfo& info)
      -> Result<RuntimeValue>;
  auto EvalMathCall(
      ProcessState& state, const Rvalue& rv, const MathCallRvalueInfo& info)
      -> Result<RuntimeValue>;
  auto EvalArrayQuery(
      ProcessState& state, const Rvalue& rv, const ArrayQueryRvalueInfo& info)
      -> Result<RuntimeValue>;
  auto EvalSystemCmd(ProcessState& state, const SystemCmdRvalueInfo& info)
      -> Result<RuntimeValue>;

  // Execute SystemTfEffect
  auto ExecSystemTfEffect(ProcessState& state, const SystemTfEffect& effect)
      -> Result<void>;

  // Resolve PlaceRoot to storage (handles Local/Temp/Design)
  static auto ResolveRoot(const ProcessState& state, const PlaceRoot& root)
      -> const RuntimeValue&;
  static auto ResolveRootMut(ProcessState& state, const PlaceRoot& root)
      -> RuntimeValue&;

  // Resolve Place for reading (returns copy)
  auto ReadPlace(const ProcessState& state, PlaceId place_id)
      -> Result<RuntimeValue>;

  // Resolve Place for writing (fails if place ends with BitRange)
  auto WritePlace(ProcessState& state, PlaceId place_id)
      -> Result<std::reference_wrapper<RuntimeValue>>;

  // Store a value to a place (handles BitRange via read-modify-write)
  auto StoreToPlace(ProcessState& state, PlaceId place_id, RuntimeValue value)
      -> Result<void>;

  // Apply projections for read path (returns const location with optional bit
  // slice)
  auto ApplyProjectionsForRead(
      const ProcessState& state, const Place& place, const RuntimeValue& root)
      -> Result<ConstLocation>;

  // Apply projections for write path (returns mutable location with optional
  // bit slice)
  auto ApplyProjectionsForWrite(
      ProcessState& state, const Place& place, RuntimeValue& root)
      -> Result<Location>;

  // Execute Assign instruction (handles both Operand and Rvalue sources)
  auto ExecAssign(ProcessState& state, const Assign& assign) -> Result<void>;

  // Execute GuardedAssign instruction (handles both Operand and Rvalue sources)
  auto ExecGuardedAssign(ProcessState& state, const GuardedAssign& guarded)
      -> Result<void>;

  // Execute Effect instruction
  auto ExecEffect(ProcessState& state, const Effect& effect) -> Result<void>;

  // Execute Call instruction (unified call handling)
  auto ExecCall(ProcessState& state, const Call& call) -> Result<void>;

  // Execute user function call
  auto ExecUserCall(ProcessState& state, const Call& call, FunctionId func_id)
      -> Result<void>;

  // Execute system TF call (dispatch by opcode)
  auto ExecSystemTfCall(
      ProcessState& state, const Call& call, SystemTfOpcode opcode)
      -> Result<void>;

  // Execute $value$plusargs via unified Call
  auto ExecValuePlusargsCall(ProcessState& state, const Call& call)
      -> Result<void>;

  // Execute $fgets via unified Call
  auto ExecFgetsCall(ProcessState& state, const Call& call) -> Result<void>;

  // Execute $fread via unified Call
  auto ExecFreadCall(ProcessState& state, const Call& call) -> Result<void>;

  // Execute $fscanf via unified Call
  auto ExecFscanfCall(ProcessState& state, const Call& call) -> Result<void>;

  // Helper to commit $value$plusargs results with staging
  auto CommitValuePlusargsResult(
      ProcessState& state, const Call& call, RuntimeValue success,
      std::optional<RuntimeValue> parsed_value, PlaceId output_tmp,
      PlaceId output_dest) -> Result<void>;

  // Execute BuiltinCall instruction (container-mutating builtins)
  auto ExecBuiltinCall(ProcessState& state, const BuiltinCall& call)
      -> Result<void>;

  // Format display ops to string (no newline appended).
  auto FormatDisplayOps(
      const ProcessState& state, std::span<const FormatOp> ops)
      -> Result<std::string>;

  // Execute DisplayEffect
  auto ExecDisplayEffect(const ProcessState& state, const DisplayEffect& disp)
      -> Result<void>;

  // Execute SeverityEffect
  auto ExecSeverityEffect(
      const ProcessState& state, const SeverityEffect& severity)
      -> Result<void>;

  // Execute MemIOEffect ($readmemh/$readmemb/$writememh/$writememb)
  auto ExecMemIOEffect(ProcessState& state, const MemIOEffect& mem_io)
      -> Result<void>;

  // Execute FillPackedEffect (assignment pattern fill)
  auto ExecFillPackedEffect(ProcessState& state, const FillPackedEffect& fill)
      -> Result<void>;

  // Execute statement
  auto ExecStatement(ProcessState& state, const Statement& stmt)
      -> Result<void>;

  // Result of executing a terminator: target block + edge args for block params
  struct TerminatorResult {
    std::optional<BasicBlockId> target;
    std::vector<RuntimeValue> edge_args;  // Values for target block's params
  };

  // Execute terminator, return next block and edge args, or nullopt for
  // completion. Returns error on suspension terminators (Delay, Wait, Repeat).
  auto ExecTerminator(ProcessState& state, const Terminator& term)
      -> Result<TerminatorResult>;

  // Evaluate edge args (operands) to RuntimeValues
  auto EvalEdgeArgs(ProcessState& state, const std::vector<Operand>& args)
      -> Result<std::vector<RuntimeValue>>;

  // Bind edge args to block params (temps defined at block entry)
  static void BindBlockParams(
      ProcessState& state, const BasicBlock& block,
      std::vector<RuntimeValue> edge_args);

  const Arena* arena_;
  const TypeArena* types_;
  const lowering::DiagnosticContext* diag_ctx_ = nullptr;
  std::ostream* output_ = nullptr;
  std::vector<std::string> plusargs_;
  std::vector<std::string> instance_paths_;  // For %m formatting
  runtime::FileManager file_manager_;
  uint64_t simulation_time_ = 0;
  std::filesystem::path fs_base_dir_;

  // PRNG state for $random/$urandom. LCG with glibc constants.
  // Same algorithm as Engine to ensure matching behavior.
  uint32_t prng_state_ = 1;

  // Security: $system execution gate (default disabled)
  bool enable_system_ = false;
};

// Helper: Create ProcessState for a given process.
// Initializes locals and temps with default values based on their types.
auto CreateProcessState(
    const Arena& arena, const TypeArena& types, ProcessId process_id,
    DesignState* design_state) -> ProcessState;

// Info about initial processes to run (collected from all modules)
struct InitialProcessInfo {
  std::vector<ProcessId> initial_processes;  // All kOnce processes in order
};

// Helper: Create DesignState with properly initialized storage.
// Scans all processes in the design to collect type information for slots.
auto CreateDesignState(
    const Arena& arena, const TypeArena& types, const Design& design)
    -> DesignState;

// Create a default RuntimeValue for a given type.
// 2-state integral -> 0, 4-state integral -> X, string -> empty, etc.
auto CreateDefaultValue(const TypeArena& types, TypeId type_id) -> RuntimeValue;

// Collect all kOnce processes (initial blocks) from all modules.
// Returns nullopt if no initial process found.
auto CollectInitialProcesses(const Design& design, const Arena& arena)
    -> std::optional<InitialProcessInfo>;

// Result of running a simulation
struct SimulationResult {
  int exit_code = 0;
  std::string error_message;
};

// High-level API: Run simulation with MIR design.
// Encapsulates all interpreter internals (process states, suspension handling,
// engine integration). Returns exit code (0 = success).
// fs_base_dir: Absolute path for relative file I/O resolution.
// enable_system: Security gate for $system execution (default false).
auto RunSimulation(
    const Design& design, const Arena& mir_arena, const TypeArena& types,
    std::ostream* output, std::span<const std::string> plusargs,
    const std::filesystem::path& fs_base_dir, bool enable_system = false,
    const lowering::DiagnosticContext* diag_ctx = nullptr) -> SimulationResult;

}  // namespace lyra::mir::interp
