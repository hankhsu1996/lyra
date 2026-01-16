#pragma once

#include <optional>
#include <ostream>

#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/interp/frame.hpp"

namespace lyra::mir::interp {

// Process execution status
enum class ProcessStatus {
  kRunning,
  kFinished,
};

// State of a single process execution
struct ProcessState {
  ProcessId process;
  BasicBlockId current_block;
  size_t instruction_index = 0;
  Frame frame;
  ProcessStatus status = ProcessStatus::kRunning;
};

// MIR Interpreter: executes a single process to completion.
//
// The interpreter is a semantic reference implementation. It supports:
// - Single initial process (ProcessKind::kOnce)
// - Basic control flow (Jump, Branch, Finish)
// - Expressions (Unary, Binary operations)
// - Local and Temp storage
//
// The interpreter hard-fails on suspension terminators (Delay, Wait, Repeat)
// as these require a scheduler/runtime.
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
  auto Run(ProcessState& state) -> ProcessStatus;

 private:
  // Evaluate Operand to RuntimeValue (always deep copy)
  auto EvalOperand(const ProcessState& state, const Operand& op)
      -> RuntimeValue;

  // Evaluate Rvalue to RuntimeValue
  auto EvalRvalue(const ProcessState& state, const Rvalue& rv) -> RuntimeValue;

  // Resolve Place for reading (returns copy)
  auto ReadPlace(const ProcessState& state, PlaceId place_id) -> RuntimeValue;

  // Resolve Place for writing
  auto WritePlace(ProcessState& state, PlaceId place_id) -> RuntimeValue&;

  // Execute Assign instruction
  void ExecAssign(ProcessState& state, const Assign& assign);

  // Execute Compute instruction
  void ExecCompute(ProcessState& state, const Compute& compute);

  // Execute instruction
  void ExecInstruction(ProcessState& state, const Instruction& inst);

  // Execute terminator, return next block or nullopt for completion
  auto ExecTerminator(ProcessState& state, const Terminator& term)
      -> std::optional<BasicBlockId>;

  // Execute system call (e.g., $display, $write)
  auto ExecSyscall(const ProcessState& state, const Rvalue& rv) -> RuntimeValue;

  const Arena* arena_;
  const TypeArena* types_;
  std::ostream* output_ = nullptr;
};

// Helper: Create ProcessState for a given process
auto CreateProcessState(const Arena& arena, ProcessId process_id)
    -> ProcessState;

}  // namespace lyra::mir::interp
