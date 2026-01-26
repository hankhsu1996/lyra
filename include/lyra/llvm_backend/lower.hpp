#pragma once

#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Information about a module variable for runtime inspection
struct VariableInfo {
  std::string name;
  size_t slot_id;  // Index into design.slot_table
};

// Abstract interface for simulation instrumentation hooks.
// Provides milestone-based hooks that map to the simulation pipeline:
//   1. InitializeDesignState + LyraInitRuntime
//   2. OnAfterInitializeDesignState hook
//   3. Run init processes (package variable initialization)
//   4. OnBeforeRunSimulation hook
//   5. LyraRunSimulation (module processes)
//   6. String cleanup
//   7. OnAfterRunSimulation hook
//   8. Return from main
//
// All hooks have empty default implementations - override only what you need.
class SimulationHooks {
 public:
  SimulationHooks() = default;
  virtual ~SimulationHooks() = default;
  SimulationHooks(const SimulationHooks&) = default;
  auto operator=(const SimulationHooks&) -> SimulationHooks& = default;
  SimulationHooks(SimulationHooks&&) = default;
  auto operator=(SimulationHooks&&) -> SimulationHooks& = default;

  // Called after DesignState is initialized (before any processes run).
  // Use for: pre-simulation setup, initial state inspection.
  virtual void OnAfterInitializeDesignState(
      Context& /*context*/, const std::vector<SlotInfo>& /*slots*/,
      llvm::Value* /*design_state*/) {
  }

  // Called after init processes complete, before module processes start.
  // Use for: inspecting state after package initialization.
  virtual void OnBeforeRunSimulation(
      Context& /*context*/, const std::vector<SlotInfo>& /*slots*/,
      llvm::Value* /*design_state*/) {
  }

  // Called after simulation completes (before main returns).
  // Use for: final variable inspection, timing reports, test assertions.
  virtual void OnAfterRunSimulation(
      Context& /*context*/, const std::vector<SlotInfo>& /*slots*/,
      llvm::Value* /*design_state*/) {
  }
};

struct LoweringInput {
  const mir::Design* design = nullptr;
  const mir::Arena* mir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
  const lowering::DiagnosticContext* diag_ctx = nullptr;
  SimulationHooks* hooks = nullptr;  // Optional instrumentation (nullable)
};

struct LoweringResult {
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
};

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult>;

auto DumpLlvmIr(const LoweringResult& result) -> std::string;

// Utility functions for use in SimulationHooks implementations.
// These emit LLVM IR for common instrumentation tasks.

// Emit variable registration calls for runtime inspection.
// Registers each variable with the runtime and emits a snapshot call.
void EmitVariableInspection(
    Context& context, const std::vector<VariableInfo>& variables,
    const std::vector<SlotInfo>& slots, llvm::Value* design_state);

// Emit time report call for test harness.
void EmitTimeReport(Context& context);

}  // namespace lyra::lowering::mir_to_llvm
