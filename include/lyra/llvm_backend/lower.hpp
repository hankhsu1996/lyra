#pragma once

#include <memory>
#include <string>
#include <vector>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/layout.hpp"
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
// Allows external code (e.g., test framework) to inject epilogue code
// without lower.cpp knowing the specifics.
class SimulationHooks {
 public:
  SimulationHooks() = default;
  virtual ~SimulationHooks() = default;
  SimulationHooks(const SimulationHooks&) = default;
  auto operator=(const SimulationHooks&) -> SimulationHooks& = default;
  SimulationHooks(SimulationHooks&&) = default;
  auto operator=(SimulationHooks&&) -> SimulationHooks& = default;

  // Emit code at end of simulation (before main returns).
  // Called after all processes complete and string cleanup.
  //
  // Parameters:
  //   context: LLVM code generation context
  //   slots: Design slot information (for variable metadata)
  //   design_state: LLVM value pointing to DesignState struct
  virtual void EmitEpilogue(
      Context& context, const std::vector<SlotInfo>& slots,
      llvm::Value* design_state) = 0;
};

struct LoweringInput {
  const mir::Design* design = nullptr;
  const mir::Arena* mir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
  SimulationHooks* hooks = nullptr;  // Optional instrumentation (nullable)
};

struct LoweringResult {
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
};

auto LowerMirToLlvm(const LoweringInput& input) -> LoweringResult;

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
