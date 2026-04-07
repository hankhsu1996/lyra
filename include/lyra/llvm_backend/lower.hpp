#pragma once

#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include "lyra/common/body_timescale.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/inspection_plan.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lowering_reports.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/lowering/origin_map_lookup.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/connection_endpoint.hpp"
#include "lyra/mir/construction_input.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Abstract interface for simulation instrumentation hooks.
// Provides milestone-based hooks that map to the simulation pipeline:
//   1. InitializeDesignState + LyraInitRuntime
//   2. OnAfterInitializeDesignState hook
//   3. Run init processes (package variable initialization)
//   4. OnBeforeRunSimulation hook
//   5. LyraRunSimulation (module processes)
//   6. String cleanup
//   7. Backend-emitted variable inspection (from GetTrackedVariables)
//   8. EmitPostSimulationReports hook
//   9. Return from main
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

  // Variables the test framework wants inspected post-simulation.
  // Backend builds typed placement from session data; hook only provides
  // identity (name + slot_id). No ownership or storage routing facts here.
  [[nodiscard]] virtual auto GetTrackedVariables() const
      -> std::span<const InspectedVarRef> {
    return {};
  }

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

  // Called after backend-emitted variable inspection is already emitted.
  // For non-inspection post-simulation reports (e.g., time report).
  // Variable inspection is backend-owned via GetTrackedVariables().
  virtual void EmitPostSimulationReports(
      Context& /*context*/, const std::vector<SlotInfo>& /*slots*/,
      llvm::Value* /*design_state*/, llvm::Value* /*abi_ptr*/) {
  }
};

// How the synthesized main() receives plusargs.
enum class MainAbi {
  // JIT/test mode: plusargs are baked into IR as global string constants.
  // main() takes no arguments: int main().
  kEmbeddedPlusargs,

  // AOT mode: main(argc, argv) forwards argv[1:] as plusargs.
  // fs_base_dir uses CWD at runtime instead of compile-time value.
  kArgvForwarding,
};

struct LoweringInput {
  const mir::Design* design = nullptr;
  const mir::ConstructionInput* construction = nullptr;
  const mir::Arena* mir_arena = nullptr;
  const TypeArena* type_arena = nullptr;
  const lowering::DiagnosticContext* diag_ctx = nullptr;
  const SourceManager* source_manager = nullptr;
  lowering::OriginMapLookup* origin_lookup = nullptr;
  SimulationHooks* hooks = nullptr;   // Optional instrumentation (nullable)
  std::string fs_base_dir;            // Base directory for file I/O (absolute)
  std::vector<std::string> plusargs;  // Command-line plusargs for $plusargs
  uint32_t feature_flags = 0;         // FeatureFlag bitmask for runtime
  // Text signal trace output path: empty = stdout, non-empty = file path.
  // Only meaningful when kEnableSignalTrace is set in feature_flags.
  std::string signal_trace_path;
  uint32_t iteration_limit = 0;  // 0 = default (1B)
  // Per-body timescale table from AST->HIR lowering.
  // Indexed by ModuleBodyId::value. Must be non-null.
  const std::vector<common::BodyTimeScale>* body_timescales = nullptr;
  bool force_two_state = false;  // Force 2-state LLVM representation
  bool collect_forwarding_analysis = false;
  MainAbi main_abi = MainAbi::kEmbeddedPlusargs;
  // DPI export wrapper descriptors for wrapper emission.
  // Owned by the MIR lowering result; passed by pointer (nullable).
  const std::vector<mir::DpiExportWrapperDesc>* dpi_export_wrappers = nullptr;
  // Endpoint-based resolved bindings from HIR-to-MIR lowering.
  // Backend adapter converts to flat ConnectionKernelEntry for layout.
  const mir::ResolvedBindingPlan* resolved_bindings = nullptr;
  // Recipe-based bound connections (new path, parallel to resolved_bindings).
  const std::vector<mir::BoundConnection>* bound_connections = nullptr;
};

struct LoweringResult {
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::Module> module;
  LoweringReport report;
};

auto LowerMirToLlvm(const LoweringInput& input) -> Result<LoweringResult>;

auto DumpLlvmIr(const LoweringResult& result) -> std::string;

// Emit variable registration calls for runtime inspection.
// Consumes a typed InspectionPlan built by BuildInspectionPlan.
// Uses std::visit to pattern-match DesignGlobalPlacement vs
// InstanceOwnedPlacement for each variable.
void EmitVariableInspection(
    Context& context, const InspectionPlan& plan,
    const std::vector<SlotInfo>& slots, llvm::Value* design_state,
    llvm::Value* abi_ptr);

// Emit time report call for test harness.
void EmitTimeReport(Context& context);

}  // namespace lyra::lowering::mir_to_llvm
