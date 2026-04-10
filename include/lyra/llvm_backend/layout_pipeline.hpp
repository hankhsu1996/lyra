#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "lyra/llvm_backend/connection_lowering.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"

namespace llvm {
class DataLayout;
class LLVMContext;
}  // namespace llvm

namespace lyra::lowering::mir_to_llvm {

struct LoweringInput;

// Module-level topology facts extracted from the design.
struct TopologyPlan {
  std::vector<LayoutModulePlan> module_plans;
  uint32_t total_design_slot_count = 0;
};

// Extract module plans and design-global slot count from the design.
auto BuildTopologyPlan(const LoweringInput& input) -> TopologyPlan;

// Build the full backend layout from topology + connection artifacts.
// Computes instance ranges, body storage layouts, design layout, and
// the final Layout in one call.
auto BuildBackendLayout(
    const LoweringInput& input, const TopologyPlan& topology,
    LoweredConnectionArtifacts& connections, llvm::LLVMContext& llvm_ctx,
    const llvm::DataLayout& data_layout) -> std::unique_ptr<Layout>;

}  // namespace lyra::lowering::mir_to_llvm
