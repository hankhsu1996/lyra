#pragma once

#include <optional>
#include <span>
#include <string>
#include <unordered_map>
#include <vector>

#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/process.hpp"

namespace lyra::mir {
struct Design;
struct ConstructionInput;
class Arena;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

struct LoweringInput;

// Runtime metadata products for one body. Parallel to
// Layout::body_realization_infos. Produced by ExtractRuntimeData,
// consumed by ApplyRuntimeDataToLayout.
struct BodyRuntimeProducts {
  OwnedProcessMetaTemplate meta;
  OwnedTriggerTemplate triggers;
  OwnedCombTemplate comb;
  OwnedObservableDescriptorTemplate observable_descriptors;
  BodyInitDescriptor init;
  std::vector<std::vector<runtime::DecisionMetaEntry>> decision_metas;
  std::vector<std::vector<std::string>> decision_meta_files;
};

// All runtime data extraction products.
struct RuntimeExtractionProducts {
  // Parallel to layout.body_realization_infos.
  std::vector<BodyRuntimeProducts> body_products;

  // Connection metadata and trigger templates.
  Layout::ConnectionTemplates connection_templates;

  // Package-level runtime data.
  OwnedObservableDescriptorTemplate package_observable_descriptors;
  PackageInitDescriptor package_init_descriptor;

  // Construction program (instance realization data).
  ConstructionProgramData construction_program;
};

// Extract all post-layout runtime data from the design.
// Reads layout structural facts but does not mutate layout.
// Returns products that must be applied via ApplyRuntimeDataToLayout.
auto ExtractRuntimeData(
    const LoweringInput& input, const Layout& layout,
    const std::unordered_map<
        const mir::ModuleBody*,
        std::vector<std::optional<ProcessTriggerEntry>>>&
        body_to_process_triggers,
    std::span<const uint32_t> instance_body_group) -> RuntimeExtractionProducts;

// Apply extracted runtime data products onto Layout and RealizationData.
// This is the single narrow writeback surface.
void ApplyRuntimeDataToLayout(
    RuntimeExtractionProducts products, Layout& layout,
    RealizationData& realization);

}  // namespace lyra::lowering::mir_to_llvm
