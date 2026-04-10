#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/origin_id.hpp"
#include "lyra/llvm_backend/lowering_reports.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/metadata/design_metadata.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct Layout;

// Extract back-edge site inputs from pre-assembled origin array.
auto PrepareBackEdgeSiteInputs(
    std::span<const common::OriginId> origins,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<metadata::BackEdgeSiteInput>;

// Extract connection descriptor entries from canonical layout.
// Trigger observation data is pre-resolved in the layout; no arena access
// needed.
auto ExtractConnectionDescriptorEntries(const Layout& layout)
    -> std::vector<metadata::ConnectionDescriptorEntry>;

// Find port-binding forwarding candidates. Analysis only -- does not
// modify the connection graph or prove transform safety. Emits all
// candidates that pass the current provable checks. Unresolved checks
// (like active trace references) are flagged but do not exclude.
auto FindPortBindingForwardingCandidates(
    std::span<const metadata::ConnectionDescriptorEntry> connections,
    const Layout& layout) -> std::vector<PortBindingForwardingCandidate>;

// Result of emitting DesignMetadata as LLVM globals.
struct MetadataGlobals {
  llvm::Constant* back_edge_site_meta_words = nullptr;
  uint32_t back_edge_site_meta_count = 0;
  llvm::Constant* back_edge_site_meta_pool = nullptr;
  uint32_t back_edge_site_meta_pool_size = 0;
  llvm::Value* conn_desc_table = nullptr;
  uint32_t conn_desc_count = 0;
};

// Emit DesignMetadata as LLVM globals. Pure emission only: no metadata
// table packing or serialization. All tables in DesignMetadata are already
// runtime-shaped.
auto EmitDesignMetadataGlobals(
    Context& context, const metadata::DesignMetadata& metadata)
    -> MetadataGlobals;

}  // namespace lyra::lowering::mir_to_llvm
