#pragma once

#include <cstdint>
#include <span>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include "lyra/common/origin_id.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/lowering_reports.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/metadata/design_metadata.hpp"

namespace lyra::lowering::mir_to_llvm {

// Extract back-edge site inputs from pre-assembled origin array.
auto PrepareBackEdgeSiteInputs(
    std::span<const common::OriginId> origins,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<metadata::BackEdgeSiteInput>;

// Extract connection descriptor entries from layout components.
// Computes body-relative byte offsets from object/member identity.
auto ExtractConnectionDescriptorEntries(
    std::span<const ConnectionKernelEntry> kernel_entries,
    std::span<const Layout::BodyRealizationInfo> body_realization_infos,
    std::span<const uint32_t> instance_body_groups)
    -> std::vector<metadata::ConnectionDescriptorEntry>;

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
    llvm::Module& mod, llvm::LLVMContext& ctx,
    const metadata::DesignMetadata& metadata) -> MetadataGlobals;

}  // namespace lyra::lowering::mir_to_llvm
