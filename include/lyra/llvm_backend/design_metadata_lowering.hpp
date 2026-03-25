#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/lowering_reports.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/metadata/design_metadata.hpp"
#include "lyra/mir/design.hpp"

namespace lyra::mir {
class Arena;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

class Context;
struct Layout;
struct DesignLayout;
struct SlotInfo;

// Extract slot metadata inputs from LLVM layout into plain link structs.
auto ExtractSlotMetaInputs(
    const std::vector<SlotInfo>& slots, const DesignLayout& design_layout)
    -> std::vector<metadata::SlotMetaInput>;

// Extract back-edge site inputs from accumulated codegen origins.
auto PrepareBackEdgeSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<metadata::BackEdgeSiteInput>;

// Extract connection descriptor entries from canonical layout.
// Trigger observation data is pre-resolved in the layout; no arena access
// needed.
auto ExtractConnectionDescriptorEntries(const Layout& layout)
    -> std::vector<metadata::ConnectionDescriptorEntry>;

// Build final trace-signal metadata inputs from compile-owned provenance.
// Assembles hierarchical names, computes bit widths, maps trace kinds.
// Each slot's provenance carries its scope kind and scope ref for O(1) lookup.
auto PrepareTraceSignalMetaInputs(
    const std::vector<mir::SlotTraceProvenance>& provenance,
    const std::vector<char>& trace_string_pool,
    const std::vector<TypeId>& slot_types,
    const std::vector<mir::SlotKind>& slot_kinds,
    const std::vector<std::string>& instance_paths, const TypeArena& types,
    const DesignLayout& design_layout)
    -> std::vector<metadata::TraceSignalMetaInput>;

// Find port-binding forwarding candidates. Analysis only -- does not
// modify the connection graph or prove transform safety. Emits all
// candidates that pass the current provable checks. Unresolved checks
// (like active trace references) are flagged but do not exclude.
auto FindPortBindingForwardingCandidates(
    std::span<const metadata::ConnectionDescriptorEntry> connections,
    const Layout& layout) -> std::vector<PortBindingForwardingCandidate>;

// Result of emitting DesignMetadata as LLVM globals.
struct MetadataGlobals {
  llvm::Constant* slot_meta_words = nullptr;
  uint32_t slot_meta_count = 0;
  llvm::Constant* back_edge_site_meta_words = nullptr;
  uint32_t back_edge_site_meta_count = 0;
  llvm::Constant* back_edge_site_meta_pool = nullptr;
  uint32_t back_edge_site_meta_pool_size = 0;
  llvm::Value* conn_desc_table = nullptr;
  uint32_t conn_desc_count = 0;
  llvm::Value* instance_paths_array = nullptr;
  uint32_t instance_path_count = 0;
  llvm::Constant* trace_signal_meta_words = nullptr;
  uint32_t trace_signal_meta_word_count = 0;
  llvm::Constant* trace_signal_meta_pool = nullptr;
  uint32_t trace_signal_meta_pool_size = 0;
};

// Emit DesignMetadata as LLVM globals. Pure emission only: no metadata
// table packing or serialization. All tables in DesignMetadata are already
// runtime-shaped.
auto EmitDesignMetadataGlobals(
    Context& context, const metadata::DesignMetadata& metadata,
    llvm::IRBuilder<>& builder) -> MetadataGlobals;

}  // namespace lyra::lowering::mir_to_llvm
