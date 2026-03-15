#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/design.hpp"
#include "lyra/realization/design_metadata.hpp"

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
    -> std::vector<realization::SlotMetaInput>;

// Extract scheduled process inputs into plain link structs.
auto PrepareScheduledProcessInputs(
    const std::vector<std::string>& instance_paths, const mir::Arena& mir_arena,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager,
    const std::vector<struct ScheduledProcess>& scheduled_processes,
    size_t num_init) -> std::vector<realization::ScheduledProcessInput>;

// Extract back-edge site inputs from accumulated codegen origins.
auto PrepareBackEdgeSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<realization::BackEdgeSiteInput>;

// Extract connection descriptor entries from LLVM layout.
auto ExtractConnectionDescriptorEntries(
    const std::vector<TypeId>& slot_types, const mir::Arena& mir_arena,
    const TypeArena& type_arena, const Layout& layout,
    const llvm::DataLayout& dl, llvm::LLVMContext& ctx, bool force_two_state)
    -> std::vector<realization::ConnectionDescriptorEntry>;

// Prepare comb kernel inputs from layout data.
// Resolves symbolic trigger observations to concrete byte ranges and
// canonicalizes to one trigger per (kernel, slot).
auto PrepareCombKernelInputs(
    const std::vector<TypeId>& slot_types, const mir::Arena& mir_arena,
    const TypeArena& types, const Layout& layout, const llvm::DataLayout& dl,
    llvm::LLVMContext& ctx, bool force_two_state, size_t num_init)
    -> std::vector<realization::CombKernelInput>;

// Build final trace-signal metadata inputs from compile-owned provenance.
// Assembles hierarchical names, computes bit widths, maps trace kinds.
// Each slot's provenance carries its scope kind and scope ref for O(1) lookup.
auto PrepareTraceSignalMetaInputs(
    const std::vector<mir::SlotTraceProvenance>& provenance,
    const std::vector<char>& trace_string_pool,
    const std::vector<TypeId>& slot_types,
    const std::vector<mir::SlotKind>& slot_kinds,
    const std::vector<std::string>& instance_paths, const TypeArena& types)
    -> std::vector<realization::TraceSignalMetaInput>;

// Result of emitting DesignMetadata as LLVM globals.
struct MetadataGlobals {
  llvm::Constant* slot_meta_words = nullptr;
  uint32_t slot_meta_count = 0;
  llvm::Constant* process_meta_words = nullptr;
  uint32_t process_meta_count = 0;
  llvm::Constant* process_meta_pool = nullptr;
  uint32_t process_meta_pool_size = 0;
  llvm::Constant* back_edge_site_meta_words = nullptr;
  uint32_t back_edge_site_meta_count = 0;
  llvm::Constant* back_edge_site_meta_pool = nullptr;
  uint32_t back_edge_site_meta_pool_size = 0;
  llvm::Value* conn_desc_table = nullptr;
  uint32_t conn_desc_count = 0;
  llvm::Value* comb_kernel_words = nullptr;
  uint32_t comb_kernel_word_count = 0;
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
    Context& context, const realization::DesignMetadata& metadata,
    llvm::IRBuilder<>& builder) -> MetadataGlobals;

}  // namespace lyra::lowering::mir_to_llvm
