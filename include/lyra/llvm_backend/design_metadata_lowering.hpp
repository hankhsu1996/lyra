#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/realization/design_metadata.hpp"

namespace lyra::mir {
struct Design;
class Arena;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

class Context;
struct Layout;
struct DesignLayout;
struct SlotInfo;

// Extract slot metadata inputs from LLVM layout into plain link structs.
auto ExtractSlotMetaInputs(
    Context& context, const std::vector<SlotInfo>& slots,
    const DesignLayout& design_layout, const llvm::DataLayout& dl,
    const TypeArena& types) -> std::vector<realization::SlotMetaInput>;

// Extract scheduled process inputs into plain link structs.
auto PrepareScheduledProcessInputs(
    const mir::Design& design, const mir::Arena& mir_arena,
    const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager,
    const std::vector<struct ScheduledProcess>& scheduled_processes,
    size_t num_init) -> std::vector<realization::ScheduledProcessInput>;

// Extract loop site inputs from accumulated codegen origins.
auto PrepareLoopSiteInputs(
    const Context& context, const lowering::DiagnosticContext* diag_ctx,
    const SourceManager* source_manager)
    -> std::vector<realization::LoopSiteInput>;

// Extract connection descriptor entries from LLVM layout.
auto ExtractConnectionDescriptorEntries(
    const mir::Design& design, const mir::Arena& mir_arena,
    const TypeArena& type_arena, const Layout& layout,
    const llvm::DataLayout& dl, llvm::LLVMContext& ctx, bool force_two_state)
    -> std::vector<realization::ConnectionDescriptorEntry>;

// Prepare comb kernel inputs from layout data.
auto PrepareCombKernelInputs(const Layout& layout, size_t num_init)
    -> std::vector<realization::CombKernelInput>;

// Prepare instance paths from design.
auto PrepareInstancePaths(const mir::Design& design)
    -> std::vector<std::string>;

// Result of emitting DesignMetadata as LLVM globals.
struct MetadataGlobals {
  llvm::Constant* slot_meta_words = nullptr;
  uint32_t slot_meta_count = 0;
  llvm::Constant* process_meta_words = nullptr;
  uint32_t process_meta_count = 0;
  llvm::Constant* process_meta_pool = nullptr;
  uint32_t process_meta_pool_size = 0;
  llvm::Constant* loop_site_meta_words = nullptr;
  uint32_t loop_site_meta_count = 0;
  llvm::Constant* loop_site_meta_pool = nullptr;
  uint32_t loop_site_meta_pool_size = 0;
  llvm::Value* conn_desc_table = nullptr;
  uint32_t conn_desc_count = 0;
  llvm::Value* comb_kernel_words = nullptr;
  uint32_t comb_kernel_word_count = 0;
  llvm::Value* instance_paths_array = nullptr;
  uint32_t instance_path_count = 0;
};

// Emit DesignMetadata as LLVM globals. Pure emission only: no metadata
// table packing or serialization. All tables in DesignMetadata are already
// runtime-shaped.
auto EmitDesignMetadataGlobals(
    Context& context, const realization::DesignMetadata& metadata,
    llvm::IRBuilder<>& builder) -> MetadataGlobals;

}  // namespace lyra::lowering::mir_to_llvm
