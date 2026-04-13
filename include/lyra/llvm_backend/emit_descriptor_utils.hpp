#pragma once

// Internal header for realization descriptor emission. Shared between
// emit_body_descriptors.cpp, emit_connection_descriptors.cpp,
// emit_realization_descriptors.cpp, and emit_descriptor_utils.cpp.
// Not part of the public backend API.

#include <cstdint>
#include <span>
#include <string>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

#include "lyra/llvm_backend/codegen_session.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/runtime/body_realization_desc.hpp"
#include "lyra/runtime/storage_construction_recipe.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Cross-TU emission carriers. Only types that flow between the body,
// connection, constructor, and utility translation units appear here.
// Types used within a single TU belong in that TU's anonymous namespace.

struct MetaTemplateEmission {
  llvm::Constant* entries_ptr = nullptr;
  uint32_t num_entries = 0;
  llvm::Constant* pool_ptr = nullptr;
  uint32_t pool_size = 0;
};

struct TriggerTemplateEmission {
  llvm::Constant* entries_ptr = nullptr;
  uint32_t num_entries = 0;
  llvm::Constant* ranges_ptr = nullptr;
  uint32_t num_ranges = 0;
  llvm::Constant* shapes_ptr = nullptr;
  llvm::Constant* groupable_ptr = nullptr;
};

struct ObservableDescriptorEmission {
  llvm::Constant* entries_ptr = nullptr;
  uint32_t num_entries = 0;
  llvm::Constant* pool_ptr = nullptr;
  uint32_t pool_size = 0;
};

struct InitDescriptorEmission {
  llvm::Constant* recipe_ptr = nullptr;
  uint32_t num_recipe_ops = 0;
  llvm::Constant* recipe_roots_ptr = nullptr;
  uint32_t num_recipe_roots = 0;
  llvm::Constant* recipe_child_indices_ptr = nullptr;
  uint32_t num_recipe_child_indices = 0;
  llvm::Constant* param_slots_ptr = nullptr;
  uint32_t num_param_slots = 0;
};

struct BodyDescriptorPackageEmission {
  llvm::Constant* header_ptr = nullptr;
  llvm::Constant* entries_ptr = nullptr;
  uint32_t num_processes = 0;
  MetaTemplateEmission meta;
  TriggerTemplateEmission triggers;
  llvm::Constant* comb_entries_ptr = nullptr;
  uint32_t num_comb_entries = 0;
  llvm::Constant* comb_kernels_ptr = nullptr;
  uint32_t num_comb_kernels = 0;
  ObservableDescriptorEmission observable;
  InitDescriptorEmission init;
  llvm::Constant* decision_tables_ptr = nullptr;
  uint32_t num_decision_tables = 0;
};

// Body descriptor emission (emit_body_descriptors.cpp).
auto EmitBodyRealizationDescs(
    Context& context, const Layout& layout,
    std::span<const CodegenSession::BodyCompiledFuncs> body_compiled_funcs)
    -> std::vector<BodyDescriptorPackageEmission>;

// Connection descriptor emission (emit_connection_descriptors.cpp).
auto EmitConnectionRealizationDescs(
    Context& context, const Layout& layout,
    std::span<llvm::Function* const> process_funcs, size_t num_init)
    -> llvm::Constant*;
auto EmitConnectionMetaTemplate(Context& context, const Layout& layout)
    -> MetaTemplateEmission;
auto EmitConnectionTriggerTemplate(Context& context, const Layout& layout)
    -> TriggerTemplateEmission;

// Template and descriptor emission (emit_descriptor_utils.cpp).
auto EmitObservableDescriptorTemplate(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    const OwnedObservableDescriptorTemplate& tmpl,
    const std::string& name_prefix) -> ObservableDescriptorEmission;
auto EmitInitDescriptor(
    llvm::LLVMContext& ctx, llvm::Module& mod,
    std::span<const runtime::StorageConstructionOp> recipe,
    std::span<const uint32_t> recipe_roots,
    std::span<const uint32_t> recipe_child_indices,
    std::span<const runtime::ParamInitSlotEntry> param_slots,
    const std::string& name_prefix) -> InitDescriptorEmission;

// Template validation (emit_descriptor_utils.cpp).
void ValidateOwnedMetaTemplate(
    const OwnedProcessMetaTemplate& tmpl, const char* caller);
void ValidateOwnedTriggerTemplate(
    const OwnedTriggerTemplate& tmpl, const char* caller);
void ValidateOwnedCombTemplate(
    const OwnedCombTemplate& tmpl, const char* caller);

}  // namespace lyra::lowering::mir_to_llvm
