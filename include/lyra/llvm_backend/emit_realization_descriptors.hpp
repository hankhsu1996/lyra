#pragma once

#include <span>
#include <vector>

#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>

#include "lyra/llvm_backend/codegen_session.hpp"

namespace llvm {
class Value;
}

namespace lyra::lowering::mir_to_llvm {

class Context;
struct Layout;

// Product of realization descriptor emission + constructor function
// generation. Contains every LLVM IR value that EmitDesignMain needs
// from the realization phase. All internal descriptor packaging
// (body descriptors, templates, schemas) is fully consumed inside
// the emission and does not appear here.
//
// The nested groups reflect the runtime handoff contract:
// process scheduling metadata, dispatch partition metadata, and
// observation/topology metadata. Each group corresponds to a
// constructor-produced result set consumed by the runtime ABI.
struct RealizationEmissionResult {
  llvm::Value* states_array = nullptr;
  llvm::Value* num_total = nullptr;
  llvm::Value* result_handle = nullptr;
  llvm::Function* destroy_fn = nullptr;

  // Constructor-produced process scheduling metadata (word table + pool).
  struct ProcessMeta {
    llvm::Value* words = nullptr;
    llvm::Value* word_count = nullptr;
    llvm::Value* pool = nullptr;
    llvm::Value* pool_size = nullptr;
  } process_meta;

  // Constructor-produced dispatch metadata (trigger + comb partitions).
  struct DispatchMeta {
    llvm::Value* trigger_words = nullptr;
    llvm::Value* trigger_word_count = nullptr;
    llvm::Value* comb_words = nullptr;
    llvm::Value* comb_word_count = nullptr;
  } dispatch_meta;

  // Constructor-produced observation topology (slots, traces, instances).
  struct ObservationMeta {
    llvm::Value* slot_meta_words = nullptr;
    llvm::Value* slot_meta_count = nullptr;
    llvm::Value* trace_meta_words = nullptr;
    llvm::Value* trace_meta_word_count = nullptr;
    llvm::Value* trace_meta_pool = nullptr;
    llvm::Value* trace_meta_pool_size = nullptr;
    llvm::Value* instance_paths = nullptr;
    llvm::Value* instance_path_count = nullptr;
    llvm::Value* instance_ptrs = nullptr;
    llvm::Value* instance_count = nullptr;
    llvm::Value* instance_bundles = nullptr;
    llvm::Value* instance_bundle_count = nullptr;
  } observation_meta;

  // Connection function table global for EmitRunSimulation.
  llvm::Constant* connection_funcs = nullptr;
};

// Emit all realization descriptors (body descriptors, connection
// descriptors, schemas, slot byte offsets, metadata templates) and
// the runtime constructor function. Returns the constructor-produced
// values needed by the main emission phase.
auto EmitRealizationAndConstructor(
    Context& context, const Layout& layout, llvm::Value* design_state,
    std::span<const CodegenSession::BodyCompiledFuncs> body_compiled_funcs,
    const std::vector<llvm::Function*>& process_funcs, size_t num_init,
    const ConstructionProgramData& construction_program)
    -> RealizationEmissionResult;

}  // namespace lyra::lowering::mir_to_llvm
