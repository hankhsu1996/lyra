#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/deferred_assertion_site.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

// Canonical LLVM struct type for DeferredAssertionExecContext.
// Layout: { ptr instance }
// Single source of truth -- use everywhere the ABI is declared or decoded.
auto BuildDeferredExecContextType(llvm::LLVMContext& ctx) -> llvm::StructType*;

// Build the LLVM struct type for a deferred assertion payload from semantic
// field types. Each TypeId is mapped through the callable ABI classifier
// to determine the concrete LLVM storage type.
// Single source of truth -- used by thunk definition (decode) and
// enqueue emission (pack).
auto BuildDeferredPayloadStructType(
    llvm::LLVMContext& llvm_ctx, std::span<const TypeId> field_types,
    const TypeArena& types, bool force_two_state) -> llvm::StructType*;

// Site-first helpers for extracting user-call actions from semantic site data.
// Returns nullptr if the action is absent or is a built-in type (CoverHit,
// default fail report).

auto GetDeferredPassUserCallAction(const mir::DeferredAssertionSiteInfo& site)
    -> const mir::DeferredUserCallAction*;

auto GetDeferredFailUserCallAction(const mir::DeferredAssertionSiteInfo& site)
    -> const mir::DeferredUserCallAction*;

// Backend-local derived types for deferred assertion call plan derivation.
// These are temporary backend data, never stored in Design or Context.

struct DeferredPayloadLayout {
  std::vector<TypeId> field_types;
};

enum class DeferredBindingKind : uint8_t {
  kPayloadField,
  kLiveRef,
  kConstLiveRef,
};

struct DeferredDerivedActual {
  DeferredBindingKind kind = DeferredBindingKind::kPayloadField;
  uint32_t payload_index = 0;
  uint32_t ref_index = 0;
  mir::PlaceId ref_place{};
};

struct DeferredDerivedCallPlan {
  mir::FunctionId callee;
  DeferredPayloadLayout payload;
  std::vector<DeferredDerivedActual> actuals;
  uint32_t ref_count = 0;
};

// Single canonical derivation from semantic DeferredUserCallAction to
// backend call plan. All backend consumers (enqueue, thunk body, metadata)
// must use this function as the only source of payload layout and ref
// binding order.
auto DeriveDeferredCallPlan(const mir::DeferredUserCallAction& action)
    -> DeferredDerivedCallPlan;

// Backend-resolved callee metadata for a single deferred user-call action.
// Captured during body session (when DeclaredFunctionScope is active),
// consumed by post-session thunk compilation. Eliminates post-session
// ambient lookup of body-local FunctionIds.
struct DeferredCalleeBackendInfo {
  llvm::Function* llvm_func = nullptr;
  bool is_module_scoped = false;
};

// Per-site captured callee backend info. Session-internal: captured and
// consumed within CompileModuleSpecSession for per-body thunk compilation.
struct DeferredSiteCalleeInfo {
  std::optional<DeferredCalleeBackendInfo> pass_callee;
  std::optional<DeferredCalleeBackendInfo> fail_callee;
};

// Per-site compiled artifact for deferred assertion thunks.
// Compiled per-body inside the spec session, then concatenated into a
// design-global array for runtime metadata emission.
struct DeferredSiteCompiledArtifact {
  llvm::Function* pass_thunk = nullptr;
  llvm::Function* fail_thunk = nullptr;
  uint32_t pass_payload_size = 0;
  uint32_t fail_payload_size = 0;
};

}  // namespace lyra::lowering::mir_to_llvm
