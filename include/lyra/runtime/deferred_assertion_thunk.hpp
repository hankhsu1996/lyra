#pragma once

#include <cstdint>

#include "lyra/common/deferred_assertion_abi.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

// Sentinel value indicating no instance binding (design-global scope).
inline constexpr uint32_t kNoInstanceId = UINT32_MAX;

// Execution context passed to deferred assertion thunks at drain time.
// Provides the canonical module binding needed for specialization-local
// access inside the thunk. The thunk derives this_ptr and instance_id
// from the RuntimeInstance pointer, matching the normal shared-body
// binding model (EmitSharedBodyBindingSetup).
//
// For design-global scope (no instance), instance is nullptr.
struct DeferredAssertionExecContext {
  // Canonical instance pointer (RuntimeInstance*). Provides:
  // - this_ptr via instance->storage.inline_base
  // - instance_id via instance->instance_id
  // nullptr for design-global scope.
  RuntimeInstance* instance = nullptr;
};

// Named function pointer type for deferred assertion action thunks.
// Use this type everywhere: runtime metadata, emitted LLVM globals,
// and dispatch code. Do not open-code raw function pointer types.
using DeferredAssertionThunkFn = void (*)(
    void* design_state, void* engine, const DeferredAssertionExecContext* ctx,
    const void* payload, const DeferredAssertionRefBindingAbi* ref_bindings,
    uint32_t ref_count);

}  // namespace lyra::runtime
