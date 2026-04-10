#pragma once

#include <cstdint>
#include <span>

namespace llvm {
class Value;
}  // namespace llvm

namespace lyra::mir {
struct DeferredAssertionSiteInfo;
}  // namespace lyra::mir

namespace lyra::lowering::mir_to_llvm {

// Function-entry-local execution mode carrier.
// Resolved once at function entry from the callable's ABI contract and
// threaded through statement/effect/call lowering for the active function.
// NOT stored on Context -- this is a local fact, not ambient state.
struct ActiveExecutionMode {
  // Caller's active decision owner, or null for contexts without one.
  // Non-null when the callable received the owner via hidden param
  // (user functions with accepts_decision_owner) or process entry setup.
  llvm::Value* decision_owner_id = nullptr;
};

// Body-level site context for effect lowering. Carries the body-local-to-
// global site ID mapping and deferred-site metadata needed by CoverHitEffect
// and EnqueueDeferredAssertionEffect codegen. Constructed once per body
// session from CompiledModuleSpecInput, threaded through statement/effect
// lowering. NOT stored on Context -- this is a session input, not ambient
// state.
struct BodySiteContext {
  uint32_t deferred_site_base = 0;
  uint32_t cover_site_base = 0;
  std::span<const mir::DeferredAssertionSiteInfo> deferred_sites;

  // Look up site info by body-local ID. Returns nullptr if out of range.
  // Defined out-of-line (DeferredAssertionSiteInfo is incomplete here).
  [[nodiscard]] auto GetDeferredSiteInfo(uint32_t body_local_id) const
      -> const mir::DeferredAssertionSiteInfo*;
};

}  // namespace lyra::lowering::mir_to_llvm
