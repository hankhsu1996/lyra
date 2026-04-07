#pragma once

#include <cstdint>

#include "lyra/common/deferred_assertion_abi.hpp"
#include "lyra/runtime/deferred_assertion_thunk.hpp"

// Runtime helpers for immediate assertion support.
// Callable from LLVM-generated code via extern "C" linkage.

extern "C" {

// Record a hit for an immediate cover statement.
// engine: pointer to Engine (cast from void*)
// site_index: design-global cover site index
void LyraRecordImmediateCoverHit(void* engine, uint32_t site_index);

// Runtime deferred assertion site metadata entry. One per site, passed as
// a flat array through the ABI. Indexes match DeferredAssertionSiteId.
struct LyraDeferredAssertionSiteMeta {
  uint8_t kind;  // 0=assert, 1=assume, 2=cover
  uint8_t pad0;
  uint16_t pad1;
  uint32_t cover_site_id;  // UINT32_MAX if not a cover site
  const char* origin_file;
  uint32_t origin_line;
  uint32_t origin_col;
  // A2e: thunk dispatch entries. nullptr if no user action for that
  // disposition. Uses the named DeferredAssertionThunkFn type.
  lyra::runtime::DeferredAssertionThunkFn pass_thunk;
  lyra::runtime::DeferredAssertionThunkFn fail_thunk;
  uint32_t pass_payload_size;
  uint32_t fail_payload_size;
};

// Register deferred assertion site metadata table.
// Called from init code before simulation. sites array must remain
// valid for program lifetime (points into LLVM-emitted globals).
void LyraInitDeferredAssertionSites(
    void* engine, const LyraDeferredAssertionSiteMeta* sites,
    uint32_t num_sites);

// Enqueue an observed deferred immediate assertion record (LRM 16.4).
// Called from LLVM-generated code when a deferred assertion condition
// is evaluated. The record is stamped with the current flush generation
// and matured at settle boundary.
//
// Enqueue with by-value payload capture and instance context.
// payload_ptr may be null if payload_size is 0 (no-action fast paths).
void LyraEnqueueObservedDeferredAssertion(
    void* engine, uint32_t process_id, uint32_t instance_id, uint32_t site_id,
    uint8_t disposition, const void* payload_ptr, uint32_t payload_size,
    const lyra::DeferredAssertionRefBindingAbi* ref_ptr, uint32_t ref_count);

}  // extern "C"
