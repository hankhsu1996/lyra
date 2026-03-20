#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/process_descriptor.hpp"
#include "lyra/runtime/simulation.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::runtime {

// Canonical process frame header layout.
//
// Binary contract between codegen (BuildHeaderType in layout.cpp) and
// runtime (simulation.cpp, engine_scheduler_fixpoint.cpp). Both sides
// must agree on field order and size. Hard assertions below enforce this.
//
// The header is the runtime-owned binding anchor for a realized process.
//
// Ownership domains:
//   Init-process headers are codegen-private ephemeral stack objects.
//   Codegen allocates, populates (including design_ptr), and passes them
//   to LyraRunProcessSync. They are not part of persistent runtime state.
//
//   Simulation-process headers are runtime-owned persistent state.
//   Runtime is the sole initializer of all binding fields:
//     design_ptr: cached binding derived from ABI design_state (source of
//       truth). Written by runtime before simulation dispatch.
//     engine_ptr: written by runtime before simulation dispatch.
//     body, this_ptr, instance_id, signal_id_offset, unstable_offsets:
//       written by runtime from descriptor data.
//     outcome: live per-call state, written by shared body on every call.
//
// After init, no field is written by codegen. The descriptor table is
// consumed only at init time to populate these fields. All dispatch
// paths (process and comb) read from the header, not from descriptors.
//
// For standalone (connection/init) processes, module-process binding
// fields (body, this_ptr, instance_id, signal_id_offset,
// unstable_offsets) are null/zero. Standalone processes use the 3-arg
// LyraProcessFunc path and do not dispatch through shared bodies.
struct ProcessFrameHeader {
  SuspendRecord suspend;
  SharedBodyFn body = nullptr;
  void* engine_ptr = nullptr;
  void* design_ptr = nullptr;
  void* this_ptr = nullptr;
  const uint64_t* unstable_offsets = nullptr;
  uint32_t instance_id = 0;
  uint32_t signal_id_offset = 0;
  ProcessOutcome outcome = {};
};

// Strongly typed field indices matching the LLVM struct type emitted by
// BuildHeaderType. Used by codegen GEP indices and runtime offsetof
// checks. This is the single canonical source of field ordering.
enum class ProcessFrameHeaderField : unsigned {
  kSuspend = 0,
  kBody = 1,
  kEnginePtr = 2,
  kDesignPtr = 3,
  kThisPtr = 4,
  kUnstableOffsets = 5,
  kInstanceId = 6,
  kSignalIdOffset = 7,
  kOutcome = 8,
  kFieldCount = 9,
};

// Hard binary contract assertions. If the struct layout changes, these
// fail at compile time rather than manifesting as runtime SIGILL in
// AOT binaries.
static_assert(offsetof(ProcessFrameHeader, body) == sizeof(SuspendRecord));
static_assert(
    offsetof(ProcessFrameHeader, engine_ptr) ==
    offsetof(ProcessFrameHeader, body) + sizeof(SharedBodyFn));
static_assert(
    offsetof(ProcessFrameHeader, design_ptr) ==
    offsetof(ProcessFrameHeader, engine_ptr) + sizeof(void*));
static_assert(
    offsetof(ProcessFrameHeader, this_ptr) ==
    offsetof(ProcessFrameHeader, design_ptr) + sizeof(void*));
static_assert(
    offsetof(ProcessFrameHeader, unstable_offsets) ==
    offsetof(ProcessFrameHeader, this_ptr) + sizeof(void*));
static_assert(
    offsetof(ProcessFrameHeader, instance_id) ==
    offsetof(ProcessFrameHeader, unstable_offsets) + sizeof(const uint64_t*));
static_assert(
    offsetof(ProcessFrameHeader, signal_id_offset) ==
    offsetof(ProcessFrameHeader, instance_id) + sizeof(uint32_t));
static_assert(
    offsetof(ProcessFrameHeader, outcome) ==
    offsetof(ProcessFrameHeader, signal_id_offset) + sizeof(uint32_t));

}  // namespace lyra::runtime
