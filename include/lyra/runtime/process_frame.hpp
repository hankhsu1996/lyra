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
// All binding fields are populated before simulation begins:
//
//   design_ptr: written by codegen during process state initialization
//     (required because init processes run before the runtime exists).
//   engine_ptr, body, this_ptr, instance_id, signal_id_offset,
//   unstable_offsets: written by runtime init from descriptor data
//     (after init processes complete, before simulation dispatch).
//   outcome: live per-call state, written by shared body on every call.
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

// Field index constants matching the LLVM struct type emitted by
// BuildHeaderType. Used by codegen GEP indices and runtime offsetof
// checks. This is the single canonical source of field ordering.
struct ProcessFrameHeaderField {
  static constexpr unsigned kSuspend = 0;
  static constexpr unsigned kBody = 1;
  static constexpr unsigned kEnginePtr = 2;
  static constexpr unsigned kDesignPtr = 3;
  static constexpr unsigned kThisPtr = 4;
  static constexpr unsigned kUnstableOffsets = 5;
  static constexpr unsigned kInstanceId = 6;
  static constexpr unsigned kSignalIdOffset = 7;
  static constexpr unsigned kOutcome = 8;
  static constexpr unsigned kFieldCount = 9;
};

// Hard binary contract assertions.
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
