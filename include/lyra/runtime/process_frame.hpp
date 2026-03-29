#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/simulation.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

// 2-arg shared body function signature (frame, resume).
// Instance binding lives in the process frame header, not in the call
// surface. This is the long-term call contract.
using SharedBodyFn = void (*)(void*, uint32_t);

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
//     body: written by runtime from constructor data.
//     instance: RuntimeInstance* execution anchor, written by runtime from
//       constructor data. Null for connection/init processes.
//     outcome: live per-call state, written by shared body on every call.
//
// After init, no field is written by codegen. The descriptor table is
// consumed only at init time to populate these fields. All dispatch
// paths (process and comb) read from the header, not from descriptors.
//
// For standalone (connection/init) processes, the instance field is null.
// Standalone processes use the 3-arg LyraProcessFunc path and do not
// dispatch through shared bodies.
struct ProcessFrameHeader {
  SuspendRecord suspend;
  SharedBodyFn body = nullptr;
  void* engine_ptr = nullptr;
  void* design_ptr = nullptr;
  RuntimeInstance* instance = nullptr;
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
  kInstance = 4,
  kOutcome = 5,
  kFieldCount = 6,
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
    offsetof(ProcessFrameHeader, instance) ==
    offsetof(ProcessFrameHeader, design_ptr) + sizeof(void*));
static_assert(
    offsetof(ProcessFrameHeader, outcome) ==
    offsetof(ProcessFrameHeader, instance) + sizeof(RuntimeInstance*));

}  // namespace lyra::runtime
