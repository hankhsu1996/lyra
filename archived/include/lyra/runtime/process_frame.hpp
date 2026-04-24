#pragma once

#include <cstddef>
#include <cstdint>

#include "lyra/runtime/simulation.hpp"
#include "lyra/runtime/suspend_record.hpp"

namespace lyra::runtime {

struct RuntimeInstance;

// Simulation-process body ABI. Invoked by the envelope on every activation.
//
// Parameters:
//   state             - per-process state pointer (ProcessState*); the first
//                       field is a ProcessFrameHeader whose SuspendRecord
//                       prefix is the suspend protocol state and whose
//                       ProcessOutcome slot is written before return.
//   resume_block      - resume target index
//   engine            - opaque Engine* for runtime helper calls
//   design            - design state base pointer
//   instance          - RuntimeInstance* execution anchor for this process
//   decision_owner_id - dense owner id for decision observation helpers
using SharedBodyFn = void (*)(
    void* state, uint32_t resume_block, void* engine, void* design,
    RuntimeInstance* instance, uint32_t decision_owner_id);

// Minimal process frame prefix. The backing object passed to every compiled
// body begins with this prefix; everything else in per-process runtime state
// lives on RuntimeProcess.
//
//   suspend - SuspendRecord at offset 0 (suspend protocol backing)
//   outcome - ProcessOutcome slot written by the body on every call
//
// Binary contract between codegen (BuildHeaderType in layout.cpp) and
// runtime (process_envelope.cpp). Both sides must agree on field order
// and size. Hard assertions below enforce this.
struct ProcessFrameHeader {
  SuspendRecord suspend;
  ProcessOutcome outcome = {};
};

// Strongly typed field indices matching the LLVM struct type emitted by
// BuildHeaderType. Used by codegen GEP indices and runtime offsetof checks.
enum class ProcessFrameHeaderField : unsigned {
  kSuspend = 0,
  kOutcome = 1,
  kFieldCount = 2,
};

// Hard binary contract assertions. If the struct layout changes, these
// fail at compile time rather than manifesting as runtime SIGILL in
// AOT binaries.
static_assert(offsetof(ProcessFrameHeader, suspend) == 0);
static_assert(offsetof(ProcessFrameHeader, outcome) == sizeof(SuspendRecord));

}  // namespace lyra::runtime
