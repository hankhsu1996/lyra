#pragma once

#include <cstdint>

namespace lyra::runtime {

// Suspend tag - matches SuspendReason variant index for consistency.
// Used in C ABI struct for both interpreter and LLVM backend communication.
enum class SuspendTag : uint8_t {
  kFinished = 0,
  kDelay = 1,
  kWait = 2,
  kRepeat = 3,
};

// C ABI layout for suspend record - used by both interpreter and LLVM.
// Process writes this before returning; runtime reads it after return.
//
// Memory layout (no explicit padding needed since fields are naturally aligned
// for this arrangement, but we verify the size for safety):
//   - tag (1 byte) + padding (7 bytes)
//   - delay_ticks (8 bytes)
//   - resume_block (4 bytes) + padding (4 bytes)
// Total: 24 bytes
struct SuspendRecord {
  SuspendTag tag = SuspendTag::kFinished;
  uint64_t delay_ticks = 0;   // For kDelay
  uint32_t resume_block = 0;  // For kDelay, kWait, kRepeat
};

static_assert(sizeof(SuspendRecord) == 24, "SuspendRecord layout mismatch");
static_assert(alignof(SuspendRecord) == 8, "SuspendRecord alignment mismatch");

}  // namespace lyra::runtime
