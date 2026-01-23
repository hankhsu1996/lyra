#pragma once

#include <array>
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

struct WaitTriggerRecord {
  uint32_t signal_id = 0;
  uint8_t edge = 0;  // runtime::EdgeKind
  std::array<uint8_t, 3> padding = {};
};

static constexpr uint32_t kMaxInlineTriggers = 8;

// C ABI layout for suspend record - used by both interpreter and LLVM.
// Process writes this before returning; runtime reads it after return.
//
// Memory layout:
//   - tag (1 byte) + padding (7 bytes) = 8
//   - delay_ticks (8 bytes) = 8
//   - resume_block (4 bytes) + num_triggers (4 bytes) = 8
//   - triggers[8] (8 bytes each) = 64
// Total: 88 bytes
struct SuspendRecord {
  SuspendTag tag = SuspendTag::kFinished;
  uint64_t delay_ticks = 0;   // For kDelay
  uint32_t resume_block = 0;  // For kDelay, kWait, kRepeat
  uint32_t num_triggers = 0;  // For kWait
  std::array<WaitTriggerRecord, kMaxInlineTriggers> triggers = {};
};

static_assert(sizeof(SuspendRecord) == 88, "SuspendRecord layout mismatch");
static_assert(alignof(SuspendRecord) == 8, "SuspendRecord alignment mismatch");

}  // namespace lyra::runtime
