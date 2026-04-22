#pragma once

#include <cstdint>
#include <vector>

namespace lyra::runtime {

static constexpr uint32_t kNoTriggerSlot = UINT32_MAX;

// Why an activation exists. Stored per-process in WakeTraceInfo when
// activation tracing is enabled. Used by activation trace for observability.
enum class WakeCause : uint8_t {
  kEdge,
  kChange,
  kContainer,
  kDelay,
  kDelayZero,
  kInitial,
  kRepeat,
  kEvent,
};

constexpr auto HasTriggerSlot(WakeCause c) -> bool {
  switch (c) {
    case WakeCause::kEdge:
    case WakeCause::kChange:
    case WakeCause::kContainer:
      return true;
    case WakeCause::kDelay:
    case WakeCause::kDelayZero:
    case WakeCause::kInitial:
    case WakeCause::kRepeat:
    case WakeCause::kEvent:
      return false;
  }
  return false;
}

enum class ActivationEventKind : uint8_t { kWake, kRun };

// Structured activation event for the ring buffer.
//
// Both kWake and kRun events carry cause and trigger_slot. For kRun events
// these retain the causal wake metadata of the activation that led to the
// run, which is useful for correlating productivity with wake reason.
//
// slots_dirtied counts distinct design slots dirtied by the activation
// through MarkSlotDirty / MarkDirtyRange only. Excludes external/container
// heap-relative dirty notifications. Deduplicated at slot granularity
// within one activation via generation counters.
struct ActivationEvent {
  uint64_t time;
  uint32_t delta;
  uint32_t process_id;
  uint32_t trigger_slot;
  uint32_t resume_block;
  ActivationEventKind kind;
  WakeCause cause;
  uint32_t slots_dirtied;
};

// Fixed-capacity ring buffer of activation events.
// Append is O(1) with no allocation after construction.
class ActivationTrace {
 public:
  static constexpr uint32_t kDefaultCapacity = 8192;

  explicit ActivationTrace(uint32_t capacity = kDefaultCapacity);

  void Append(const ActivationEvent& event);

  // Number of events currently stored (up to capacity).
  [[nodiscard]] auto Size() const -> uint32_t;

  [[nodiscard]] auto Capacity() const -> uint32_t;

  // Index access: 0 = oldest retained event, Size()-1 = newest.
  [[nodiscard]] auto operator[](uint32_t index) const -> const ActivationEvent&;

 private:
  std::vector<ActivationEvent> buffer_;
  uint32_t capacity_;
  uint32_t head_ = 0;
  uint32_t count_ = 0;
};

}  // namespace lyra::runtime
