#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/common/edge_kind.hpp"
#include "lyra/runtime/engine_types.hpp"

namespace lyra::runtime {

// Subscription node - doubly linked in signal's list, singly linked in
// process's list. Each subscription is linked into two lists for O(1) removal.
struct SubscriptionNode {
  ProcessHandle handle;
  ResumePoint resume;
  common::EdgeKind edge = common::EdgeKind::kAnyChange;
  SignalId signal = 0;  // For removal from signal's list

  // Observation region within the slot (relative to SlotMeta::base_off).
  // byte_offset: start of observed range within the slot.
  // byte_size:   number of bytes in the observed range (always > 0).
  //   kAnyChange:         SlotMeta::total_bytes (full-slot byte comparison).
  //   kPosedge/kNegedge:  1 (one byte containing the observed bit).
  uint32_t byte_offset = 0;
  uint32_t byte_size = 0;
  uint8_t bit_index = 0;

  // Per-subscription snapshot.
  // kPosedge/kNegedge: single bit stored in last_bit.
  // kAnyChange:        byte-range snapshot via SnapshotData().
  uint8_t last_bit = 0;

  // Small-buffer-optimized snapshot for kAnyChange byte-range comparison.
  // Inline for observed regions <= kInlineSnapshotCap bytes (covers most
  // scalars). Heap-allocated for larger regions (freed by Engine::FreeNode).
  static constexpr uint32_t kInlineSnapshotCap = 16;
  std::array<uint8_t, kInlineSnapshotCap> snapshot_inline{};
  std::vector<uint8_t> snapshot_heap;

  [[nodiscard]] auto SnapshotData() -> uint8_t* {
    return byte_size <= kInlineSnapshotCap ? snapshot_inline.data()
                                           : snapshot_heap.data();
  }
  [[nodiscard]] auto SnapshotData() const -> const uint8_t* {
    return byte_size <= kInlineSnapshotCap ? snapshot_inline.data()
                                           : snapshot_heap.data();
  }

  // Links for signal's waiter list (doubly linked for O(1) removal)
  SubscriptionNode* signal_prev = nullptr;
  SubscriptionNode* signal_next = nullptr;

  // Link for process's subscription list (singly linked, only traverse on
  // clear)
  SubscriptionNode* process_next = nullptr;
};

// Signal's waiter list head/tail pointers.
struct SignalWaiters {
  SubscriptionNode* head = nullptr;
  SubscriptionNode* tail = nullptr;
};

// Per-process state (keyed by ProcessHandle).
struct ProcessState {
  bool is_enqueued = false;  // De-dup flag for next-delta queue
  size_t subscription_count = 0;
  SubscriptionNode* subscription_head = nullptr;
};

}  // namespace lyra::runtime
