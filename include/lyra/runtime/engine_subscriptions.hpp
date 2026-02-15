#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"

namespace lyra::runtime {

// Reference into a process's IndexPlanPool.
struct IndexPlanRef {
  uint32_t start = 0;
  uint16_t count = 0;
};

// Per-process plan storage. Stable spans -- not invalidated until
// ClearProcessSubscriptions.
struct IndexPlanPool {
  std::vector<IndexPlanOp> ops;
};

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

  // Edge subscription activation state.
  // When false, FlushSignalUpdates skips edge evaluation entirely.
  // Set to false when rebind detects OOB or X index.
  bool is_active = true;

  // Late-bound rebinding fields.
  // For rebind nodes: rebind_target points to the edge subscription.
  // For edge target nodes: plan_ref + rebind_mapping define the expression.
  SubscriptionNode* rebind_target = nullptr;
  IndexPlanRef plan_ref = {};
  BitTargetMapping rebind_mapping = {};

  // Epoch guard: prevents redundant re-evaluations when multiple dep slots
  // change in the same delta.
  uint32_t last_rebind_epoch = 0;

  // Links for signal's waiter list (doubly linked for O(1) removal).
  // Normal subscriptions use head/tail; rebind subscriptions use
  // rebind_head/rebind_tail. Distinguish by checking rebind_target != nullptr.
  SubscriptionNode* signal_prev = nullptr;
  SubscriptionNode* signal_next = nullptr;

  // Link for process's subscription list (singly linked, only traverse on
  // clear)
  SubscriptionNode* process_next = nullptr;
};

// Signal's waiter list head/tail pointers.
// Normal subscriptions are linked via head/tail.
// Rebind subscriptions are linked via rebind_head/rebind_tail (separate list).
struct SignalWaiters {
  SubscriptionNode* head = nullptr;
  SubscriptionNode* tail = nullptr;
  SubscriptionNode* rebind_head = nullptr;
  SubscriptionNode* rebind_tail = nullptr;
};

// Per-process state (keyed by ProcessHandle).
struct ProcessState {
  bool is_enqueued = false;  // De-dup flag for next-delta queue
  size_t subscription_count = 0;
  SubscriptionNode* subscription_head = nullptr;
  IndexPlanPool plan_pool;
};

}  // namespace lyra::runtime
