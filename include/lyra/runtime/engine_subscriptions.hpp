#pragma once

#include <cstddef>

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

// Per-signal edge accumulator for delta-level coalescing.
// Tracks all edge transitions within a single delta cycle.
struct EdgeRecord {
  bool initialized = false;    // First update sets prev_lsb
  bool prev_lsb = false;       // LSB after last update in this delta
  bool saw_posedge = false;    // Any 0->1 transition occurred
  bool saw_negedge = false;    // Any 1->0 transition occurred
  bool value_changed = false;  // Any full-value change occurred
};

}  // namespace lyra::runtime
