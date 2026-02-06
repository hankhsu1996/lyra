#include "lyra/runtime/engine_subscriptions.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/edge_kind.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/update_set.hpp"

namespace lyra::runtime {

auto Engine::AllocNode() -> SubscriptionNode* {
  if (!free_list_.empty()) {
    auto* node = free_list_.back();
    free_list_.pop_back();
    return node;
  }
  node_pool_.emplace_back();
  return &node_pool_.back();
}

void Engine::FreeNode(SubscriptionNode* node) {
  node->snapshot_heap.clear();
  node->snapshot_heap.shrink_to_fit();
  free_list_.push_back(node);
}

auto Engine::EvaluateEdge(common::EdgeKind edge, bool old_lsb, bool new_lsb)
    -> bool {
  switch (edge) {
    case common::EdgeKind::kPosedge:
      return !old_lsb && new_lsb;
    case common::EdgeKind::kNegedge:
      return old_lsb && !new_lsb;
    case common::EdgeKind::kAnyChange:
      return true;
  }
  return false;  // Unreachable
}

auto Engine::CheckSubscriptionLimits(const ProcessState& proc_state) -> bool {
  if (max_total_subscriptions_ > 0 &&
      live_subscription_count_ >= max_total_subscriptions_) {
    TerminateWithResourceError(
        "global subscription limit exceeded", live_subscription_count_,
        max_total_subscriptions_);
    return false;
  }

  if (max_subscriptions_per_process_ > 0 &&
      proc_state.subscription_count >= max_subscriptions_per_process_) {
    TerminateWithResourceError(
        "per-process subscription limit exceeded",
        proc_state.subscription_count, max_subscriptions_per_process_);
    return false;
  }

  return true;
}

void Engine::TerminateWithResourceError(
    std::string_view reason, size_t current, size_t limit) {
  termination_reason_ = std::format("{} ({}/{})", reason, current, limit);
  finished_ = true;

  fmt::println(
      stderr,
      "lyra: simulation terminated: {}\n"
      "  time: {}",
      *termination_reason_, current_time_);

  PrintTopWaiters(5);
}

void Engine::PrintTopWaiters(size_t count) {
  std::vector<std::pair<SignalId, size_t>> waiter_counts;
  waiter_counts.reserve(signal_waiters_.size());

  for (const auto& [signal, sw] : signal_waiters_) {
    size_t n = 0;
    for (auto* node = sw.head; node != nullptr; node = node->signal_next) {
      ++n;
    }
    waiter_counts.emplace_back(signal, n);
  }

  auto middle = waiter_counts.begin() + static_cast<std::ptrdiff_t>(std::min(
                                            count, waiter_counts.size()));
  std::ranges::partial_sort(
      waiter_counts, middle,
      [](const auto& a, const auto& b) { return a.second > b.second; });

  fmt::println(stderr, "  top {} signals by waiter count:", count);
  for (size_t i = 0; i < std::min(count, waiter_counts.size()); ++i) {
    fmt::println(
        stderr, "    signal {}: {} waiters", waiter_counts[i].first,
        waiter_counts[i].second);
  }
}

void Engine::ClearProcessSubscriptions(ProcessHandle handle) {
  auto proc_it = process_states_.find(handle);
  if (proc_it == process_states_.end()) {
    return;
  }

  SubscriptionNode* node = proc_it->second.subscription_head;
  while (node != nullptr) {
    SubscriptionNode* next = node->process_next;

    auto sig_it = signal_waiters_.find(node->signal);
    if (sig_it != signal_waiters_.end()) {
      auto& sw = sig_it->second;

      if (node->signal_prev != nullptr) {
        node->signal_prev->signal_next = node->signal_next;
      } else {
        sw.head = node->signal_next;
      }
      if (node->signal_next != nullptr) {
        node->signal_next->signal_prev = node->signal_prev;
      } else {
        sw.tail = node->signal_prev;
      }

      if (sw.head == nullptr) {
        signal_waiters_.erase(sig_it);
      }
    }

    FreeNode(node);
    if (live_subscription_count_ == 0) {
      throw common::InternalError(
          "Engine::ClearProcessSubscriptions",
          "live_subscription_count_ underflow");
    }
    --live_subscription_count_;
    node = next;
  }

  proc_it->second.subscription_head = nullptr;
  proc_it->second.subscription_count = 0;
}

void Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge) {
  // Lifecycle invariant: Subscribe() only called during Run(), after
  // SetDesignStateBase() and InitSlotMeta().
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before InitSlotMeta");
  }

  const auto& meta = slot_meta_registry_.Get(signal);
  uint32_t obs_size =
      (edge == common::EdgeKind::kAnyChange) ? meta.total_bytes : 1;
  Subscribe(handle, resume, signal, edge, 0, obs_size);
}

void Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size) {
  if (finished_) {
    return;
  }

  // Lifecycle invariant: Subscribe() only called during Run(), after
  // SetDesignStateBase() and InitSlotMeta().
  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::Subscribe", "Subscribe before InitSlotMeta");
  }

  auto& proc_state = process_states_[handle];

  if (!CheckSubscriptionLimits(proc_state)) {
    return;
  }

  const auto& meta = slot_meta_registry_.Get(signal);

  // Range validation.
  if (byte_size == 0) {
    throw common::InternalError("Engine::Subscribe", "byte_size must be > 0");
  }
  if (static_cast<uint64_t>(byte_offset) + static_cast<uint64_t>(byte_size) >
      meta.total_bytes) {
    throw common::InternalError(
        "Engine::Subscribe", "observation range exceeds slot size");
  }

  // Sub-slot edge observation not yet supported.
  if (edge != common::EdgeKind::kAnyChange &&
      (byte_offset != 0 || byte_size != 1)) {
    throw common::InternalError(
        "Engine::Subscribe",
        "edge subscriptions require byte_offset=0, byte_size=1");
  }

  auto* node = AllocNode();
  *node = SubscriptionNode{};
  node->handle = handle;
  node->resume = resume;
  node->edge = edge;
  node->signal = signal;

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  node->byte_offset = byte_offset;
  if (edge == common::EdgeKind::kAnyChange) {
    node->byte_size = byte_size;
    node->bit_index = 0;
    const auto* src = &design_state[meta.base_off + byte_offset];
    if (byte_size <= SubscriptionNode::kInlineSnapshotCap) {
      std::memcpy(node->snapshot_inline.data(), src, byte_size);
    } else {
      node->snapshot_heap.resize(byte_size);
      std::memcpy(node->snapshot_heap.data(), src, byte_size);
    }
  } else {
    node->byte_size = byte_size;
    node->bit_index = 0;
    node->last_bit =
        (design_state[meta.base_off + byte_offset] >> node->bit_index) & 1;
  }

  auto& sw = signal_waiters_[signal];
  node->signal_next = sw.head;
  node->signal_prev = nullptr;
  if (sw.head != nullptr) {
    sw.head->signal_prev = node;
  }
  sw.head = node;
  if (sw.tail == nullptr) {
    sw.tail = node;
  }

  node->process_next = proc_state.subscription_head;
  proc_state.subscription_head = node;

  ++proc_state.subscription_count;
  ++live_subscription_count_;
}

void Engine::FlushSignalUpdates() {
  if (finished_) {
    update_set_.ClearDelta();
    return;
  }

  // Guard: MIR path has no slot meta / design state.
  if (!slot_meta_registry_.IsPopulated() || design_state_base_ == nullptr) {
    update_set_.ClearDelta();
    return;
  }

  auto newly_dirty = update_set_.DeltaDirtySlots();
  if (newly_dirty.empty()) {
    update_set_.ClearDelta();
    return;
  }

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  for (uint32_t slot_id : newly_dirty) {
    auto it = signal_waiters_.find(slot_id);
    if (it == signal_waiters_.end()) continue;

    auto dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto* node = it->second.head; node != nullptr;) {
      auto* next = node->signal_next;

      // Range intersection filter: skip if no dirty range overlaps observation.
      if (!RangesOverlap(dirty_ranges, node->byte_offset, node->byte_size)) {
        node = next;
        continue;
      }

      bool should_wake = false;
      if (node->edge == common::EdgeKind::kAnyChange) {
        // Byte-range comparison: compare observed region against snapshot.
        const auto* current = &design_state[meta.base_off + node->byte_offset];
        auto* snapshot = node->SnapshotData();
        if (std::memcmp(current, snapshot, node->byte_size) != 0) {
          should_wake = true;
          std::memcpy(snapshot, current, node->byte_size);
        }
      } else {
        // Per-bit comparison for kPosedge/kNegedge.
        uint8_t current_bit =
            (design_state[meta.base_off + node->byte_offset] >>
             node->bit_index) &
            1;
        if (node->last_bit != current_bit) {
          should_wake =
              EvaluateEdge(node->edge, node->last_bit != 0, current_bit != 0);
          node->last_bit = current_bit;
        }
      }

      if (should_wake) {
        auto& proc_state = process_states_[node->handle];
        if (!proc_state.is_enqueued) {
          next_delta_queue_.push_back(
              ScheduledEvent{.handle = node->handle, .resume = node->resume});
          proc_state.is_enqueued = true;
        }
      }

      node = next;
    }
  }

  update_set_.ClearDelta();
}

}  // namespace lyra::runtime
