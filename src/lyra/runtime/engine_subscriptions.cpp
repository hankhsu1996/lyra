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
  node->rebind_target = nullptr;
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
    for (auto* node = sw.rebind_head; node != nullptr;
         node = node->signal_next) {
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

      // Rebind nodes use rebind_head/rebind_tail; normal nodes use head/tail.
      bool is_rebind = (node->rebind_target != nullptr);
      auto*& list_head = is_rebind ? sw.rebind_head : sw.head;
      auto*& list_tail = is_rebind ? sw.rebind_tail : sw.tail;

      if (node->signal_prev != nullptr) {
        node->signal_prev->signal_next = node->signal_next;
      } else {
        list_head = node->signal_next;
      }
      if (node->signal_next != nullptr) {
        node->signal_next->signal_prev = node->signal_prev;
      } else {
        list_tail = node->signal_prev;
      }

      if (sw.head == nullptr && sw.rebind_head == nullptr) {
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

auto Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge) -> SubscriptionNode* {
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
  return Subscribe(handle, resume, signal, edge, 0, obs_size);
}

auto Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge, uint32_t byte_offset, uint32_t byte_size,
    uint8_t bit_index) -> SubscriptionNode* {
  if (finished_) {
    return nullptr;
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
    return nullptr;
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

  if (edge != common::EdgeKind::kAnyChange && byte_size != 1) {
    throw common::InternalError(
        "Engine::Subscribe", "edge subscriptions require byte_size=1");
  }
  if (edge != common::EdgeKind::kAnyChange && bit_index > 7) {
    throw common::InternalError(
        "Engine::Subscribe", "bit_index must be in [0,7]");
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
    node->bit_index = bit_index;
    node->last_bit =
        (design_state[meta.base_off + byte_offset] >> bit_index) & 1;
    // Save full byte for same-delta rebinding: if a rebind moves the
    // subscription to a different bit in the same byte, we need the old
    // byte to extract the correct old bit value for edge detection.
    node->snapshot_inline[0] = design_state[meta.base_off + byte_offset];
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
  return node;
}

void Engine::SubscribeRebind(
    ProcessHandle handle, ResumePoint resume, SignalId index_signal,
    SubscriptionNode* target_node, BitTargetMapping mapping,
    uint32_t index_byte_offset, uint32_t index_byte_size,
    uint8_t index_bit_width, bool index_is_signed) {
  if (finished_) return;

  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeRebind", "SubscribeRebind before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeRebind", "SubscribeRebind before InitSlotMeta");
  }

  auto& proc_state = process_states_[handle];
  if (!CheckSubscriptionLimits(proc_state)) return;

  const auto& meta = slot_meta_registry_.Get(index_signal);

  auto* node = AllocNode();
  *node = SubscriptionNode{};
  node->handle = handle;
  node->resume = resume;
  node->edge = common::EdgeKind::kAnyChange;
  node->signal = index_signal;

  // AnyChange on full index slot for rebind detection.
  node->byte_offset = 0;
  node->byte_size = meta.total_bytes;
  node->bit_index = 0;

  // Capture initial snapshot.
  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());
  const auto* src = &design_state[meta.base_off];
  if (meta.total_bytes <= SubscriptionNode::kInlineSnapshotCap) {
    std::memcpy(node->snapshot_inline.data(), src, meta.total_bytes);
  } else {
    node->snapshot_heap.resize(meta.total_bytes);
    std::memcpy(node->snapshot_heap.data(), src, meta.total_bytes);
  }

  // Set rebind fields.
  node->rebind_target = target_node;
  node->rebind_mapping = mapping;
  node->index_slot_id = index_signal;
  node->index_byte_offset = index_byte_offset;
  node->index_byte_size = index_byte_size;
  node->index_bit_width = index_bit_width;
  node->index_is_signed = index_is_signed;

  // Link into rebind waiter list (not normal waiter list).
  auto& sw = signal_waiters_[index_signal];
  node->signal_next = sw.rebind_head;
  node->signal_prev = nullptr;
  if (sw.rebind_head != nullptr) {
    sw.rebind_head->signal_prev = node;
  }
  sw.rebind_head = node;
  if (sw.rebind_tail == nullptr) {
    sw.rebind_tail = node;
  }

  // Link into process list.
  node->process_next = proc_state.subscription_head;
  proc_state.subscription_head = node;

  ++proc_state.subscription_count;
  ++live_subscription_count_;

  // Initial rebind: validate codegen-computed target against runtime state.
  // Catches X/Z indices at initial suspend time.
  RebindSubscription(node);
}

namespace {

// Read an integer value from design state at the given slot/offset/size.
// Masks to actual bit_width and sign-extends if is_signed.
auto ReadIndexValue(
    const void* design_state_base, const SlotMetaRegistry& registry,
    uint32_t slot_id, uint32_t byte_offset, uint32_t byte_size,
    uint32_t bit_width, bool is_signed) -> int64_t {
  if (byte_size == 0 || byte_size > 8) {
    throw common::InternalError(
        "ReadIndexValue",
        std::format("invariant violated: byte_size={}", byte_size));
  }
  const auto& meta = registry.Get(slot_id);
  const auto* base =
      static_cast<const uint8_t*>(design_state_base) + meta.base_off;
  uint64_t v = 0;
  std::memcpy(&v, base + byte_offset, byte_size);
  // Mask to actual bit width.
  if (bit_width > 0 && bit_width < 64) {
    v &= (uint64_t{1} << bit_width) - 1;
  }
  // Sign-extend from bit_width if signed, else already zero-extended.
  if (is_signed && bit_width > 0 && bit_width < 64) {
    uint64_t sign_bit = uint64_t{1} << (bit_width - 1);
    v = (v ^ sign_bit) - sign_bit;
  }
  return static_cast<int64_t>(v);
}

// Check if a 4-state index variable has X/Z bits in its unknown plane.
auto HasUnknownBits(
    const void* design_state_base, const SlotMetaRegistry& registry,
    uint32_t slot_id, uint32_t byte_offset, uint32_t byte_size) -> bool {
  const auto& meta = registry.Get(slot_id);
  if (meta.kind != SlotStorageKind::kPacked4) return false;
  const auto* unk = static_cast<const uint8_t*>(design_state_base) +
                    meta.base_off + meta.planes.unk_off + byte_offset;
  for (uint32_t i = 0; i < byte_size; ++i) {
    if (unk[i] != 0) return true;
  }
  return false;
}

}  // namespace

void Engine::RebindSubscription(SubscriptionNode* rebind_node) {
  auto* target = rebind_node->rebind_target;
  const auto& mapping = rebind_node->rebind_mapping;

  // Check for X/Z bits in the index variable. If present, deactivate the
  // edge subscription. When X/Z clears, the next rebind will re-activate.
  if (HasUnknownBits(
          design_state_base_, slot_meta_registry_, rebind_node->index_slot_id,
          rebind_node->index_byte_offset, rebind_node->index_byte_size)) {
    target->is_active = false;
    return;
  }

  int64_t index_val = ReadIndexValue(
      design_state_base_, slot_meta_registry_, rebind_node->index_slot_id,
      rebind_node->index_byte_offset, rebind_node->index_byte_size,
      rebind_node->index_bit_width, rebind_node->index_is_signed);

  int64_t logical_offset =
      (index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      static_cast<uint64_t>(logical_offset) >= mapping.total_bits) {
    target->is_active = false;
    return;
  }

  target->is_active = true;
  auto bit = static_cast<uint64_t>(logical_offset);
  uint32_t new_byte_offset = static_cast<uint32_t>(bit / 8);
  uint8_t new_bit_index = static_cast<uint8_t>(bit % 8);

  // Get old bit value at the new position for correct edge detection.
  // The edge sub's snapshot_inline[0] has the byte from subscribe time
  // or last flush. If the new position is in the same byte, extract the
  // old bit from the snapshot. Otherwise, use current design state.
  const auto& meta = slot_meta_registry_.Get(target->signal);
  const auto* base = static_cast<const uint8_t*>(design_state_base_);
  if (new_byte_offset == target->byte_offset) {
    target->last_bit = (target->snapshot_inline[0] >> new_bit_index) & 1;
  } else {
    target->last_bit =
        (base[meta.base_off + new_byte_offset] >> new_bit_index) & 1;
    target->snapshot_inline[0] = base[meta.base_off + new_byte_offset];
  }
  target->byte_offset = new_byte_offset;
  target->bit_index = new_bit_index;
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

  // Pass 1: Rebind phase -- update late-bound targets before edge checks.
  // When an index variable changes, we re-read it and recompute the edge
  // subscription's byte_offset/bit_index via the affine mapping.
  for (uint32_t slot_id : newly_dirty) {
    auto it = signal_waiters_.find(slot_id);
    if (it == signal_waiters_.end()) continue;

    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto* node = it->second.rebind_head; node != nullptr;) {
      auto* next = node->signal_next;
      const auto* current = &design_state[meta.base_off + node->byte_offset];
      auto* snapshot = node->SnapshotData();
      if (std::memcmp(current, snapshot, node->byte_size) != 0) {
        std::memcpy(snapshot, current, node->byte_size);
        RebindSubscription(node);
      }
      node = next;
    }
  }

  // Pass 2: Edge/wake phase -- existing logic, with is_active check.
  for (uint32_t slot_id : newly_dirty) {
    auto it = signal_waiters_.find(slot_id);
    if (it == signal_waiters_.end()) continue;

    auto dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto* node = it->second.head; node != nullptr;) {
      auto* next = node->signal_next;

      if (!node->is_active) {
        node = next;
        continue;
      }

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
        uint8_t current_byte = design_state[meta.base_off + node->byte_offset];
        uint8_t current_bit = (current_byte >> node->bit_index) & 1;
        if (node->last_bit != current_bit) {
          should_wake =
              EvaluateEdge(node->edge, node->last_bit != 0, current_bit != 0);
          node->last_bit = current_bit;
        }
        // Update byte snapshot for potential rebinding.
        node->snapshot_inline[0] = current_byte;
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
