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
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"
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
  node->container_base_off = UINT32_MAX;
  node->container_elem_stride = 0;
  node->container_sv_index = 0;
  node->container_epoch = 0;
  node->rebind_target = nullptr;
  node->plan_ref = {};
  node->rebind_mapping = {};
  node->last_rebind_epoch = 0;
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
  proc_it->second.plan_pool.ops.clear();
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

auto Engine::SubscribeContainerElement(
    ProcessHandle handle, ResumePoint resume, SignalId signal,
    common::EdgeKind edge, int64_t sv_index, uint32_t elem_stride)
    -> SubscriptionNode* {
  if (finished_) return nullptr;

  if (design_state_base_ == nullptr) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement",
        "SubscribeContainerElement before SetDesignStateBase");
  }
  if (!slot_meta_registry_.IsPopulated()) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement",
        "SubscribeContainerElement before InitSlotMeta");
  }

  auto& proc_state = process_states_[handle];
  if (!CheckSubscriptionLimits(proc_state)) return nullptr;

  const auto& meta = slot_meta_registry_.Get(signal);

  auto* node = AllocNode();
  *node = SubscriptionNode{};
  node->handle = handle;
  node->resume = resume;
  node->edge = edge;
  node->signal = signal;
  node->byte_size = 1;
  node->bit_index = 0;
  node->container_base_off = meta.base_off;
  node->container_elem_stride = elem_stride;
  node->container_sv_index = sv_index;

  // Chase handle from DesignState.
  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base_),
      meta.base_off + sizeof(void*));
  void* handle_ptr = nullptr;
  std::memcpy(&handle_ptr, &ds[meta.base_off], sizeof(void*));

  if (handle_ptr == nullptr) {
    node->is_active = false;
    node->byte_offset = 0;
    node->container_epoch = 0;
  } else {
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "Engine::SubscribeContainerElement", "invalid container magic");
    }
    node->container_epoch = arr->epoch;

    if (arr->data == nullptr || sv_index < 0 || sv_index >= arr->size) {
      node->is_active = false;
      node->byte_offset = 0;
    } else {
      node->is_active = true;
      auto byte_off = static_cast<uint32_t>(sv_index) * elem_stride;
      node->byte_offset = byte_off;
      auto elem = std::span(
          static_cast<const uint8_t*>(arr->data), byte_off + elem_stride);
      node->last_bit = elem[byte_off] & 1;
      node->snapshot_inline[0] = elem[byte_off];
    }
  }

  // Link into signal's waiter list.
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
    ProcessHandle handle, ResumePoint resume, SubscriptionNode* target_node,
    std::span<const IndexPlanOp> plan, BitTargetMapping mapping,
    std::span<const uint32_t> dep_slots) {
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

  // Store plan in process's pool and set target_node's plan_ref.
  auto plan_start = static_cast<uint32_t>(proc_state.plan_pool.ops.size());
  proc_state.plan_pool.ops.insert(
      proc_state.plan_pool.ops.end(), plan.begin(), plan.end());
  target_node->plan_ref = {
      .start = plan_start, .count = static_cast<uint16_t>(plan.size())};
  target_node->rebind_mapping = mapping;

  // Create a rebind node for each dep slot.
  for (uint32_t dep_slot : dep_slots) {
    if (!CheckSubscriptionLimits(proc_state)) return;

    const auto& meta = slot_meta_registry_.Get(dep_slot);

    auto* node = AllocNode();
    *node = SubscriptionNode{};
    node->handle = handle;
    node->resume = resume;
    node->edge = common::EdgeKind::kAnyChange;
    node->signal = dep_slot;

    // AnyChange on full dep slot for rebind detection.
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

    // Link into rebind waiter list (not normal waiter list).
    auto& sw = signal_waiters_[dep_slot];
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
  }

  // Initial rebind: validate codegen-computed target against runtime state.
  // Catches X/Z indices at initial suspend time.
  RebindSubscription(target_node);
}

void Engine::RebindSubscription(SubscriptionNode* rebind_node) {
  // Epoch guard: skip if already rebound this flush cycle.
  if (rebind_node->last_rebind_epoch == flush_epoch_) return;
  rebind_node->last_rebind_epoch = flush_epoch_;

  const auto& mapping = rebind_node->rebind_mapping;

  // Resolve plan from the process's pool.
  auto& proc_state = process_states_[rebind_node->handle];
  auto all_ops = std::span<const IndexPlanOp>(proc_state.plan_pool.ops);
  auto plan_span =
      all_ops.subspan(rebind_node->plan_ref.start, rebind_node->plan_ref.count);

  bool should_deactivate = false;
  int64_t index_val = EvaluateIndexPlan(
      design_state_base_, slot_meta_registry_, plan_span, &should_deactivate);

  if (should_deactivate) {
    rebind_node->is_active = false;
    return;
  }

  // Container path: chase handle from DesignState, read from heap.
  if (rebind_node->container_base_off != UINT32_MAX) {
    auto ds = std::span(
        static_cast<const uint8_t*>(design_state_base_),
        rebind_node->container_base_off + sizeof(void*));
    void* handle_ptr = nullptr;
    std::memcpy(
        &handle_ptr, &ds[rebind_node->container_base_off], sizeof(void*));

    if (handle_ptr == nullptr) {
      rebind_node->is_active = false;
      return;
    }
    const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
    if (arr->magic != DynArrayData::kMagic) {
      throw common::InternalError(
          "RebindSubscription", "invalid container magic");
    }
    rebind_node->container_sv_index = index_val;
    rebind_node->container_epoch = arr->epoch;
    if (arr->data == nullptr || index_val < 0 || index_val >= arr->size) {
      rebind_node->is_active = false;
      return;
    }
    rebind_node->is_active = true;
    auto new_byte_offset =
        static_cast<uint32_t>(index_val) * rebind_node->container_elem_stride;
    rebind_node->byte_offset = new_byte_offset;
    rebind_node->bit_index = 0;
    // Always read from heap (content may have changed even if offset same).
    auto heap = std::span(
        static_cast<const uint8_t*>(arr->data),
        new_byte_offset + rebind_node->container_elem_stride);
    rebind_node->last_bit = heap[new_byte_offset] & 1;
    rebind_node->snapshot_inline[0] = heap[new_byte_offset];
    return;
  }

  // Packed/unpacked DesignState path.
  int64_t logical_offset =
      static_cast<int64_t>(index_val - mapping.index_base) * mapping.index_step;

  if (logical_offset < 0 ||
      static_cast<uint64_t>(logical_offset) >= mapping.total_bits) {
    rebind_node->is_active = false;
    return;
  }

  rebind_node->is_active = true;
  auto bit = static_cast<uint64_t>(logical_offset);
  auto new_byte_offset = static_cast<uint32_t>(bit / 8);
  auto new_bit_index = static_cast<uint8_t>(bit % 8);

  // Get old bit value at the new position for correct edge detection.
  const auto& meta = slot_meta_registry_.Get(rebind_node->signal);
  auto ds = std::span(
      static_cast<const uint8_t*>(design_state_base_),
      meta.base_off + new_byte_offset + 1);
  if (new_byte_offset == rebind_node->byte_offset) {
    rebind_node->last_bit =
        (rebind_node->snapshot_inline[0] >> new_bit_index) & 1;
  } else {
    rebind_node->last_bit =
        (ds[meta.base_off + new_byte_offset] >> new_bit_index) & 1;
    rebind_node->snapshot_inline[0] = ds[meta.base_off + new_byte_offset];
  }
  rebind_node->byte_offset = new_byte_offset;
  rebind_node->bit_index = new_bit_index;
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

  // Increment flush epoch for rebind deduplication.
  ++flush_epoch_;

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  // Pass 1: Rebind phase -- update late-bound targets before edge checks.
  // When a dep slot changes, we re-evaluate the index plan and recompute the
  // edge subscription's byte_offset/bit_index via the affine mapping.
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
        RebindSubscription(node->rebind_target);
      }
      node = next;
    }
  }

  // Pass 2: Edge/wake phase -- existing logic, with is_active check.
  for (uint32_t slot_id : newly_dirty) {
    auto it = signal_waiters_.find(slot_id);
    if (it == signal_waiters_.end()) continue;

    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    const auto& external_ranges = update_set_.DeltaExternalRangesFor(slot_id);
    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto* node = it->second.head; node != nullptr;) {
      auto* next = node->signal_next;

      // Container element subscription path.
      if (node->container_base_off != UINT32_MAX) {
        if (!node->is_active) {
          // Inactive container sub: try OOB reactivation.
          // Skip if rebind set inactive this cycle (X/Z).
          if (node->last_rebind_epoch == flush_epoch_) {
            node = next;
            continue;
          }
          // Range overlap filter for inactive subs.
          // When external ranges are present, use precise filtering.
          // When absent (element write via LyraStorePacked), fall through
          // since the slot is dirty and heap content may have changed.
          if (!external_ranges.IsEmpty()) {
            auto candidate_off = static_cast<uint32_t>(std::max(
                                     int64_t{0}, node->container_sv_index)) *
                                 node->container_elem_stride;
            if (!external_ranges.Overlaps(candidate_off, 1)) {
              node = next;
              continue;
            }
          }
          // Chase handle, check if now in-range.
          void* handle_ptr = nullptr;
          std::memcpy(
              &handle_ptr, &design_state[node->container_base_off],
              sizeof(void*));
          if (handle_ptr == nullptr) {
            node = next;
            continue;
          }
          const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
          if (arr->magic != DynArrayData::kMagic) {
            throw common::InternalError(
                "FlushSignalUpdates", "invalid container magic");
          }
          int64_t idx = node->container_sv_index;
          if (arr->data == nullptr || idx < 0 || idx >= arr->size) {
            node = next;
            continue;
          }
          // Reactivate: capture snapshot, no edge trigger.
          node->is_active = true;
          node->container_epoch = arr->epoch;
          auto byte_off =
              static_cast<uint32_t>(idx) * node->container_elem_stride;
          node->byte_offset = byte_off;
          auto heap_data = std::span(
              static_cast<const uint8_t*>(arr->data),
              byte_off + node->container_elem_stride);
          node->last_bit = heap_data[byte_off] & 1;
          node->snapshot_inline[0] = heap_data[byte_off];
          node = next;
          continue;
        }

        // Active container sub: range overlap filter.
        // When external ranges are present, use precise filtering.
        // When absent (element write via LyraStorePacked), fall through.
        if (!external_ranges.IsEmpty() &&
            !external_ranges.Overlaps(node->byte_offset, 1)) {
          node = next;
          continue;
        }
        // Chase handle, read from heap.
        void* handle_ptr = nullptr;
        std::memcpy(
            &handle_ptr, &design_state[node->container_base_off],
            sizeof(void*));
        if (handle_ptr == nullptr) {
          node->is_active = false;
          node = next;
          continue;
        }
        const auto* arr = static_cast<const DynArrayData*>(handle_ptr);
        if (arr->magic != DynArrayData::kMagic) {
          throw common::InternalError(
              "FlushSignalUpdates", "invalid container magic");
        }
        int64_t idx = node->container_sv_index;
        if (arr->data == nullptr || idx < 0 || idx >= arr->size) {
          node->is_active = false;
          node->container_epoch = arr->epoch;
          node = next;
          continue;
        }
        node->container_epoch = arr->epoch;
        auto heap_data = std::span(
            static_cast<const uint8_t*>(arr->data),
            node->byte_offset + node->container_elem_stride);
        uint8_t current_bit =
            (heap_data[node->byte_offset] >> node->bit_index) & 1;
        bool should_wake = false;
        if (node->last_bit != current_bit) {
          should_wake =
              EvaluateEdge(node->edge, node->last_bit != 0, current_bit != 0);
          node->last_bit = current_bit;
        }
        node->snapshot_inline[0] = heap_data[node->byte_offset];
        if (should_wake) {
          auto& proc_state = process_states_[node->handle];
          if (!proc_state.is_enqueued) {
            next_delta_queue_.push_back(
                ScheduledEvent{.handle = node->handle, .resume = node->resume});
            proc_state.is_enqueued = true;
          }
        }
        node = next;
        continue;
      }

      // Normal DesignState subscription path.
      if (!node->is_active) {
        node = next;
        continue;
      }

      // Range intersection filter: skip if no dirty range overlaps observation.
      if (!dirty_ranges.Overlaps(node->byte_offset, node->byte_size)) {
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
