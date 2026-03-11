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

#include "lyra/common/bit_target_mapping.hpp"
#include "lyra/common/edge_kind.hpp"
#include "lyra/common/index_plan.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/dyn_array_data.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_scheduler.hpp"
#include "lyra/runtime/engine_types.hpp"
#include "lyra/runtime/index_plan.hpp"
#include "lyra/runtime/slot_meta.hpp"
#include "lyra/runtime/suspend_record.hpp"
#include "lyra/runtime/update_set.hpp"
#include "lyra/runtime/wait_site.hpp"

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

  for (uint32_t signal = 0; signal < signal_waiters_.size(); ++signal) {
    const auto& sw = signal_waiters_[signal];
    if (sw.head == nullptr && sw.rebind_head == nullptr) continue;
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

void Engine::ClearInstalledSubscriptions(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) {
    return;
  }
  auto& proc_state = process_states_[handle.process_id];

  SubscriptionNode* node = proc_state.subscription_head;
  while (node != nullptr) {
    SubscriptionNode* next = node->process_next;

    if (node->signal >= signal_waiters_.size()) {
      throw common::InternalError(
          "Engine::ClearInstalledSubscriptions",
          std::format(
              "stale subscription signal {} exceeds slot count {}",
              node->signal, signal_waiters_.size()));
    }
    auto& sw = signal_waiters_[node->signal];

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

    FreeNode(node);
    if (live_subscription_count_ == 0) {
      throw common::InternalError(
          "Engine::ClearInstalledSubscriptions",
          "live_subscription_count_ underflow");
    }
    --live_subscription_count_;
    node = next;
  }

  proc_state.subscription_head = nullptr;
  proc_state.subscription_count = 0;
  proc_state.plan_pool.ops.clear();
}

void Engine::InvalidateInstalledWait(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) return;
  auto& proc_state = process_states_[handle.process_id];
  proc_state.installed_wait = InstalledWaitState{};
}

void Engine::ResetInstalledWait(ProcessHandle handle) {
  ClearInstalledSubscriptions(handle);
  InvalidateInstalledWait(handle);
}

void Engine::ClearProcessSubscriptions(ProcessHandle handle) {
  ResetInstalledWait(handle);
}

void Engine::RefreshInstalledSnapshots(ProcessHandle handle) {
  if (handle.process_id >= num_processes_) return;

  auto& proc_state = process_states_[handle.process_id];

  std::span design_state(
      static_cast<const uint8_t*>(design_state_base_),
      slot_meta_registry_.MaxExtent());

  for (auto* node = proc_state.subscription_head; node != nullptr;
       node = node->process_next) {
    // Static refresh invariant: all installed nodes must be refreshable
    // in place. Rebind nodes and container element nodes violate this
    // contract -- their presence under a kStatic site is a compiler bug.
    if (node->rebind_target != nullptr) {
      throw common::InternalError(
          "Engine::RefreshInstalledSnapshots",
          std::format(
              "process {} has rebind node on static refresh path",
              handle.process_id));
    }
    if (node->container_base_off != UINT32_MAX) {
      throw common::InternalError(
          "Engine::RefreshInstalledSnapshots",
          std::format(
              "process {} has container element node on static refresh path",
              handle.process_id));
    }

    const auto& meta = slot_meta_registry_.Get(node->signal);

    if (node->edge == common::EdgeKind::kAnyChange) {
      const auto* current = &design_state[meta.base_off + node->byte_offset];
      auto* snapshot = node->SnapshotData();
      std::memcpy(snapshot, current, node->byte_size);
    } else {
      uint8_t current_byte = design_state[meta.base_off + node->byte_offset];
      node->last_bit = (current_byte >> node->bit_index) & 1;
      node->snapshot_inline[0] = current_byte;
    }
  }
}

void Engine::InstallWaitSite(
    ProcessHandle handle, SuspendRecord* suspend,
    const CompiledWaitSite& descriptor) {
  auto resume =
      ResumePoint{.block_index = suspend->resume_block, .instruction_index = 0};
  auto triggers = std::span(suspend->triggers_ptr, suspend->num_triggers);

  bool has_late_bound =
      suspend->num_late_bound > 0 && suspend->late_bound_ptr != nullptr;

  // Validate late-bound presence matches compiled descriptor.
  if (has_late_bound != descriptor.has_late_bound) {
    throw common::InternalError(
        "Engine::InstallWaitSite",
        std::format(
            "process {} wait_site {}: has_late_bound mismatch: "
            "descriptor={} vs suspend={}",
            handle.process_id, descriptor.id, descriptor.has_late_bound,
            has_late_bound));
  }

  // Collect created subscription nodes for late-bound rebinding.
  std::vector<SubscriptionNode*> edge_nodes;
  if (has_late_bound) {
    edge_nodes.resize(suspend->num_triggers, nullptr);
  }

  // Pre-scan: build container stride map from late-bound headers.
  // Use a linear scan instead of hash map -- trigger counts are small.
  std::vector<std::pair<uint32_t, uint32_t>> container_strides;
  if (has_late_bound) {
    auto headers = std::span(suspend->late_bound_ptr, suspend->num_late_bound);
    for (const auto& hdr : headers) {
      if (hdr.container_elem_stride > 0) {
        container_strides.emplace_back(
            hdr.trigger_index, hdr.container_elem_stride);
      }
    }
  }

  for (uint32_t i = 0; i < triggers.size(); ++i) {
    const auto& trigger = triggers[i];
    auto edge = static_cast<common::EdgeKind>(trigger.edge);
    SubscriptionNode* node = nullptr;

    // Linear scan for container stride.
    uint32_t cs_stride = 0;
    for (const auto& [idx, stride] : container_strides) {
      if (idx == i) {
        cs_stride = stride;
        break;
      }
    }

    if (cs_stride > 0) {
      int64_t sv_index =
          (trigger.byte_size == UINT32_MAX)
              ? -1
              : static_cast<int64_t>(trigger.byte_offset / cs_stride);
      node = SubscribeContainerElement(
          handle, resume, trigger.signal_id, edge, sv_index, cs_stride);
    } else if (trigger.byte_size == UINT32_MAX) {
      node = Subscribe(handle, resume, trigger.signal_id, edge, 0, 1, 0);
      if (node != nullptr) node->is_active = false;
    } else if (trigger.byte_size > 0) {
      node = Subscribe(
          handle, resume, trigger.signal_id, edge, trigger.byte_offset,
          trigger.byte_size, trigger.bit_index);
    } else {
      node = Subscribe(handle, resume, trigger.signal_id, edge);
    }
    if (has_late_bound) {
      edge_nodes[i] = node;
    }
  }

  // Create rebind subscriptions for late-bound triggers.
  if (has_late_bound) {
    auto headers = std::span(suspend->late_bound_ptr, suspend->num_late_bound);
    auto all_plan_ops = std::span(suspend->plan_ops_ptr, suspend->num_plan_ops);
    auto all_dep_slots =
        std::span(suspend->dep_slots_ptr, suspend->num_dep_slots);
    for (const auto& hdr : headers) {
      if (hdr.trigger_index >= suspend->num_triggers) {
        throw common::InternalError(
            "Engine::InstallWaitSite",
            std::format(
                "process {} wait_site {}: trigger_index {} >= num_triggers {}",
                handle.process_id, descriptor.id, hdr.trigger_index,
                suspend->num_triggers));
      }
      auto* target = edge_nodes[hdr.trigger_index];
      if (target == nullptr) continue;
      if (hdr.dep_slots_count == 0) continue;
      if (hdr.plan_ops_start + hdr.plan_ops_count > suspend->num_plan_ops) {
        throw common::InternalError(
            "Engine::InstallWaitSite",
            std::format(
                "process {} wait_site {}: plan_ops span [{}, +{}) exceeds "
                "pool size {}",
                handle.process_id, descriptor.id, hdr.plan_ops_start,
                hdr.plan_ops_count, suspend->num_plan_ops));
      }
      if (hdr.dep_slots_start + hdr.dep_slots_count > suspend->num_dep_slots) {
        throw common::InternalError(
            "Engine::InstallWaitSite",
            std::format(
                "process {} wait_site {}: dep_slots span [{}, +{}) exceeds "
                "pool size {}",
                handle.process_id, descriptor.id, hdr.dep_slots_start,
                hdr.dep_slots_count, suspend->num_dep_slots));
      }
      BitTargetMapping mapping{
          .index_base = hdr.index_base,
          .index_step = hdr.index_step,
          .total_bits = hdr.total_bits};
      auto plan_ops =
          all_plan_ops.subspan(hdr.plan_ops_start, hdr.plan_ops_count);
      auto dep_slots =
          all_dep_slots.subspan(hdr.dep_slots_start, hdr.dep_slots_count);
      SubscribeRebind(handle, resume, target, plan_ops, mapping, dep_slots);
    }
  }

  // Unconditionally publish installed wait state from the compiled descriptor.
  auto& proc_state = process_states_[handle.process_id];
  proc_state.installed_wait = InstalledWaitState{
      .wait_site_id = descriptor.id, .shape = descriptor.shape, .valid = true};
}

void Engine::RegisterSuspendRecords(std::span<SuspendRecord*> records) {
  suspend_records_.assign(records.begin(), records.end());
}

void Engine::ReconcilePostActivation(ProcessHandle handle) {
  if (!HasPostActivationReconciliation()) {
    throw common::InternalError(
        "Engine::ReconcilePostActivation",
        "called without post-activation reconciliation capability");
  }
  if (handle.process_id >= suspend_records_.size()) {
    throw common::InternalError(
        "Engine::ReconcilePostActivation",
        std::format(
            "process_id {} >= suspend_records size {}", handle.process_id,
            suspend_records_.size()));
  }
  auto* suspend = suspend_records_[handle.process_id];

  auto resume =
      ResumePoint{.block_index = suspend->resume_block, .instruction_index = 0};

  switch (suspend->tag) {
    case SuspendTag::kFinished:
      ResetInstalledWait(handle);
      break;

    case SuspendTag::kDelay:
      ResetInstalledWait(handle);
      Delay(handle, resume, suspend->delay_ticks);
      break;

    case SuspendTag::kWait: {
      // Validate wait_site_id before use.
      if (suspend->wait_site_id == kInvalidWaitSiteId) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} suspended with kWait but wait_site_id is invalid",
                handle.process_id));
      }
      if (suspend->wait_site_id >= wait_site_meta_.Size()) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} wait_site_id {} >= registry size {}",
                handle.process_id, suspend->wait_site_id,
                wait_site_meta_.Size()));
      }

      const auto& descriptor = wait_site_meta_.Get(suspend->wait_site_id);

      // Validate transitional invariants: compiled descriptor must agree
      // with runtime activation record.
      if (descriptor.resume_block != suspend->resume_block) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} wait_site {} resume_block mismatch: "
                "descriptor={} vs suspend={}",
                handle.process_id, suspend->wait_site_id,
                descriptor.resume_block, suspend->resume_block));
      }
      if (descriptor.num_triggers != suspend->num_triggers) {
        throw common::InternalError(
            "Engine::ReconcilePostActivation",
            std::format(
                "process {} wait_site {} num_triggers mismatch: "
                "descriptor={} vs suspend={}",
                handle.process_id, suspend->wait_site_id,
                descriptor.num_triggers, suspend->num_triggers));
      }

      auto& proc_state = process_states_[handle.process_id];
      bool can_refresh =
          proc_state.installed_wait.valid &&
          proc_state.installed_wait.wait_site_id == suspend->wait_site_id &&
          descriptor.shape == WaitShapeKind::kStatic;

      if (can_refresh) {
        RefreshInstalledSnapshots(handle);
      } else {
        ResetInstalledWait(handle);
        InstallWaitSite(handle, suspend, descriptor);
      }
      break;
    }

    case SuspendTag::kRepeat:
      ResetInstalledWait(handle);
      ScheduleNextDelta(handle, ResumePoint{.block_index = 0});
      break;
  }
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

  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::Subscribe", std::format(
                                 "process_id {} exceeds num_processes {}",
                                 handle.process_id, num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];

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

  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeContainerElement",
        std::format(
            "process_id {} exceeds num_processes {}", handle.process_id,
            num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];
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

  if (handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::SubscribeRebind", std::format(
                                       "process_id {} exceeds num_processes {}",
                                       handle.process_id, num_processes_));
  }
  auto& proc_state = process_states_[handle.process_id];

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
  if (rebind_node->handle.process_id >= num_processes_) {
    throw common::InternalError(
        "Engine::RebindSubscription",
        std::format(
            "process_id {} exceeds num_processes {}",
            rebind_node->handle.process_id, num_processes_));
  }
  auto& proc_state = process_states_[rebind_node->handle.process_id];
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
    return;
  }

  // Guard: MIR path has no slot meta / design state.
  if (!slot_meta_registry_.IsPopulated() || design_state_base_ == nullptr) {
    return;
  }

  auto newly_dirty = update_set_.DeltaDirtySlots();
  if (newly_dirty.empty()) {
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
    const auto& sw = signal_waiters_[slot_id];
    if (sw.rebind_head == nullptr) continue;

    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto* node = sw.rebind_head; node != nullptr;) {
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
  const bool has_external = update_set_.HasExternalRanges();

  for (uint32_t slot_id : newly_dirty) {
    const auto& sw = signal_waiters_[slot_id];
    if (sw.head == nullptr) continue;

    const auto& dirty_ranges = update_set_.DeltaRangesFor(slot_id);
    static const common::RangeSet kEmptyRangeSet;
    const auto& external_ranges =
        has_external ? update_set_.DeltaExternalRangesFor(slot_id)
                     : kEmptyRangeSet;
    const auto& meta = slot_meta_registry_.Get(slot_id);

    for (auto* node = sw.head; node != nullptr;) {
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
          if (node->handle.process_id >= num_processes_) {
            throw common::InternalError(
                "Engine::FlushSignalUpdates",
                std::format(
                    "wakeup process_id {} exceeds num_processes {}",
                    node->handle.process_id, num_processes_));
          }
          auto& proc_state = process_states_[node->handle.process_id];
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
        auto& proc_state = process_states_[node->handle.process_id];
        if (!proc_state.is_enqueued) {
          next_delta_queue_.push_back(
              ScheduledEvent{.handle = node->handle, .resume = node->resume});
          proc_state.is_enqueued = true;
        }
      }

      node = next;
    }
  }

  // Note: delta is NOT cleared here. The caller (FlushAndPropagateConnections)
  // clears it after connection evaluation has also consumed the delta.
}

}  // namespace lyra::runtime
