#include "lyra/runtime/engine_subscriptions.hpp"

#include <algorithm>
#include <cstddef>
#include <format>
#include <print>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"

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

  std::println(
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

  std::println(stderr, "  top {} signals by waiter count:", count);
  for (size_t i = 0; i < std::min(count, waiter_counts.size()); ++i) {
    std::println(
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
  if (finished_) {
    return;
  }

  auto& proc_state = process_states_[handle];

  if (!CheckSubscriptionLimits(proc_state)) {
    return;
  }

  auto* node = AllocNode();
  *node = SubscriptionNode{
      .handle = handle, .resume = resume, .edge = edge, .signal = signal};

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

void Engine::RecordSignalUpdate(
    SignalId signal, bool old_lsb, bool new_lsb, bool value_changed) {
  if (finished_) {
    return;
  }

  auto& record = pending_edges_[signal];

  if (!record.initialized) {
    // First update for this signal in this delta: capture initial LSB
    record.initialized = true;
    record.prev_lsb = old_lsb;
  }

  // Detect edge from prev_lsb to new_lsb
  if (!record.prev_lsb && new_lsb) {
    record.saw_posedge = true;
  }
  if (record.prev_lsb && !new_lsb) {
    record.saw_negedge = true;
  }

  // Update prev_lsb for next update in this delta
  record.prev_lsb = new_lsb;

  // Accumulate value_changed (never invent change events)
  record.value_changed |= value_changed;
}

auto Engine::EvaluateEdgeFromRecord(
    common::EdgeKind edge, const EdgeRecord& record) -> bool {
  switch (edge) {
    case common::EdgeKind::kPosedge:
      return record.saw_posedge;
    case common::EdgeKind::kNegedge:
      return record.saw_negedge;
    case common::EdgeKind::kAnyChange:
      return record.value_changed;
  }
  std::unreachable();
}

void Engine::FlushSignalUpdates() {
  if (pending_edges_.empty() || finished_) {
    return;
  }

  for (const auto& [signal, record] : pending_edges_) {
    auto it = signal_waiters_.find(signal);
    if (it == signal_waiters_.end()) {
      continue;
    }

    for (auto* node = it->second.head; node != nullptr;) {
      auto* next = node->signal_next;

      if (EvaluateEdgeFromRecord(node->edge, record)) {
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

  pending_edges_.clear();
}

}  // namespace lyra::runtime
