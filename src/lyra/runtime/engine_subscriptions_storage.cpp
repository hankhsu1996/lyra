#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <string_view>

#include <fmt/core.h>

#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/engine_subscriptions.hpp"

namespace lyra::runtime {

auto Engine::AllocEdgeCold() -> uint32_t {
  if (!edge_cold_free_list_.empty()) {
    uint32_t idx = edge_cold_free_list_.back();
    edge_cold_free_list_.pop_back();
    edge_cold_pool_[idx] = EdgeTargetCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(edge_cold_pool_.size());
  edge_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeEdgeCold(uint32_t idx) {
  edge_cold_pool_[idx] = EdgeTargetCold{};
  edge_cold_free_list_.push_back(idx);
}

auto Engine::AllocChangeCold() -> uint32_t {
  if (!change_cold_free_list_.empty()) {
    uint32_t idx = change_cold_free_list_.back();
    change_cold_free_list_.pop_back();
    change_cold_pool_[idx] = ChangeSnapshotCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(change_cold_pool_.size());
  change_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeChangeCold(uint32_t idx) {
  change_cold_pool_[idx].snapshot.clear();
  change_cold_pool_[idx].snapshot.shrink_to_fit();
  change_cold_free_list_.push_back(idx);
}

auto Engine::AllocWatcherCold() -> uint32_t {
  if (!watcher_cold_free_list_.empty()) {
    uint32_t idx = watcher_cold_free_list_.back();
    watcher_cold_free_list_.pop_back();
    watcher_cold_pool_[idx] = WatcherCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(watcher_cold_pool_.size());
  watcher_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeWatcherCold(uint32_t idx) {
  watcher_cold_pool_[idx].snapshot.clear();
  watcher_cold_pool_[idx].snapshot.shrink_to_fit();
  watcher_cold_free_list_.push_back(idx);
}

auto Engine::AllocContainerCold() -> uint32_t {
  if (!container_cold_free_list_.empty()) {
    uint32_t idx = container_cold_free_list_.back();
    container_cold_free_list_.pop_back();
    container_cold_pool_[idx] = ContainerCold{};
    return idx;
  }
  auto idx = static_cast<uint32_t>(container_cold_pool_.size());
  container_cold_pool_.emplace_back();
  return idx;
}

void Engine::FreeContainerCold(uint32_t idx) {
  container_cold_pool_[idx] = ContainerCold{};
  container_cold_free_list_.push_back(idx);
}

auto Engine::AllocEdgeTarget(EdgeTargetHandle handle) -> uint32_t {
  if (!edge_target_free_list_.empty()) {
    uint32_t id = edge_target_free_list_.back();
    edge_target_free_list_.pop_back();
    edge_target_table_[id] = handle;
    return id;
  }
  auto id = static_cast<uint32_t>(edge_target_table_.size());
  edge_target_table_.push_back(handle);
  return id;
}

void Engine::FreeEdgeTarget(uint32_t id) {
  edge_target_table_[id] = EdgeTargetHandle{
      .slot_id = UINT32_MAX, .kind = SubKind::kEdge, .index = UINT32_MAX};
  edge_target_free_list_.push_back(id);
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

  for (uint32_t signal = 0; signal < signal_subs_.size(); ++signal) {
    const auto& slot = signal_subs_[signal];
    size_t n = 0;
    for (const auto& g : slot.edge_groups) {
      n += g.posedge_subs.size() + g.negedge_subs.size();
    }
    n += slot.change_subs.size() + slot.rebind_subs.size() +
         slot.container_subs.size();
    if (n == 0) continue;
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

}  // namespace lyra::runtime
