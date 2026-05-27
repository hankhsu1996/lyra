#pragma once

#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"

namespace lyra::runtime {

class RuntimeEvent;
class Observable;

// LRM 9.4.2 edge specifier on `@(...)`. Stored per-trigger on the wait so the
// dispatcher can decide which subscribed waiters to wake on a value change.
enum class Edge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

// One observable + the edge polarity the waiter cares about. Multiple Triggers
// in a single ValueChangeWait model an event list `@(a or posedge b)`; the
// process resumes when any matches.
struct Trigger {
  Observable* observable = nullptr;
  Edge edge = Edge::kAnyChange;
};

struct DelayWait {
  SimDuration duration;
};

struct EventWait {
  RuntimeEvent* event = nullptr;
};

struct ValueChangeWait {
  std::vector<Trigger> triggers;
};

struct FinishWait {
  int level;
};

using WaitRequest =
    std::variant<DelayWait, EventWait, ValueChangeWait, FinishWait>;

class ProcessRunResult {
 public:
  static auto Completed() -> ProcessRunResult {
    return ProcessRunResult{};
  }
  static auto Suspended(WaitRequest wait) -> ProcessRunResult {
    return ProcessRunResult{std::move(wait)};
  }

  [[nodiscard]] auto IsCompleted() const -> bool {
    return !wait_.has_value();
  }
  [[nodiscard]] auto IsSuspended() const -> bool {
    return wait_.has_value();
  }

  auto TakeWait() -> WaitRequest {
    if (!wait_.has_value()) {
      throw InternalError(
          "ProcessRunResult::TakeWait: completed result has no wait");
    }
    auto out = std::move(*wait_);
    wait_.reset();
    return out;
  }

 private:
  ProcessRunResult() = default;
  explicit ProcessRunResult(WaitRequest wait) : wait_(std::move(wait)) {
  }
  std::optional<WaitRequest> wait_;
};

}  // namespace lyra::runtime
