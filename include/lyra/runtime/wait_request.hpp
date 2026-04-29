#pragma once

#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"

namespace lyra::runtime {

struct DelayRequest {
  SimDuration duration;
};

using WaitRequest = std::variant<DelayRequest>;

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
