#pragma once

#include <string>

namespace lyra::lir {

struct ExecuteResult {
  enum class Action { kContinue, kDelay, kFinish, kJump };

  Action action{};
  uint64_t delay_amount = 0;
  std::string target_label{};

  static auto Continue() -> ExecuteResult {
    return ExecuteResult{.action = Action::kContinue};
  }

  static auto Delay(uint64_t amount) -> ExecuteResult {
    return ExecuteResult{.action = Action::kDelay, .delay_amount = amount};
  }

  static auto Finish() -> ExecuteResult {
    return ExecuteResult{.action = Action::kFinish};
  }

  static auto Jump(std::string label) -> ExecuteResult {
    return ExecuteResult{
        .action = Action::kJump, .target_label = std::move(label)};
  }
};

}  // namespace lyra::lir
