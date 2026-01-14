// context.hpp

#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include "lyra/common/timescale.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::lowering::mir_to_lir {

class LoweringContext {
 public:
  struct Loop {
    lir::LabelRef continue_label;
    lir::LabelRef break_label;
  };

  void PushLoop(Loop loop) {
    loop_stack_.push_back(std::move(loop));
  }

  void PopLoop() {
    loop_stack_.pop_back();
  }

  [[nodiscard]] auto CurrentLoop() const -> const Loop& {
    return loop_stack_.back();
  }

  [[nodiscard]] auto HasLoop() const -> bool {
    return !loop_stack_.empty();
  }

  /// Set the timescale for the current module.
  void SetTimescale(
      std::optional<common::TimeScale> ts, int8_t global_precision) {
    timescale_ = ts;
    global_precision_power_ = global_precision;
  }

  /// Get the multiplier to scale delays from module's timeunit to global
  /// precision.
  [[nodiscard]] auto DelayMultiplier() const -> uint64_t {
    if (timescale_) {
      return timescale_->DelayMultiplier(global_precision_power_);
    }
    // Default timescale: 1ns / 1ps
    return common::TimeScale::Default().DelayMultiplier(
        global_precision_power_);
  }

  [[nodiscard]] auto GlobalPrecisionPower() const -> int8_t {
    return global_precision_power_;
  }

 private:
  std::vector<Loop> loop_stack_;
  std::optional<common::TimeScale> timescale_;
  int8_t global_precision_power_ = common::TimeScale::kDefaultPrecisionPower;
};

}  // namespace lyra::lowering::mir_to_lir
