#pragma once

#include <cstdint>

namespace lyra::common {

class TimingControl {
 public:
  enum class Kind {
    kDelay,
    kEvent,
  };

  Kind kind{};
  int64_t delay_amount = 0;

  static TimingControl Delay(int64_t amount) {
    TimingControl t;
    t.kind = Kind::kDelay;
    t.delay_amount = amount;
    return t;
  }

  static TimingControl Event() {
    TimingControl t;
    t.kind = Kind::kEvent;
    return t;
  }

 private:
  TimingControl() = default;
};

}  // namespace lyra::common
