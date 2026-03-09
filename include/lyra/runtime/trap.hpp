#pragma once

#include <cstdint>

namespace lyra::runtime {

enum class TrapReason : uint32_t {
  kLoopBudgetExceeded,
  kUserFatal,
  kInternalError,
};

struct TrapPayload {
  TrapReason reason = TrapReason::kLoopBudgetExceeded;
  uint32_t a = 0;  // e.g., loop_site_id
  uint32_t b = 0;  // spare
};

}  // namespace lyra::runtime
