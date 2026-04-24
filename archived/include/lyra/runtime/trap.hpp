#pragma once

#include <cstdint>

namespace lyra::runtime {

enum class TrapReason : uint32_t {
  kIterationLimitExceeded,
  kUserFatal,
  kInternalError,
};

struct TrapPayload {
  TrapReason reason = TrapReason::kIterationLimitExceeded;
  uint32_t a = 0;  // e.g., back_edge_site_id
  uint32_t b = 0;  // spare
};

}  // namespace lyra::runtime
