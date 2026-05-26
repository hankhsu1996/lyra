#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

struct DeferredCheckSiteId {
  std::uint32_t value;

  auto operator<=>(const DeferredCheckSiteId&) const
      -> std::strong_ordering = default;
};

// Re-submits at the same site_id within one time slot replace the prior
// closure; this last-write-wins is the glitch-suppression mechanism for
// deferred checks during combinational settle.
struct RuntimeSubmitObservedCall {
  DeferredCheckSiteId site_id;
  ExprId closure;
};

}  // namespace lyra::mir
