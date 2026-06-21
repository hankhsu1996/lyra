#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/runtime_print.hpp"

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

// Append-only submit to the engine's global NBA queue; the closure commits
// in the slot's NBA region in enqueue order (LRM 4.4.2).
struct RuntimeSubmitNbaCall {
  ExprId closure;
};

// LRM 21.2.2 `$strobe` payload. Holds the same `RuntimePrintCall` shape
// `$display` produces -- items reference the enclosing scope's ExprIds
// directly, so the LRM 21.2.2 "evaluate at end-of-slot" semantic falls
// out of where the print is rendered (inside the postponed lambda).
// Block-local references inside the items are by-value snapshotted
// at the C++ codegen boundary via the lambda's init-capture list; module
// signals are reached through `this` and read at fire time.
struct RuntimeSubmitPostponedCall {
  RuntimePrintCall print;
};

}  // namespace lyra::mir
