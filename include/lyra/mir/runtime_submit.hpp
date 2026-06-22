#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/runtime_print.hpp"

namespace lyra::mir {

// Identity for a deferred-check site (`$assert final` / unique-cascade
// observed submit). Compile-time-allocated per site by the compilation unit;
// the engine indexes its per-scope pending-closure table by the same id.
struct DeferredCheckSiteId {
  std::uint32_t value;

  auto operator<=>(const DeferredCheckSiteId&) const
      -> std::strong_ordering = default;
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
