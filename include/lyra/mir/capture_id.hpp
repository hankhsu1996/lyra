#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of one environment field of a closure's callable code -- a binding
// the code reads that is bound at closure-value construction, not created per
// invocation. Indexes the code's `captures` arena. A distinct id space from
// `LocalId` (the activation locals): a body reference is either a `LocalRef`
// (an activation local / parameter) or a `CaptureRef` (an environment field),
// never one disguised as the other. Scope-local to the callable, like the
// other arena ids.
struct CaptureId {
  std::uint32_t value;

  auto operator<=>(const CaptureId&) const -> std::strong_ordering = default;
};

}  // namespace lyra::mir
