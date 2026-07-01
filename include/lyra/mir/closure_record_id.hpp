#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// Identity of a closure record declaration within a compilation unit -- the
// per-closure-site nominal value type a closure expression constructs. A
// reference to a closure record (a closure value's type, the receiver of its
// invoke body) names this id, and the unit's closure-record registry resolves
// it to the declaration. Unit-wide like `ClassId`, and deliberately a separate
// id space: a closure record is a value callable, not a nominal object, so it
// is never resolved through the class registry.
struct ClosureRecordId {
  std::uint32_t value;

  auto operator<=>(const ClosureRecordId&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
