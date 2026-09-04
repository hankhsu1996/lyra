#pragma once

#include <array>
#include <cstddef>
#include <functional>
#include <optional>
#include <vector>

#include "lyra/runtime/region.hpp"
#include "lyra/runtime/registration.hpp"

namespace lyra::runtime {

// What one region of one time slot holds pending, in the two kinds an event can
// be. LRM 4.5 fixes no order between the events of one region, so each kind is
// kept in the container that suits it: an activation is reached through a
// registration record it owns, which is what keeps revoking a parked one a
// constant-time unlink, and a deferred effect owns nothing and is held until
// the region runs.
struct RegionQueue {
  RegistrationList activations;
  std::vector<std::function<void()>> effects;

  [[nodiscard]] auto Empty() const noexcept -> bool {
    return activations.Empty() && effects.empty();
  }
};

// One time slot: what each of its regions has pending. The regions are indexed
// rather than separately named because LRM 4.5 iterates them -- its inner loop
// asks for the first nonempty region of a range -- so a slot answers that
// question by scanning itself.
class TimeSlot {
 public:
  auto operator[](Region region) -> RegionQueue& {
    return regions_.at(static_cast<std::size_t>(region));
  }

  [[nodiscard]] auto FirstPending(Region first, Region last) const
      -> std::optional<Region> {
    for (auto index = static_cast<std::size_t>(first);
         index <= static_cast<std::size_t>(last); ++index) {
      if (!regions_.at(index).Empty()) {
        return static_cast<Region>(index);
      }
    }
    return std::nullopt;
  }

  [[nodiscard]] auto Empty() const noexcept -> bool {
    return !FirstPending(Region::kPreponed, Region::kPostponed).has_value();
  }

 private:
  std::array<RegionQueue, kRegionCount> regions_;
};

}  // namespace lyra::runtime
