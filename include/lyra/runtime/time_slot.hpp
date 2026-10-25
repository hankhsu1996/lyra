#pragma once

#include <array>
#include <cstddef>
#include <optional>
#include <utility>

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/deferred_effects.hpp"
#include "lyra/runtime/effect_storage.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/timer.hpp"

namespace lyra::runtime {

// What one region of one time slot holds pending, in the three kinds an event
// can be. Processes of one region may run in any order (LRM 4.7), and
// nonblocking updates run in the order they were placed (LRM 4.6), so each
// kind is kept in the container that suits it: an activation and a timer are
// each reached through the place in a queue it owns, which is what keeps taking
// a queued one off a constant-time unlink, and a deferred effect is made in the
// run's effect storage and held in order until the region runs.
struct RegionQueue {
  explicit RegionQueue(EffectStorage& storage) : effects(storage) {
  }

  IntrusiveList<QueuePlace> activations;
  IntrusiveList<TimerPlace> timers;
  DeferredEffects effects;

  [[nodiscard]] auto Empty() const noexcept -> bool {
    return activations.Empty() && timers.Empty() && effects.Empty();
  }

  // Moves everything `other` holds onto the back of this queue, each kind in
  // the order it was placed (LRM 4.6), leaving `other` empty.
  void Append(RegionQueue& other) {
    activations.Append(other.activations);
    timers.Append(other.timers);
    effects.Append(other.effects);
  }
};

// One time slot: what each of its regions has pending. The regions are indexed
// rather than separately named because LRM 4.5 iterates them -- its inner loop
// asks for the first nonempty region of a range -- so a slot answers that
// question by scanning itself.
class TimeSlot {
 public:
  explicit TimeSlot(EffectStorage& storage)
      : regions_(
            RegionsOver(storage, std::make_index_sequence<kRegionCount>{})) {
  }

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

  // Makes every event of a later slot this one's, when time reaches it.
  void Append(TimeSlot& other) {
    for (std::size_t index = 0; index < kRegionCount; ++index) {
      regions_.at(index).Append(other.regions_.at(index));
    }
  }

 private:
  template <std::size_t>
  static auto QueueOver(EffectStorage& storage) -> RegionQueue {
    return RegionQueue(storage);
  }
  template <std::size_t... Index>
  static auto RegionsOver(EffectStorage& storage, std::index_sequence<Index...>)
      -> std::array<RegionQueue, kRegionCount> {
    return {QueueOver<Index>(storage)...};
  }

  std::array<RegionQueue, kRegionCount> regions_;
};

}  // namespace lyra::runtime
