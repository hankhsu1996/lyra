#pragma once

#include <concepts>
#include <cstddef>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// The drive strength of a driver's contribution (LRM 28).
struct DriveStrength {
  auto operator==(const DriveStrength&) const -> bool = default;
};

// One driver's contribution to a net's resolution: a logic value and its drive
// strength (LRM 28).
template <value::LyraValue T>
struct DriveContribution {
  T value{};
  DriveStrength strength{};
};

// Resolution policy for `wire` / `tri` nets (LRM 6.6.1). A net's value is the
// resolution of its drivers' current contributions: one driver resolves to its
// value, no driver yields the undriven value. A net carrying more than one
// driver is rejected at lowering, so reaching the multi-contribution case here
// is an invariant violation, not a runtime condition.
struct WireResolver {
  [[nodiscard]] static auto Resolve(
      const std::vector<DriveContribution<value::PackedArray>>& slots,
      const value::PackedArray& undriven) -> value::PackedArray {
    if (slots.empty()) {
      return undriven;
    }
    if (slots.size() == 1) {
      return slots.front().value;
    }
    throw InternalError(
        "WireResolver::Resolve: more than one driver contribution; the "
        "lowering rejects multi-driver nets");
  }
};

template <value::LyraValue T, class Resolver>
class Driver;

// A net: a resolved observable value produced from a set of independently
// attached driver contributions under `Resolver` (LRM 6.5, 6.6). Readable and
// observable like a `Var<T>` (it extends `Observable`, so a process can wait on
// it), but never written directly: a value reaches it only by a driver updating
// its own contribution, after which the net re-resolves and publishes on a real
// change (LRM 9.4.2). The node owns the driver slots; a `Driver<T, Resolver>`
// names one slot.
template <value::LyraValue T, class Resolver>
class ResolvedNet : public Observable {
 public:
  ResolvedNet() = default;

  // The undriven value (high-impedance for the common net types) seeds the
  // resolution when no driver contributes.
  explicit ResolvedNet(T undriven)
      : resolved_(undriven), undriven_(std::move(undriven)) {
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet() = default;

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return resolved_;
  }

  // Attaches a new driver and returns its handle. The slot starts at the
  // undriven contribution; the driver seeds its real contribution during
  // Initialize. Slots only grow, so a slot index is a stable identity.
  auto AttachDriver() -> Driver<T, Resolver>;

 private:
  friend class Driver<T, Resolver>;

  void UpdateContribution(
      RuntimeServices& services, std::size_t slot, const T& value) {
    if (slot >= slots_.size()) {
      throw InternalError("ResolvedNet::UpdateContribution: bad slot");
    }
    slots_[slot].value = value;
    PublishIfChanged(services, Resolver::Resolve(slots_, undriven_));
  }

  // Mirrors `Var<T>::Set`: store the resolved value and wake subscribers only
  // when it actually changed (LRM 9.4.2). A contribution that moves without
  // changing the resolved value wakes no observer.
  void PublishIfChanged(RuntimeServices& services, T next) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      const value::PackedArray old_val = resolved_;
      const bool changed = !resolved_.IsBitIdentical(next);
      resolved_ = std::move(next);
      if (changed) {
        services.TriggerValueChange(
            *this, MakePackedArrayEdgeClassifier(old_val, resolved_));
      }
    } else {
      const bool changed = !resolved_.IsBitIdentical(next);
      resolved_ = std::move(next);
      if (changed) {
        services.TriggerValueChange(
            *this, [](std::uint64_t, std::uint64_t, Edge edge) -> bool {
              return edge == Edge::kAnyChange;
            });
      }
    }
  }

  T resolved_{};
  T undriven_{};
  std::vector<DriveContribution<T>> slots_;
};

// The write capability for a net: a handle to one driver slot of a
// `ResolvedNet`. Owned by the driver's source (for a port edge, the parent
// instance). Default-constructed as a member and bound at Resolve via
// `ResolvedNet::AttachDriver`. Updating a contribution goes only through this
// handle; the net's slot storage is never addressed directly.
template <value::LyraValue T, class Resolver>
class Driver {
 public:
  Driver() = default;
  Driver(ResolvedNet<T, Resolver>& node, std::size_t slot)
      : node_(&node), slot_(slot) {
  }

  void Update(RuntimeServices& services, const T& value) const {
    if (node_ == nullptr) {
      throw InternalError("Driver::Update: driver is not attached");
    }
    node_->UpdateContribution(services, slot_, value);
  }

 private:
  ResolvedNet<T, Resolver>* node_ = nullptr;
  std::size_t slot_ = 0;
};

template <value::LyraValue T, class Resolver>
auto ResolvedNet<T, Resolver>::AttachDriver() -> Driver<T, Resolver> {
  slots_.emplace_back();
  return Driver<T, Resolver>{*this, slots_.size() - 1};
}

}  // namespace lyra::runtime
