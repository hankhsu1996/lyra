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

// Resolution policy for `wire` / `tri` nets (LRM 6.6.1, Table 6-2). A net's
// value is the resolution of its drivers' current contributions, folded under
// the tri-state truth table. The undriven value seeds the fold: with no driver
// it is the result, and because high-impedance is the resolution identity a
// single driver folds to its own value while additional drivers combine, a
// 0/1 conflict resolving to `x`.
struct WireResolver {
  [[nodiscard]] static auto Resolve(
      const std::vector<DriveContribution<value::PackedArray>>& slots,
      const value::PackedArray& undriven) -> value::PackedArray {
    value::PackedArray resolved = undriven;
    for (const auto& slot : slots) {
      resolved = resolved.ResolveTriState(slot.value);
    }
    return resolved;
  }

  // The empty-driver value of a `wire` / `tri` net: high-impedance at the
  // declared width (LRM 6.6.1). `prototype` carries the net's declared type;
  // its contents are unused. The net installs this at construction so an
  // undriven read sees the net type's undriven value.
  [[nodiscard]] static auto ResolveEmpty(const value::PackedArray& prototype)
      -> value::PackedArray {
    return value::PackedArray::HighImpedanceLike(prototype);
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

  // Installs, once at construction, the net's empty-driver resolved value at
  // the prototype's declared type. The net is therefore a readable, well-typed
  // observable before any driver attaches, and an undriven read sees the net
  // type's undriven value rather than an uninitialized cell. Installing twice
  // is a lowering defect.
  void Initialize(T prototype) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!resolved_.IsUninitialized()) {
        throw InternalError(
            "ResolvedNet::Initialize: cell is already initialized");
      }
    }
    undriven_ = Resolver::ResolveEmpty(prototype);
    resolved_ = undriven_;
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
            *this,
            [](std::uint64_t, std::uint64_t, support::EventEdge edge) -> bool {
              return edge == support::EventEdge::kAnyChange;
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
