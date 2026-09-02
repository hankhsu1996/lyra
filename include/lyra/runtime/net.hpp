#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// The drive strength of a driver's contribution (LRM 28). Strength is a
// property of what a driver contributes, not of the net's resolved value, so it
// rides the contribution rather than sitting beside the net.
struct DriveStrength {};

// One driver's contribution to a net's resolution: a logic value and its drive
// strength (LRM 28).
template <value::NetResolvable T>
struct DriveContribution {
  T value{};
  DriveStrength strength{};
};

// Resolution policy for `wire` / `tri` nets (LRM 6.6.1, Table 6-2): the
// drivers' current contributions folded under the tri-state truth table, where
// agreement passes through and a 0/1 conflict yields `x`.
//
// The policy is stated over any value a net may hold (LRM 6.7.1), not over one
// value type: a net's data type may be an unpacked aggregate of net-valid
// elements, and the truth table then applies to each of its bits. Which value
// types those are is `value::NetResolvable`; the fold reads the same two
// operations from every one of them.
struct WireResolver {
  template <value::NetResolvable T>
  [[nodiscard]] static auto Resolve(
      const std::vector<DriveContribution<T>>& contributions,
      const T& nondriving) -> T {
    T resolved = nondriving;
    for (const auto& contribution : contributions) {
      resolved = resolved.ResolveTriState(contribution.value);
    }
    return resolved;
  }

  // What a driver of a `wire` / `tri` net contributes where it is not driving:
  // high-impedance (LRM 6.6.1). That makes it the fold's identity, since
  // folding it in changes nothing. `prototype` carries the net's declared type;
  // its contents are unused.
  //
  // This is not the same question as what the net reads when nothing drives it.
  // The two coincide here, but a net type whose undriven level is 0 or 1
  // answers the second with a built-in driver of that level rather than with a
  // different identity -- the identity has to stay the value a fold can absorb.
  template <value::NetResolvable T>
  [[nodiscard]] static auto NondrivingContribution(const T& prototype) -> T {
    return T::HighImpedanceLike(prototype);
  }
};

template <value::NetResolvable T, class Resolver>
class Driver;

// A net: a resolved observable value produced from a set of independently
// attached driver contributions under `Resolver` (LRM 6.5, 6.6). Readable and
// observable like a `Var<T>` (it extends `Observable`, so a process can wait on
// it), but never written directly: a value reaches it only by a driver updating
// its own contribution, after which the net re-resolves and publishes on a real
// change (LRM 9.4.2). The net owns the contribution storage; a
// `Driver<T, Resolver>` names one contribution by an index the net issued, so
// the storage stays the net's to reorganize.
template <value::NetResolvable T, class Resolver>
class ResolvedNet : public Observable {
 public:
  ResolvedNet() = default;

  // Fixes the net's declared type, once at construction, from a value carrying
  // it. The net is therefore a readable, well-typed observable before any
  // driver attaches. Its value at that point is the fold over no contributions
  // at all, which is the same fold every later value comes from -- an empty
  // driver set is not a case of its own. Installing twice is a lowering defect.
  void Initialize(T prototype) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!resolved_.IsUninitialized()) {
        throw InternalError(
            "ResolvedNet::Initialize: the net's declared type is already "
            "fixed");
      }
    }
    nondriving_ = Resolver::NondrivingContribution(prototype);
    resolved_ = Resolver::Resolve(contributions_, nondriving_);
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet() = default;

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return resolved_;
  }

  // Attaches a new driver and returns its handle. Its contribution starts at
  // the non-driving one, so a driver that has not yet driven leaves the
  // resolution exactly as it was -- attaching is not itself an act of driving.
  // The contribution list only grows, so an index into it is a stable identity.
  // The handle is the net's own, so a source that can hold one by value copies
  // it out of the reference and one that cannot keeps the reference itself.
  auto AttachDriver() -> Driver<T, Resolver>&;

 private:
  friend class Driver<T, Resolver>;

  void UpdateContribution(
      RuntimeEffects& runtime, std::size_t index, const T& value) {
    ContributionOf(index).value = value;
    PublishIfChanged(runtime, Resolver::Resolve(contributions_, nondriving_));
  }

  // The contribution a driver names. Every driver reaches its own through the
  // index the net issued it, so an index the net never issued is a lowering
  // defect rather than a value to answer. A driver reads its contribution back
  // to write part of it without disturbing the rest (LRM 6.6.1): the positions
  // it has never driven still hold what it started at.
  [[nodiscard]] auto ContributionOf(std::size_t index)
      -> DriveContribution<T>& {
    if (index >= contributions_.size()) {
      throw InternalError("ResolvedNet: driver names no attached contribution");
    }
    return contributions_[index];
  }

  // Mirrors `Var<T>::Set`: store the resolved value and wake subscribers only
  // when it actually changed (LRM 9.4.2). A contribution that moves without
  // changing the resolved value wakes no observer.
  void PublishIfChanged(RuntimeEffects& runtime, T next) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      const value::PackedArray old_val = resolved_;
      const bool changed = !resolved_.IsBitIdentical(next);
      resolved_ = std::move(next);
      if (changed) {
        runtime.TriggerValueChange(
            *this, MakePackedArrayEdgeClassifier(old_val, resolved_));
      }
    } else {
      const bool changed = !resolved_.IsBitIdentical(next);
      resolved_ = std::move(next);
      if (changed) {
        runtime.TriggerValueChange(
            *this,
            [](std::uint64_t, std::uint64_t, support::EventEdge edge) -> bool {
              return edge == support::EventEdge::kAnyChange;
            });
      }
    }
  }

  T resolved_{};
  T nondriving_{};
  std::vector<DriveContribution<T>> contributions_;
  // The handles this net has issued. They are the net's rather than each
  // source's so that a source reaching its driver by address holds nothing that
  // points into the contributions above: those stay the net's to reorganize,
  // and what a reorganization would have to rewrite is these, which it can.
  // Growth therefore must not move what has already been handed out.
  std::deque<Driver<T, Resolver>> drivers_;
};

// The drive capability for a net: a handle to one contribution of a
// `ResolvedNet`. The net issues it and owns it, so a source may hold it either
// by value or by address; a source's slot starts unbound and the net binds it
// when it attaches, during Resolve. Updating a contribution goes only through
// this handle; the net's contribution storage is never addressed directly, and
// the net's resolved value is never written at all (LRM 6.5).
//
// A source that drives only part of the net drives through `Mutate`, the same
// partial-write entry a variable cell offers: what it commits is this driver's
// whole contribution with that part replaced, so the positions it does not
// drive keep contributing high-impedance and defer to whoever does drive them.
template <value::NetResolvable T, class Resolver>
class Driver {
 public:
  using ValueType = T;

  Driver() = default;
  Driver(ResolvedNet<T, Resolver>& net, std::size_t contribution)
      : net_(&net), contribution_(contribution) {
  }

  // Publishes this driver's whole contribution; the net then re-resolves and
  // publishes on a real change. It carries the capability family's store name
  // because a store through a handle reaches whatever that handle addresses,
  // and what this one addresses is a contribution -- never the net's resolved
  // value.
  void Set(RuntimeEffects& runtime, const T& value) const {
    Net().UpdateContribution(runtime, contribution_, value);
  }

  [[nodiscard]] auto Mutate(RuntimeEffects& runtime) const
      -> ScopedMutation<Driver> {
    return ScopedMutation<Driver>{runtime, *this};
  }

  // The `MutationSink` surface: this driver's whole contribution, and the
  // publish that commits a mutated one.
  [[nodiscard]] auto MutationBase() const -> T {
    return Net().ContributionOf(contribution_).value;
  }
  void CommitMutation(RuntimeEffects& runtime, const T& value) const {
    Set(runtime, value);
  }

 private:
  [[nodiscard]] auto Net() const -> ResolvedNet<T, Resolver>& {
    if (net_ == nullptr) {
      throw InternalError("Driver: driver is not attached");
    }
    return *net_;
  }

  ResolvedNet<T, Resolver>* net_ = nullptr;
  std::size_t contribution_ = 0;
};

template <value::NetResolvable T, class Resolver>
auto ResolvedNet<T, Resolver>::AttachDriver() -> Driver<T, Resolver>& {
  contributions_.push_back(DriveContribution<T>{.value = nondriving_});
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

static_assert(MutationSink<Driver<value::PackedArray, WireResolver>>);

}  // namespace lyra::runtime
