#pragma once

#include <concepts>
#include <cstddef>
#include <deque>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/net_resolution.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/net_resolution.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// The value layer's own naming of the fold a net's declared net type picked
// (LRM 6.6). A net crosses into the runtime carrying the backend-facing
// `support::NetResolution`; the per-bit resolution the value layer performs is
// named by `value::NetResolution`, and this is the one place the two meet.
[[nodiscard]] inline auto ToValueNetResolution(
    support::NetResolution resolution) -> value::NetResolution {
  switch (resolution) {
    case support::NetResolution::kTriState:
      return value::NetResolution::kTriState;
    case support::NetResolution::kWiredAnd:
      return value::NetResolution::kWiredAnd;
    case support::NetResolution::kWiredOr:
      return value::NetResolution::kWiredOr;
  }
  throw InternalError("ToValueNetResolution: unknown net resolution");
}

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

// Folds a net's driver contributions into its resolved value under the truth
// table `resolution` names (LRM 6.6): tri-state for `wire` / `tri`, wired-and
// for `wand` / `triand`, wired-or for `wor` / `trior` (LRM 6.6.1 Table 6-2,
// LRM 6.6.3 Tables 6-3 and 6-4). The fold starts at `nondriving` -- the all-`z`
// value that is every fold's identity -- so an empty driver set is not a case
// of its own, and it reads the same operation from every net-valid value type,
// which is what `value::NetResolvable` states. The fold is a value the net
// carries, not a type it is parameterized by, so one realization serves every
// net type and a backend that erases the value type still resolves correctly.
template <value::NetResolvable T>
[[nodiscard]] auto FoldContributions(
    const std::vector<DriveContribution<T>>& contributions, const T& nondriving,
    value::NetResolution resolution) -> T {
  T resolved = nondriving;
  for (const auto& contribution : contributions) {
    resolved = resolved.ResolveNet(contribution.value, resolution);
  }
  return resolved;
}

template <value::NetResolvable T>
class Driver;

// A net: a resolved observable value produced from a set of independently
// attached driver contributions folded under the net's resolution (LRM 6.5,
// 6.6). Readable and observable like a `Var<T>` (it extends `Observable`, so a
// process can wait on it), but never written directly: a value reaches it by a
// driver updating its own contribution, or by a procedural continuous
// assignment overriding what the drivers resolve to (LRM 10.6.2), and either
// way the net re-resolves and publishes on a real change (LRM 9.4.2). The net
// owns the contribution
// storage; a `Driver<T>` names one contribution by an index the net issued, so
// the storage stays the net's to reorganize. The fold is fixed at construction
// from the declared net type and carried as data, not as a template parameter.
template <value::NetResolvable T>
class ResolvedNet : public Observable {
 public:
  explicit ResolvedNet(support::NetResolution resolution)
      : resolution_(ToValueNetResolution(resolution)) {
  }

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
    nondriving_ = T::HighImpedanceLike(prototype);
    resolved_ = FoldContributions(contributions_, nondriving_, resolution_);
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet() = default;

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return resolved_;
  }

  // Puts the net under a procedural continuous assignment and takes it back
  // out (LRM 10.6.2). A `force` on a net overrides every driver rather than
  // joining them, so what these change is what the net shows, never the
  // contributions -- which go on being updated underneath and are what the net
  // answers with again once it is released.
  auto BeginTakeover(const value::PackedArray& level) -> value::PackedArray {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<Takeovers<T>>();
    }
    return TakeoverGenerationValue(takeovers_->Begin(TakeoverLevelOf(level)));
  }

  auto DriveTakeover(
      const value::PackedArray& level, const value::PackedArray& generation,
      const T& value) -> bool {
    if (takeovers_ == nullptr ||
        !takeovers_->Drive(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), value)) {
      return false;
    }
    Reresolve(current_runtime());
    return true;
  }

  void EndTakeover(const value::PackedArray& level) {
    if (takeovers_ == nullptr) {
      return;
    }
    takeovers_->End(TakeoverLevelOf(level));
    Reresolve(current_runtime());
  }

  // Attaches a new driver and returns its handle. Its contribution starts at
  // the non-driving one, so a driver that has not yet driven leaves the
  // resolution exactly as it was -- attaching is not itself an act of driving.
  // The contribution list only grows, so an index into it is a stable identity.
  // The handle is the net's own, so a source that can hold one by value copies
  // it out of the reference and one that cannot keeps the reference itself.
  auto AttachDriver() -> Driver<T>&;

 private:
  friend class Driver<T>;

  void UpdateContribution(
      RuntimeEffects& runtime, std::size_t index, const T& value) {
    ContributionOf(index).value = value;
    Reresolve(runtime);
  }

  // Recomputes the resolved value from the contributions as they now stand,
  // and publishes it if it moved. A driver that wrote its contribution in
  // place calls this instead of handing one back: the net reads the same
  // storage either way, and the transition that matters is the resolved
  // value's, which no driver can see.
  //
  // This is also where a `force` takes effect (LRM 10.6.2). The fold runs
  // either way and a takeover replaces its result, so the drivers stay current
  // underneath a force and the net is immediately assigned the value they
  // determine the moment it is released.
  void Reresolve(RuntimeEffects& runtime) {
    T next = FoldContributions(contributions_, nondriving_, resolution_);
    const T* forced = takeovers_ == nullptr ? nullptr : takeovers_->Highest();
    if (forced != nullptr) {
      next = *forced;
    }
    PublishIfChanged(runtime, std::move(next));
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
        runtime.WakeWaitersOf(
            *this, MakePackedProjectionTest(old_val, resolved_));
      }
    } else {
      const bool changed = !resolved_.IsBitIdentical(next);
      resolved_ = std::move(next);
      if (changed) {
        runtime.WakeWaitersOf(*this, MakeWholeValueProjectionTest());
      }
    }
  }

  T resolved_{};
  T nondriving_{};
  value::NetResolution resolution_;
  // The procedural continuous assignments this net has been put under (LRM
  // 10.6.2), absent until the first one starts, so a net nobody forces resolves
  // exactly as it did before the construct existed.
  std::unique_ptr<Takeovers<T>> takeovers_;
  std::vector<DriveContribution<T>> contributions_;
  // The handles this net has issued. They are the net's rather than each
  // source's so that a source reaching its driver by address holds nothing that
  // points into the contributions above: those stay the net's to reorganize,
  // and what a reorganization would have to rewrite is these, which it can.
  // Growth therefore must not move what has already been handed out.
  std::deque<Driver<T>> drivers_;
};

// The drive capability for a net: a handle to one contribution of a
// `ResolvedNet`. The net issues it and owns it, so a source may hold it either
// by value or by address; a source's slot starts unbound and the net binds it
// when it attaches, during Resolve. Updating a contribution goes only through
// this handle; the net's contribution storage is never addressed directly, and
// the net's resolved value is never written at all (LRM 6.5).
//
// A source that drives only part of the net opens the same partial-write
// bracket a variable cell offers: the write lands in this driver's
// own contribution and reaches only the positions it names, so the ones it does
// not drive keep contributing high-impedance and defer to whoever does drive
// them. The net then re-resolves, which is the only place the resolved value
// is ever written.
template <value::NetResolvable T>
class Driver {
 public:
  using ValueType = T;
  using TransitionBase = T;

  Driver() = default;
  Driver(ResolvedNet<T>& net, std::size_t contribution)
      : net_(&net), contribution_(contribution) {
  }

  // Publishes this driver's whole contribution; the net then re-resolves and
  // publishes on a real change. It carries the capability family's store name
  // because a store through a handle reaches whatever that handle addresses,
  // and what this one addresses is a contribution -- never the net's resolved
  // value.
  void Set(const T& value) const {
    Net().UpdateContribution(current_runtime(), contribution_, value);
  }

  // This driver's own contribution as it currently stands -- what it publishes
  // into the resolution, never the resolved value the net arrives at.
  [[nodiscard]] auto Get() const -> const T& {
    return Net().ContributionOf(contribution_).value;
  }

  [[nodiscard]] auto Mutate() const -> ScopedMutation<Driver> {
    return ScopedMutation<Driver>{*this};
  }

  // The `MutationSink` surface: this driver's own contribution as storage, and
  // the re-resolution that follows a write to it. What is held from before the
  // write is the contribution as it stood, because a chain that leaves it
  // bit-identical leaves the resolution over it unchanged too, and there is
  // then nothing for the net to redo.
  [[nodiscard]] auto MutationStorage() const -> T& {
    return Net().ContributionOf(contribution_).value;
  }
  [[nodiscard]] auto CaptureTransitionBase() const -> T {
    return MutationStorage();
  }
  void PublishTransition(const T& before) const {
    if (before.IsBitIdentical(MutationStorage())) {
      return;
    }
    Net().Reresolve(current_runtime());
  }

 private:
  [[nodiscard]] auto Net() const -> ResolvedNet<T>& {
    if (net_ == nullptr) {
      throw InternalError("Driver: driver is not attached");
    }
    return *net_;
  }

  ResolvedNet<T>* net_ = nullptr;
  std::size_t contribution_ = 0;
};

template <value::NetResolvable T>
auto ResolvedNet<T>::AttachDriver() -> Driver<T>& {
  contributions_.push_back(DriveContribution<T>{.value = nondriving_});
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

static_assert(MutationSink<Driver<value::PackedArray>>);

}  // namespace lyra::runtime
