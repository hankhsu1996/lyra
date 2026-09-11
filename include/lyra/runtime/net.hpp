#pragma once

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/net_resolution.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// Which level a contribution is driven at. It arrives as a PackedArray
// literal, the way every compile-time scalar crosses into a runtime entry.
[[nodiscard]] inline auto StrengthLevelOf(const value::PackedArray& level)
    -> support::StrengthLevel {
  return static_cast<support::StrengthLevel>(level.ToInt64());
}

// A position or a count of them, as a compile-time scalar reaches a runtime
// entry: the same `PackedArray` literal carriage every other one takes.
[[nodiscard]] inline auto PositionOf(const value::PackedArray& count)
    -> std::uint32_t {
  return static_cast<std::uint32_t>(count.ToInt64());
}

// One contribution to a net's resolution: a logic value and the strength it is
// driven at (LRM 28.11). Strength rides the contribution rather than the net's
// resolved value, because all it decides is which contribution determines a
// position, and nothing that reads a net asks how strongly it got there.
template <value::NetResolvable T>
struct DriveContribution {
  T value{};
  support::StrengthLevel strength{};
};

// What the contribution a net type makes to its own resolution does once the
// net has been driven: hold what the declaration gave it, or take what the
// drivers last decided, which is how a net stores a value (LRM 6.6.4).
enum class OwnContribution : std::uint8_t { kFixed, kRetained };

template <value::NetResolvable T>
class Driver;

template <value::NetResolvable T>
class ResolvedNet;

template <value::NetResolvable T>
class PhysicalNet;

// Where one declared net sits in a physical net: the net, and where among that
// net's own positions the run the physical net covers begins. Every name in a
// physical net covers the whole of it, so the run's width is the physical net's
// and a name reaching part of one reaches it as a separate physical net -- what
// a connection relates is a run, and a run related to two different things at
// two alignments is two runs. A net no connection reached is the one placement
// of its own physical net at offset zero.
template <value::NetResolvable T>
struct NetPlacement {
  ResolvedNet<T>* net{};
  std::uint32_t net_offset{};
};

// One run of a declared net's own positions and the physical net it reaches
// there. A net's runs cover it exactly and in order, so a name whose positions
// were never split reaches one.
template <value::NetResolvable T>
struct NetReach {
  std::shared_ptr<PhysicalNet<T>> physical;
  std::uint32_t net_offset{};
  std::uint32_t width{};
};

// A physical net: the positions that resolve together. LRM 10.11 names it,
// declaring alias members as signals "whose bits share the same physical nets"
// and an alias itself as multiple names "for the same physical net, or bits
// within a net"; LRM 23.3.3.7 calls the one a port connection forms a simulated
// net. Which declared nets reach it, and over which of their positions, is the
// connectivity of the elaborated design, so this exists only while the design
// runs and nothing compiles against one.
//
// It carries the answers a resolution needs and a name does not: the fold, the
// contribution the net type makes to its own resolution, and any procedural
// continuous assignment in force over the positions. That is what lets two
// positions of one name resolve under different net types, which a
// concatenation of dissimilar nets across a bidirectional port produces --
// LRM 23.3.3.7's table is read per bit range, because different bits may have
// different net types.
//
// A name keeps its own contributions, its own observers, and a copy of what
// these positions produced over the run it reaches, so reading a net reaches
// its own storage directly and never follows a pointer to get there.
template <value::NetResolvable T>
class PhysicalNet {
 public:
  PhysicalNet(
      T nondriving, DriveContribution<T> own, value::PackedArray own_fill,
      value::NetResolution fold, OwnContribution own_kind, std::uint32_t width)
      : nondriving_(std::move(nondriving)),
        own_(std::move(own)),
        own_fill_(std::move(own_fill)),
        fold_(fold),
        own_kind_(own_kind),
        width_(width) {
  }

  PhysicalNet(const PhysicalNet&) = delete;
  auto operator=(const PhysicalNet&) -> PhysicalNet& = delete;
  PhysicalNet(PhysicalNet&&) = delete;
  auto operator=(PhysicalNet&&) -> PhysicalNet& = delete;
  ~PhysicalNet() = default;

 private:
  friend class ResolvedNet<T>;

  // Whether any contribution sits at a given level, one bit per level, so a
  // resolution visits only the levels that exist. A contribution at high
  // impedance is never recorded, because it determines no position (LRM
  // 28.12.1) -- which is what leaves positions nobody drives resolving in no
  // passes at all.
  using OccupiedLevels = std::uint32_t;

  [[nodiscard]] static auto LevelBit(support::StrengthLevel level)
      -> OccupiedLevels {
    if (level == support::StrengthLevel::kHighImpedance) {
      return 0U;
    }
    return OccupiedLevels{1} << static_cast<unsigned>(level);
  }

  // How many positions a value of this type states. An aggregate net states one
  // indivisible position, because nothing names a part of one: a connection
  // reaching such a net covers the whole of it.
  [[nodiscard]] static auto PositionsOf(const T& value) -> std::uint32_t {
    if constexpr (std::same_as<T, value::PackedArray>) {
      return static_cast<std::uint32_t>(value.BitWidth());
    } else {
      return 1;
    }
  }

  // Whether a placement stands for the whole of the name reaching it, so
  // carrying a value between the two leaves every position where it already is.
  // That is the case every design connecting whole nets is in.
  [[nodiscard]] auto CoversTheName(
      const NetPlacement<T>& placement, std::uint64_t net_width) const -> bool {
    return placement.net_offset == 0 && width_ == net_width;
  }

  // A name's value seen at these positions.
  [[nodiscard]] auto FromPlacement(
      const T& value, const NetPlacement<T>& placement) const -> T {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (CoversTheName(placement, value.BitWidth())) {
        return value;
      }
      return value.ExtractBits(
          value::PackedArray::IntUnsigned(placement.net_offset), width_);
    } else {
      return value;
    }
  }

  // These positions seen at a name's, written over whatever that name held
  // there.
  [[nodiscard]] auto IntoPlacement(
      T into, const T& value, const NetPlacement<T>& placement) const -> T {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (CoversTheName(placement, into.BitWidth())) {
        return value;
      }
      into.AssignSlice(
          value::PackedArray::IntUnsigned(placement.net_offset), width_, value);
      return into;
    } else {
      return value;
    }
  }

  // What these positions resolve to, from the contributions every name reaching
  // them has made. Between levels the stronger contribution determines every
  // position it drives and leaves the rest (LRM 28.12.1); within one level the
  // fold decides -- tri-state, wired-and, or wired-or (LRM 6.6.1 Table 6-2, LRM
  // 6.6.3 Tables 6-3 and 6-4, LRM 28.12.4). The net type's own contribution
  // takes part like any other, so positions nothing drives resolve to it, and
  // every level starts from the all-`z` value every fold treats as its
  // identity, so a level nothing occupies and positions with no drivers are not
  // cases of their own.
  //
  // Every operation here reads the same way from every net-valid value type,
  // which is what `value::NetResolvable` states, so one realization serves
  // every net type and a backend that erases the value type still resolves
  // correctly.
  [[nodiscard]] auto Resolve() const -> T;

  // Recomputes these positions and gives every name reaching them what they
  // produced over the run it reaches, each publishing under its own name if
  // what it shows moved (LRM 23.3.3.7). One resolution serves every name,
  // however many a connection joined, because what resolves is this rather than
  // any of them.
  void Reresolve(RuntimeEffects& runtime);

  // These positions cut in two at `at`, which keeps the low part and hands back
  // the high one. Every name here covers the whole of these positions, so each
  // is placed in both halves, the high one at that name's own offset advanced
  // by the cut. A cut is what a connection reaching part of these positions
  // asks for: the part it reaches goes on to resolve with whatever it is
  // coupled to, and the rest of these positions is a resolution of its own.
  [[nodiscard]] auto Split(std::uint32_t at)
      -> std::shared_ptr<PhysicalNet<T>> {
    if constexpr (!std::same_as<T, value::PackedArray>) {
      throw InternalError(
          "PhysicalNet: an aggregate states one indivisible position, so a "
          "connection reaching one reaches the whole of it");
    } else {
      if (at == 0 || at >= width_) {
        throw InternalError(
            "PhysicalNet: a cut falls inside these positions, since a cut at "
            "either end asks for the positions that are already here");
      }
      if (takeovers_ != nullptr) {
        throw InternalError(
            "PhysicalNet: every connection is stated while the design "
            "resolves, which is before any procedural continuous assignment "
            "can have taken these positions over");
      }
      const std::uint32_t above = width_ - at;
      auto high = std::make_shared<PhysicalNet<T>>(
          SliceOfPositions(nondriving_, at, above),
          DriveContribution<T>{
              .value = SliceOfPositions(own_.value, at, above),
              .strength = own_.strength},
          own_fill_, fold_, own_kind_, above);
      for (const NetPlacement<T>& placement : placements_) {
        high->placements_.push_back(
            NetPlacement<T>{
                .net = placement.net, .net_offset = placement.net_offset + at});
      }
      nondriving_ = SliceOfPositions(nondriving_, 0, at);
      own_.value = SliceOfPositions(own_.value, 0, at);
      width_ = at;
      return high;
    }
  }

  // Adds a name over these positions, at the offset among its own that the
  // first of them stands at. A name already here at that offset is already
  // saying this, which is what an alias repeated in two statements states.
  void Admit(const NetPlacement<T>& placement) {
    for (const auto& existing : placements_) {
      if (existing.net == placement.net &&
          existing.net_offset == placement.net_offset) {
        return;
      }
    }
    placements_.push_back(placement);
  }

  // A run of a value's positions, as a value of its own.
  [[nodiscard]] static auto SliceOfPositions(
      const T& value, std::uint32_t from, std::uint32_t width) -> T {
    if constexpr (std::same_as<T, value::PackedArray>) {
      return value.ExtractBits(value::PackedArray::IntUnsigned(from), width);
    } else {
      return value;
    }
  }

  T nondriving_{};
  DriveContribution<T> own_{};
  // The scalar the net type contributes at every position (LRM 6.6.5, 6.6.6),
  // which is what a net type states; the filled contribution above is that
  // scalar taken to these positions. Two nets state the same net type when they
  // agree on this, on the strength it is held at, on the fold, and on whether
  // resolution keeps it current -- and on nothing about how wide either is,
  // which is what lets a connection reach a run narrower than a whole net.
  value::PackedArray own_fill_{};
  value::NetResolution fold_{};
  OwnContribution own_kind_{};
  std::uint32_t width_{};
  std::vector<NetPlacement<T>> placements_;
  // The procedural continuous assignments these positions have been put under
  // (LRM 10.6.2), absent until the first one starts, so positions nobody forces
  // carry neither the storage nor the work of maintaining it. It belongs here
  // rather than to any name because what a force overrides is the drivers of
  // the physical net, so it shows under every name reaching these positions.
  std::unique_ptr<Takeovers<T>> takeovers_;
};

// A net: a name for a run of positions, the contributions the sources in its
// own unit make to them, and what those positions last resolved to (LRM 6.5,
// 6.6). Readable and observable like a `Var<T>` (it extends `Observable`, so a
// process can wait on it), but never written directly: a value reaches it by a
// driver updating its own contribution, or by a procedural continuous
// assignment overriding what the contributions resolve to (LRM 10.6.2), and
// either way the positions re-resolve and the net publishes on a real change
// (LRM 9.4.2). The net owns the contribution storage; a `Driver<T>` names one
// contribution by an index the net issued, so the storage stays the net's to
// reorganize.
//
// What resolves is not the net. A bidirectional connection and an `alias` each
// state that runs of positions across several nets are the same physical net
// (LRM 23.3.3.7, 10.11), so what resolves is that and every name reaching it
// shows what it produced. A net no connection reached is the one name of a
// physical net covering it exactly, which is the same walk over one.
template <value::NetResolvable T>
class ResolvedNet : public Observable {
 public:
  ResolvedNet() = default;

  // Fixes what the net's declaration gives it -- the declared type, from a
  // value carrying it, and what its declared net type states: which truth
  // table resolves contributions of equal strength, and the contribution the
  // net type itself makes, as the value it shows where nothing drives it and
  // the strength it holds that value at (LRM 6.7.1). The net is therefore a
  // readable, well-typed observable before any driver attaches, and its value
  // at that point comes from the same resolution every later value comes from.
  // Installing twice is a lowering defect.
  //
  // One entry per resolution, since a truth table has no spelling as a value
  // the call could carry: tri-state for `wire` / `tri` (LRM 6.6.1 Table 6-2),
  // wired-and for `wand` / `triand` and wired-or for `wor` / `trior` (LRM 6.6.3
  // Tables 6-3 and 6-4), and one that resolves tri-state and leaves its own
  // contribution holding what the drivers last decided, which is how a net
  // stores a value (LRM 6.6.4).
  void InitializeTriState(
      T prototype, const value::PackedArray& fill,
      const value::PackedArray& strength) {
    Install(
        std::move(prototype), fill, strength, value::NetResolution::kTriState,
        OwnContribution::kFixed);
  }
  void InitializeWiredAnd(
      T prototype, const value::PackedArray& fill,
      const value::PackedArray& strength) {
    Install(
        std::move(prototype), fill, strength, value::NetResolution::kWiredAnd,
        OwnContribution::kFixed);
  }
  void InitializeWiredOr(
      T prototype, const value::PackedArray& fill,
      const value::PackedArray& strength) {
    Install(
        std::move(prototype), fill, strength, value::NetResolution::kWiredOr,
        OwnContribution::kFixed);
  }
  void InitializeRetaining(
      T prototype, const value::PackedArray& fill,
      const value::PackedArray& strength) {
    Install(
        std::move(prototype), fill, strength, value::NetResolution::kTriState,
        OwnContribution::kRetained);
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet() = default;

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return resolved_;
  }

  // Puts the positions this net reaches under a procedural continuous
  // assignment and takes them back out (LRM 10.6.2). A `force` on a net
  // overrides every driver rather than joining them, so what these change is
  // what the positions show, never the contributions -- which go on being
  // updated underneath and are what they answer with again once released.
  //
  // What is forced is the physical net, so the value shows under every name
  // reaching those positions. A name covering only part of one has no way to
  // say which part it meant, which is the same gap a force naming part of a
  // target already has.
  auto BeginTakeover(const value::PackedArray& level) -> value::PackedArray {
    PhysicalNet<T>& physical = WholePhysicalNet();
    if (physical.takeovers_ == nullptr) {
      physical.takeovers_ = std::make_unique<Takeovers<T>>();
    }
    return TakeoverGenerationValue(
        physical.takeovers_->Begin(TakeoverLevelOf(level)));
  }

  auto DriveTakeover(
      const value::PackedArray& level, const value::PackedArray& generation,
      const T& value) -> bool {
    PhysicalNet<T>& physical = WholePhysicalNet();
    if (physical.takeovers_ == nullptr ||
        !physical.takeovers_->Drive(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), value)) {
      return false;
    }
    physical.Reresolve(current_runtime());
    return true;
  }

  void EndTakeover(const value::PackedArray& level) {
    PhysicalNet<T>& physical = WholePhysicalNet();
    if (physical.takeovers_ == nullptr) {
      return;
    }
    physical.takeovers_->End(TakeoverLevelOf(level));
    physical.Reresolve(current_runtime());
  }

  // States that `width` positions of this net from `here`, and the same many of
  // `other` from `there`, are one physical net (LRM 23.3.3.7, 10.11). Every
  // driver reaching either run becomes a contribution to the same fold, at the
  // strength it drives at, which is what makes the connection
  // non-strength-reducing (LRM 23.3.3); no net's resolved value is ever an
  // input to another's resolution. Stating a run twice over is a connection
  // restating what another established, which is a shape a design writes rather
  // than a mistake.
  //
  // One physical net has one net type, so the two have to state the same one.
  // Where they differ the standard names a dominating type per pair of nets
  // (LRM 23.3.3.7 Table 23-1), which does not extend to the set a chain of
  // connections joins -- the relation it tabulates is not transitive. The
  // report names what the design wrote rather than which net type won, because
  // which net type a net is, is not something below the net's declaration knows
  // or should learn.
  void Join(
      ResolvedNet* other, const value::PackedArray& here,
      const value::PackedArray& there, const value::PackedArray& width);

  // Attaches a new driver at the strength its source drives at and returns its
  // handle. Its contribution starts at the non-driving one, so a driver that
  // has not yet driven leaves the resolution exactly as it was -- attaching is
  // not itself an act of driving. The contribution list only grows, so an index
  // into it is a stable identity. The handle is the net's own, so a source that
  // can hold one by value copies it out of the reference and one that cannot
  // keeps the reference itself.
  auto AttachDriver(const value::PackedArray& strength) -> Driver<T>&;

 private:
  friend class Driver<T>;
  friend class PhysicalNet<T>;

  // Which of this net's runs begins at `position`, cutting what it reaches so
  // that one does and so that the run is no longer than `within`. A connection
  // names a run of a net's own positions; what the net reaches there was fixed
  // by whatever connections came before, so the two are made to agree here.
  auto RunAt(std::uint32_t position, std::uint32_t within) -> std::size_t;

  // Cuts a physical net in two, `at` positions from its start, and gives every
  // name reaching it the two runs that replace the one.
  static void CutPhysical(
      std::shared_ptr<PhysicalNet<T>> physical, std::uint32_t at);

  // Makes one physical net of two that cover the same positions, leaving every
  // name of the second reaching the first.
  static void MergePhysical(
      std::shared_ptr<PhysicalNet<T>> keep,
      std::shared_ptr<PhysicalNet<T>> folded);

  void Install(
      T prototype, const value::PackedArray& fill,
      const value::PackedArray& strength, value::NetResolution resolution,
      OwnContribution own_kind) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!resolved_.IsUninitialized()) {
        throw InternalError(
            "ResolvedNet: the net's declared type is already fixed");
      }
    }
    nondriving_ =
        T::FilledLike(prototype, value::PackedArray::HighImpedanceScalar());
    const std::uint32_t width = PhysicalNet<T>::PositionsOf(nondriving_);
    auto physical = std::make_shared<PhysicalNet<T>>(
        nondriving_,
        DriveContribution<T>{
            .value = T::FilledLike(prototype, fill),
            .strength = StrengthLevelOf(strength)},
        fill, resolution, own_kind, width);
    physical->Admit(NetPlacement<T>{.net = this, .net_offset = 0});
    resolved_ = physical->Resolve();
    reaches_.push_back(
        NetReach<T>{
            .physical = std::move(physical), .net_offset = 0, .width = width});
  }

  // The physical net this name reaches, where it reaches one covering the whole
  // of it. An operation written on a name rather than on a run needs that,
  // because a name reaching several has no way to say which it meant.
  [[nodiscard]] auto WholePhysicalNet() -> PhysicalNet<T>& {
    if (reaches_.size() != 1 ||
        reaches_.front().width != PhysicalNet<T>::PositionsOf(nondriving_)) {
      throw SimulationError(
          "a procedural continuous assignment names a net that a connection "
          "reaches over part of one resolution, so what it overrides is part "
          "of a physical net, which is not yet supported (LRM 10.6.2)");
    }
    return *reaches_.front().physical;
  }

  void UpdateContribution(
      RuntimeEffects& runtime, std::size_t index, const T& value) {
    ContributionOf(index).value = value;
    ReresolveJoint(runtime);
  }

  // A driver writes wherever this net's own positions are, so every resolution
  // any run of it takes part in recomputes. A net no connection split reaches
  // one, which is the same walk over one.
  void ReresolveJoint(RuntimeEffects& runtime) {
    for (const NetReach<T>& reach : reaches_) {
      reach.physical->Reresolve(runtime);
    }
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
  // The physical nets this name reaches, covering its positions exactly and in
  // order. One, its own, until a connection states that a run of these
  // positions is also a run of another name's -- which cuts this net's runs at
  // that run's ends, since what the two share resolves together and what it
  // does not goes on resolving alone.
  std::vector<NetReach<T>> reaches_;
  // Whether any of this net's own contributions sits at a given level, which a
  // resolution reads from every name reaching it. Recording it per name is what
  // leaves a driver attached after a connection needing nothing propagated, and
  // a connection made after one needing nothing rebuilt.
  typename PhysicalNet<T>::OccupiedLevels occupied_{};
  std::vector<DriveContribution<T>> contributions_;
  // The handles this net has issued. They are the net's rather than each
  // source's so that a source reaching its driver by address holds nothing that
  // points into the contributions above: those stay the net's to reorganize,
  // and what a reorganization would have to rewrite is these, which it can.
  // Growth therefore must not move what has already been handed out.
  std::deque<Driver<T>> drivers_;
};

template <value::NetResolvable T>
auto PhysicalNet<T>::Resolve() const -> T {
  OccupiedLevels occupied = LevelBit(own_.strength);
  for (const auto& placement : placements_) {
    occupied |= placement.net->occupied_;
  }
  T resolved = nondriving_;
  for (std::size_t level = support::kStrengthLevelCount; level-- > 0;) {
    const auto at = static_cast<support::StrengthLevel>(level);
    if ((occupied & LevelBit(at)) == 0U) {
      continue;
    }
    T group = own_.strength == at ? own_.value : nondriving_;
    for (const auto& placement : placements_) {
      for (const auto& driver : placement.net->contributions_) {
        if (driver.strength == at) {
          group =
              group.ResolveNet(FromPlacement(driver.value, placement), fold_);
        }
      }
    }
    resolved = resolved.Dominating(group);
  }
  return resolved;
}

template <value::NetResolvable T>
void PhysicalNet<T>::Reresolve(RuntimeEffects& runtime) {
  T next = Resolve();
  // A net type that stores a value holds what its drivers last decided, so its
  // own contribution takes the resolution it just took part in: the positions
  // something drove are what it now carries, and the positions nothing drove
  // are the ones it decided itself, which leaves them as they were (LRM 6.6.4).
  // A takeover displaces what the positions show and not what they hold, so
  // this happens before one is consulted (LRM 10.6.2).
  if (own_kind_ == OwnContribution::kRetained) {
    own_.value = next;
  }
  const T* forced = takeovers_ == nullptr ? nullptr : takeovers_->Highest();
  if (forced != nullptr) {
    next = *forced;
  }
  for (const NetPlacement<T>& placement : placements_) {
    placement.net->PublishIfChanged(
        runtime, IntoPlacement(placement.net->resolved_, next, placement));
  }
}

template <value::NetResolvable T>
void ResolvedNet<T>::Join(
    ResolvedNet* other, const value::PackedArray& here,
    const value::PackedArray& there, const value::PackedArray& width) {
  std::uint32_t at_here = PositionOf(here);
  std::uint32_t at_there = PositionOf(there);
  std::uint32_t remaining = PositionOf(width);
  // The two sides reach physical nets whose runs fall wherever earlier
  // connections left them, so the coupling is taken in the pieces both sides
  // have whole. Each piece cuts what is longer than it, which is what leaves
  // every physical net covered entirely by every name in it.
  while (remaining > 0) {
    // Each side is asked in turn and cuts as it answers, so asking the first
    // again after the second has answered is what settles the piece: a cut for
    // one side reaches every name in the physical net it cut, the other side
    // among them.
    std::uint32_t run = reaches_[RunAt(at_here, remaining)].width;
    run = other->reaches_[other->RunAt(at_there, run)].width;
    const std::shared_ptr<PhysicalNet<T>> mine =
        reaches_[RunAt(at_here, run)].physical;
    const std::shared_ptr<PhysicalNet<T>> theirs =
        other->reaches_[other->RunAt(at_there, run)].physical;
    MergePhysical(mine, theirs);
    at_here += run;
    at_there += run;
    remaining -= run;
  }
}

template <value::NetResolvable T>
auto ResolvedNet<T>::RunAt(std::uint32_t position, std::uint32_t within)
    -> std::size_t {
  for (std::size_t at = 0; at < reaches_.size(); ++at) {
    const NetReach<T>& reach = reaches_[at];
    if (position < reach.net_offset ||
        position - reach.net_offset >= reach.width) {
      continue;
    }
    if (position > reach.net_offset) {
      CutPhysical(reach.physical, position - reach.net_offset);
      return RunAt(position, within);
    }
    if (reach.width > within) {
      CutPhysical(reach.physical, within);
      return RunAt(position, within);
    }
    return at;
  }
  throw InternalError(
      "ResolvedNet: a net's own runs cover every position it has, so a "
      "connection naming one reaches a run that holds it");
}

template <value::NetResolvable T>
void ResolvedNet<T>::CutPhysical(
    std::shared_ptr<PhysicalNet<T>> physical, std::uint32_t at) {
  const std::shared_ptr<PhysicalNet<T>> above = physical->Split(at);
  for (const NetPlacement<T>& placement : above->placements_) {
    std::vector<NetReach<T>>& runs = placement.net->reaches_;
    for (std::size_t run = 0; run < runs.size(); ++run) {
      if (runs[run].physical != physical) {
        continue;
      }
      const NetReach<T> tail{
          .physical = above,
          .net_offset = runs[run].net_offset + at,
          .width = runs[run].width - at};
      runs[run].width = at;
      runs.insert(runs.begin() + static_cast<std::ptrdiff_t>(run) + 1, tail);
      break;
    }
  }
}

template <value::NetResolvable T>
void ResolvedNet<T>::MergePhysical(
    std::shared_ptr<PhysicalNet<T>> keep,
    std::shared_ptr<PhysicalNet<T>> folded) {
  if (keep == folded) {
    return;
  }
  if (keep->fold_ != folded->fold_ || keep->own_kind_ != folded->own_kind_ ||
      keep->own_.strength != folded->own_.strength ||
      !keep->own_fill_.IsBitIdentical(folded->own_fill_)) {
    throw SimulationError(
        "a connection makes one physical net of runs whose nets state "
        "dissimilar net types; a resolution over net types that resolve "
        "differently (LRM 23.3.3.7, 10.11) is not yet supported");
  }
  // Both cover the same number of positions and the coupling puts their first
  // ones together, so each name keeps the offset it had among its own.
  for (const NetPlacement<T>& placement : folded->placements_) {
    keep->Admit(placement);
    for (NetReach<T>& reach : placement.net->reaches_) {
      if (reach.physical == folded) {
        reach.physical = keep;
      }
    }
  }
  keep->Reresolve(current_runtime());
}

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
// them. The positions then re-resolve, which is the only place a resolved value
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

  // Publishes this driver's whole contribution; the positions then re-resolve
  // and every name reaching them publishes on a real change. It carries the
  // capability family's store name because a store through a handle reaches
  // whatever that handle addresses, and what this one addresses is a
  // contribution -- never a resolved value.
  void Set(const T& value) const {
    Net().UpdateContribution(current_runtime(), contribution_, value);
  }

  // This driver's own contribution as it currently stands -- what it publishes
  // into the resolution, never the resolved value it arrives at.
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
  // then nothing to redo.
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
    Net().ReresolveJoint(current_runtime());
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
auto ResolvedNet<T>::AttachDriver(const value::PackedArray& strength)
    -> Driver<T>& {
  const support::StrengthLevel level = StrengthLevelOf(strength);
  contributions_.push_back(
      DriveContribution<T>{.value = nondriving_, .strength = level});
  occupied_ |= PhysicalNet<T>::LevelBit(level);
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

static_assert(MutationSink<Driver<value::PackedArray>>);

}  // namespace lyra::runtime
