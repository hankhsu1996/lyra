#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/fixed_array.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// A value of an integral type of `count` positions with every one holding
// `fill` (LRM 6.7.1), as a value of the type `T` that lays it out: `fill` in
// each of its positions, and every position above them clear.
template <BitAddressed T>
[[nodiscard]] auto FilledOver(std::uint64_t count, value::FourStateBit fill)
    -> T {
  if constexpr (value::IntegralValue<T>) {
    T out = T::Zero();
    value::FillScalar(out.MutableView().Truncated(count), fill);
    return out;
  } else {
    return T::Filled(count, fill);
  }
}

// The scalar a net type contributes at each position (LRM 6.7.1), which
// arrives at a net's install as the machine integer every runtime scalar is.
[[nodiscard]] auto NetFillOf(std::int64_t fill) -> value::FourStateBit;

// One contribution to a net's resolution: a logic value and the strength it is
// driven at (LRM 28.11). Strength rides the contribution rather than the net's
// resolved value, because all it decides is which contribution determines a
// position, and nothing that reads a net asks how strongly it got there.
template <class T>
struct DriveContribution {
  T value{};
  support::StrengthLevel strength{};
};

// What the contribution a net type makes to its own resolution does once the
// net has been driven: hold what the declaration gave it, or take what the
// drivers last decided, which is how a net stores a value (LRM 6.6.4).
enum class OwnContribution : std::uint8_t { kFixed, kRetained };

// What decides the value a net shows. It is settled once, by when a driver
// first gives a value (LRM 4.9.1): every driver has attached and every
// connection has been stated before any does. A net one driver decides has no
// value apart from what that driver last produced, because every table
// contributions resolve under leaves a lone one as it stands, its
// high-impedance positions included (LRM 6.6.1 Table 6-2, 6.6.3 Tables 6-3 and
// 6-4), and strength decides only between drivers (LRM 28.12.1). Any other net
// shows what its contributions resolve to.
enum class NetDecision : std::uint8_t { kUnsettled, kOneDriver, kResolution };

// Refuses a driver attaching to a net that has settled what decides it: every
// driver attaches while the design resolves, which is before any gives a value.
void RequireUnsettled(NetDecision decision);

// Replaces the whole of the value `net` shows, which lies at `shown`, by what
// the one driver deciding it gives, as a whole write of a variable is taken
// (LRM 4.3): a net no wait is enrolled on has no one to tell, so nothing is
// compared; a value it already shows moves nothing; and otherwise whoever
// waits is told what moved (LRM 9.4.2). `already_shown` answers whether the
// net shows the same bits, and `overwrite` replaces what it shows.
template <class T, class AlreadyShown, class Overwrite>
void ShowDriverValue(
    RuntimeEffects& runtime, Observable& net, const T& shown,
    AlreadyShown already_shown, Overwrite overwrite) {
  if (!net.HasMembers()) {
    overwrite();
    return;
  }
  if (already_shown()) {
    return;
  }
  runtime.WakeParkedOn(net, PartPath{}, ReplaceWhole(shown, overwrite));
}

// Where a write of one of a net's drivers lands, which says what follows it.
// In a net that resolves it lands in that driver's contribution, and the net
// resolves again. The one driver of a net it decides writes the value the net
// shows, and whoever waits on what moved is told, where a wait is enrolled at
// all (LRM 4.3). While a procedural continuous assignment covers such a net
// the write lands in what is kept beneath it, which the net shows again once
// nothing covers it (LRM 10.6.2), and nothing follows.
template <class T>
struct Contributing {
  T* contribution;
};
template <class T>
struct Showing {
  T* shown;
  bool watched;
};
template <class T>
struct KeptBeneath {
  T* kept;
};
template <class T>
using DriverLanding = std::variant<Contributing<T>, Showing<T>, KeptBeneath<T>>;

// The driver's value as it stands, wherever its writes land.
template <class T>
[[nodiscard]] auto DriverValueAt(const DriverLanding<T>& landing) -> T& {
  return std::visit(
      Overloaded{
          [](const Contributing<T>& to) -> T& { return *to.contribution; },
          [](const Showing<T>& to) -> T& { return *to.shown; },
          [](const KeptBeneath<T>& to) -> T& { return *to.kept; }},
      landing);
}

// Whether any contribution sits at a given level, one bit per level, so a
// resolution visits only the levels that exist. A contribution at high
// impedance is never recorded, because it determines no position (LRM
// 28.12.1) -- which is what leaves positions nobody drives resolving in no
// passes at all.
using OccupiedLevels = std::uint32_t;

[[nodiscard]] auto LevelBit(support::StrengthLevel level) -> OccupiedLevels;

// Some positions of nets whose values are read bit by bit: one four-state bit
// each, and how many there are. A connection cuts a net at the positions it
// names (LRM 23.3.3.7), so how many resolve together is settled while the
// design is constructed, and two nets of different declared types may reach
// the same ones; positions therefore carry no type, and everything done to
// them is done over their words at their count.
class NetPositions {
 public:
  // `count` positions each holding `bit`.
  NetPositions(std::uint32_t count, value::FourStateBit bit);

  // Defined in the library, as everything here is: a unit holding a net reaches
  // positions only through what the library has already compiled.
  NetPositions(const NetPositions&);
  NetPositions(NetPositions&&) noexcept;
  auto operator=(const NetPositions&) -> NetPositions&;
  auto operator=(NetPositions&&) noexcept -> NetPositions&;
  ~NetPositions();

  [[nodiscard]] auto Count() const -> std::uint32_t;
  [[nodiscard]] auto View() const -> value::ConstIntegralView;
  [[nodiscard]] auto MutableView() -> value::IntegralView;

  // `count` of these positions from `from`.
  [[nodiscard]] auto Part(std::uint32_t from, std::uint32_t count) const
      -> NetPositions;

 private:
  std::uint32_t count_;
  // The value plane's words, then the unknown plane's.
  base::FixedArray<std::uint64_t, 2> words_;
};

// As many of a net value's positions as `positions` holds, from `offset`. A
// position the value does not reach reads x, which no connection names.
template <BitAddressed T>
void ReadPositions(
    const T& value, std::uint32_t offset, NetPositions& positions) {
  value::Extract(positions.MutableView(), value.View(), offset);
}

// A net value with the positions from `offset` replaced by `positions`, every
// other position left as it stands.
template <BitAddressed T>
void WritePositions(
    T& into, std::uint32_t offset, const NetPositions& positions) {
  value::Insert(into.MutableView(), positions.View(), offset);
}

class NetName;
class PhysicalNet;

// A value a net holds (LRM 6.7.1): an integral one, whose positions resolve
// wherever a connection put them, or an unpacked aggregate, which folds one
// contribution into another itself.
template <class T>
concept NetValue = BitAddressed<T> || value::NetResolvable<T>;

template <NetValue T>
class Driver;

// Where one declared net sits in a physical net: the net, and where among that
// net's own positions the ones the physical net covers begin. Every name in a
// physical net covers the whole of it, so how many positions it covers is the
// physical net's count, and a name reaching part of one reaches it as a
// separate physical net -- what a connection relates is positions, and
// positions related to two different things at two alignments are two physical
// nets. A net no connection reached is the one placement of its own physical
// net at offset zero.
struct NetPlacement {
  NetName* net{};
  std::uint32_t net_offset{};
};

// Some of a declared net's own positions and the physical net it reaches there.
// A net's reaches cover it exactly and in order, so a name whose positions were
// never split reaches one.
struct NetReach {
  std::shared_ptr<PhysicalNet> physical;
  std::uint32_t net_offset{};
  std::uint32_t count{};
};

// A declared net as the physical nets it reaches see it: which of them it
// reaches and over which of its own positions, the strengths its own
// contributions sit at, and the two things a resolution asks of it -- its
// contributions as they stand over some of its positions, and to show what
// those positions resolved to. What type the net's own value is does not
// appear, which is what lets nets of different types reach one physical net.
class NetName {
 public:
  NetName(const NetName&) = delete;
  auto operator=(const NetName&) -> NetName& = delete;
  NetName(NetName&&) = delete;
  auto operator=(NetName&&) -> NetName& = delete;

  // States that `count` of this name's positions from `here`, and the same
  // many of `other`'s from `there`, are one physical net.
  void Join(
      NetName& other, std::uint32_t here, std::uint32_t there,
      std::uint32_t count);

  // The physical nets this name reaches, covering its positions exactly and in
  // order.
  [[nodiscard]] auto Reaches() const -> const std::vector<NetReach>&;

  // Gives this name the physical net its declaration makes of its `count`
  // positions, which is the first it reaches: the scalar the net type
  // contributes at each of them and the strength it is held at, how
  // contributions of equal strength fold, and whether resolution keeps that
  // contribution current. Answers what the positions resolve to before any
  // driver attaches.
  auto Declare(
      support::StrengthLevel strength, value::FourStateBit own_fill,
      value::NetResolution fold, OwnContribution own_kind, std::uint32_t count)
      -> NetPositions;

  // Records that one of this name's own contributions sits at `level`.
  void Occupy(support::StrengthLevel level);

  // Fixes which names reach the positions this name reaches: no connection
  // joins or cuts them from here on.
  void SettleReaches();

  // Whether this name's own drivers are all that contributes to its positions:
  // they are one physical net no other name reaches, whose net type contributes
  // at high impedance, which takes no part (LRM 28.12.1), and keeps nothing it
  // was driven with (LRM 6.6.4).
  [[nodiscard]] auto OnlyOwnDriversContribute() const -> bool;

  // Whether a procedural continuous assignment in force covers the positions
  // this name reaches (LRM 10.6.2).
  [[nodiscard]] auto Overridden() const -> bool;

 protected:
  NetName();
  ~NetName();

 private:
  friend class PhysicalNet;

  // One reach, the name's own, until a connection states that some of these
  // positions are also some of another name's -- which cuts this net's reaches
  // where those positions end, since what the two share resolves together and
  // what it does not goes on resolving alone.
  std::vector<NetReach> reaches_;
  // Whether any of this net's own contributions sits at a given level, which a
  // resolution reads from every name reaching it. Recording it per name is what
  // leaves a driver attached after a connection needing nothing propagated, and
  // a connection made after one needing nothing rebuilt.
  OccupiedLevels occupied_{};

  // Folds each of this name's contributions at `level` into `group` under
  // `fold`, as it stands over the positions `group` holds, which begin at
  // `offset` among this name's own.
  virtual void FoldContributions(
      support::StrengthLevel level, value::NetResolution fold,
      std::uint32_t offset, NetPositions& group) const = 0;

  // Shows `positions` over this name's own from `offset`, publishing under
  // this name if what it shows moved (LRM 23.3.3.7).
  virtual void ShowPositions(
      RuntimeEffects& runtime, std::uint32_t offset,
      const NetPositions& positions) = 0;

  // Which of this net's reaches begins at `position`, cutting what it reaches
  // so that one does and so that it covers no more than `within` positions. A
  // connection names some of a net's own positions; what the net reaches there
  // was fixed by whatever connections came before, so the two are made to agree
  // here.
  auto ReachAt(std::uint32_t position, std::uint32_t within) -> std::size_t;

  // Cuts a physical net in two, `at` positions from its start, and gives every
  // name reaching it the two reaches that replace the one.
  static void CutPhysical(
      std::shared_ptr<PhysicalNet> physical, std::uint32_t at);

  // Makes one physical net of two that cover the same positions, leaving every
  // name of the second reaching the first.
  static void MergePhysical(
      std::shared_ptr<PhysicalNet> keep, std::shared_ptr<PhysicalNet> folded);
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
// A name keeps its own contributions, its own observers, and the value it
// shows -- a copy of what these positions produced over the ones it reaches, or
// its one driver's value where that decides it -- so reading a net reaches its
// own storage directly and never follows a pointer to get there.
class PhysicalNet {
 public:
  PhysicalNet(
      DriveContribution<NetPositions> own, value::FourStateBit own_fill,
      value::NetResolution fold, OwnContribution own_kind);

  PhysicalNet(const PhysicalNet&) = delete;
  auto operator=(const PhysicalNet&) -> PhysicalNet& = delete;
  PhysicalNet(PhysicalNet&&) = delete;
  auto operator=(PhysicalNet&&) -> PhysicalNet& = delete;
  ~PhysicalNet();

  // How many positions resolve together here.
  [[nodiscard]] auto Count() const -> std::uint32_t;

  // What these positions resolve to, from the contributions every name reaching
  // them has made. Between levels the stronger contribution determines every
  // position it drives and leaves the rest (LRM 28.12.1); within one level the
  // fold decides -- tri-state, wired-and, or wired-or (LRM 6.6.1 Table 6-2, LRM
  // 6.6.3 Tables 6-3 and 6-4, LRM 28.12.4). The net type's own contribution
  // takes part like any other, so positions nothing drives resolve to it, and
  // every level starts from the all-`z` value every fold treats as its
  // identity, so a level nothing occupies and positions with no drivers are not
  // cases of their own.
  [[nodiscard]] auto Resolve() const -> NetPositions;

  // Recomputes these positions and gives every name reaching them what they
  // produced over the positions it reaches, each publishing under its own name
  // if what it shows moved (LRM 23.3.3.7). One resolution serves every name,
  // however many a connection joined, because what resolves is this rather than
  // any of them.
  void Reresolve(RuntimeEffects& runtime);

  // Puts these positions under a procedural continuous assignment and takes
  // them back out (LRM 10.6.2). What a force overrides is the drivers of the
  // physical net, so it shows under every name reaching these positions. A
  // level is driven with a value, of which it keeps as many positions as
  // resolve here.
  auto BeginTakeover(support::TakeoverLevel level) -> std::uint32_t;
  auto DriveTakeover(
      support::TakeoverLevel level, std::uint32_t generation,
      const value::ConstIntegralView& forced) -> bool;
  auto EndTakeover(support::TakeoverLevel level) -> bool;

 private:
  friend class NetName;

  // Adds a name over these positions, at the offset among its own that the
  // first of them stands at. A name already here at that offset is already
  // saying this, which is what an alias repeated in two statements states.
  void Admit(const NetPlacement& placement);

  // These positions cut in two at `at`, which keeps the low part and hands back
  // the high one. Every name here covers the whole of these positions, so each
  // is placed in both halves, the high one at that name's own offset advanced
  // by the cut. A cut is what a connection reaching part of these positions
  // asks for: the part it reaches goes on to resolve with whatever it is
  // coupled to, and the rest of these positions is a resolution of its own.
  [[nodiscard]] auto Split(std::uint32_t at) -> std::shared_ptr<PhysicalNet>;

  DriveContribution<NetPositions> own_;
  // The scalar the net type contributes at every position (LRM 6.6.5, 6.6.6),
  // which is what a net type states; the contribution above is that scalar
  // taken to these positions. Two nets state the same net type when they agree
  // on this, on the strength it is held at, on the fold, and on whether
  // resolution keeps it current -- and on nothing about how many positions
  // either has, which is what lets a connection reach fewer than a whole net.
  value::FourStateBit own_fill_;
  value::NetResolution fold_;
  OwnContribution own_kind_;
  std::vector<NetPlacement> placements_;
  // Set once a name reaching these positions has settled what decides it,
  // after which no name joins them and they are not cut.
  bool names_settled_ = false;
  // The procedural continuous assignments these positions have been put under
  // (LRM 10.6.2), absent until the first one starts, so positions nobody forces
  // carry neither the storage nor the work of maintaining it.
  std::unique_ptr<Takeovers<NetPositions>> takeovers_;
};

// A net as a connection reaches it: the name its positions resolve under. A
// connection relates positions of two nets (LRM 23.3.3.7, 10.11), and positions
// are held the same way whatever type either net gives its own value, so this
// is what one net takes of another it is joined to without naming the other's
// type.
//
// Deriving from this has to leave the net's own address equal to the address
// of this part, for the reason an observable's does: generated code hands a
// net's address across a C boundary, where the pointer carries no type to
// adjust by.
class ConnectableNet : public Observable {
 public:
  ConnectableNet(const ConnectableNet&) = delete;
  auto operator=(const ConnectableNet&) -> ConnectableNet& = delete;
  ConnectableNet(ConnectableNet&&) = delete;
  auto operator=(ConnectableNet&&) -> ConnectableNet& = delete;

  [[nodiscard]] auto Name() const -> NetName&;

 protected:
  explicit ConnectableNet(NetName& name);
  ~ConnectableNet();

 private:
  NetName* name_;
};

// A net (LRM 6.5, 6.6): readable and observable like a `Var<T>` (it extends
// `Observable`, so a process can wait on it), but never written directly. A
// value reaches it through a driver, and what it shows is what its drivers
// determine -- the value of the one driver that decides it, or what every
// contribution reaching it resolves to -- published on a real change (LRM
// 9.4.2). The net owns where each driver's value lies; a `Driver<T>` names one
// by an index the net issued, so the storage stays the net's to reorganize.
//
// LRM 6.7.1 admits two kinds of data type, and a net of each is a class of its
// own: an integral type, whose positions a connection may name some of, and an
// unpacked aggregate, which no connection names any position of.
template <NetValue T>
class ResolvedNet;

// A net of an integral type: a name for some positions, the contributions the
// sources in its own unit make to them, and the value it shows -- what those
// positions last resolved to, or what its one driver last gave. A procedural
// continuous assignment may override what the drivers determine (LRM 10.6.2).
//
// What resolves is not the net. A bidirectional connection and an `alias` each
// state that positions across several nets are the same physical net
// (LRM 23.3.3.7, 10.11), so what resolves is that and every name reaching it
// shows what it produced. A net no connection reached is the one name of a
// physical net covering it exactly, which is the same walk over one.
template <NetValue T>
  requires BitAddressed<T>
class ResolvedNet<T> : public ConnectableNet {
 public:
  ResolvedNet();

  // Fixes what the net's declaration gives it -- how many positions its
  // declared type has, which the type laying its value out does not say where
  // it lays out a narrower one, and what its declared net type states: which
  // truth table resolves contributions of equal strength, and the contribution
  // the net type itself makes, as the scalar it shows where nothing drives it
  // and the strength it holds that scalar at (LRM 6.7.1). The net is therefore
  // a readable observable of its declared count before any driver attaches,
  // showing what its net type's own contribution resolves to. Installing twice
  // is a lowering defect.
  //
  // One entry per resolution: tri-state for `wire` / `tri` (LRM 6.6.1 Table
  // 6-2), wired-and for `wand` / `triand` and wired-or for `wor` / `trior` (LRM
  // 6.6.3 Tables 6-3 and 6-4), and one that resolves tri-state and leaves its
  // own contribution holding what the drivers last decided, which is how a net
  // stores a value (LRM 6.6.4).
  void InitializeTriState(
      std::int64_t count, std::int64_t fill, std::int64_t strength) {
    Install(
        count, fill, strength, value::NetResolution::kTriState,
        OwnContribution::kFixed);
  }
  void InitializeWiredAnd(
      std::int64_t count, std::int64_t fill, std::int64_t strength) {
    Install(
        count, fill, strength, value::NetResolution::kWiredAnd,
        OwnContribution::kFixed);
  }
  void InitializeWiredOr(
      std::int64_t count, std::int64_t fill, std::int64_t strength) {
    Install(
        count, fill, strength, value::NetResolution::kWiredOr,
        OwnContribution::kFixed);
  }
  void InitializeRetaining(
      std::int64_t count, std::int64_t fill, std::int64_t strength) {
    Install(
        count, fill, strength, value::NetResolution::kTriState,
        OwnContribution::kRetained);
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet();

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
  auto BeginTakeover(std::int64_t level) -> std::int64_t {
    return std::int64_t{
        WholePhysicalNet().BeginTakeover(TakeoverLevelOf(level))};
  }

  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& value) -> bool {
    return DriveTakeoverFromView(level, generation, value.AsWords().View());
  }

  // The same of a value handed as its bytes, as wide as what the net holds.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes) -> bool
    requires value::TakesBytesInPlace<T>
  {
    return DriveTakeoverFromView(level, generation, resolved_.ReadAt(bytes));
  }

  void EndTakeover(std::int64_t level) {
    PhysicalNet& physical = WholePhysicalNet();
    if (!physical.EndTakeover(TakeoverLevelOf(level))) {
      return;
    }
    RuntimeEffects& runtime = current_runtime();
    if (name_.Overridden() || !DecidedByOneDriver()) {
      physical.Reresolve(runtime);
      return;
    }
    // Nothing covers a net one driver decides any longer, so it shows what
    // that driver produced meanwhile (LRM 10.6.2), where an override ever
    // gave a value at all.
    if (const std::unique_ptr<T> kept = std::move(kept_beneath_)) {
      ShowDriverValue(
          runtime, *this, resolved_,
          [&] { return resolved_.IsBitIdentical(*kept); },
          [&] { resolved_ = *kept; });
    }
  }

  // States that `count` positions of this net from `here`, and the same many of
  // `other` from `there`, are one physical net (LRM 23.3.3.7, 10.11). Every
  // driver reaching either side becomes a contribution to the same fold, at the
  // strength it drives at, which is what makes the connection
  // non-strength-reducing (LRM 23.3.3); no net's resolved value is ever an
  // input to another's resolution. Stating the same positions twice over is a
  // connection restating what another established, which is a shape a design
  // writes rather than a mistake.
  //
  // One physical net has one net type, so the two have to state the same one.
  // Where they differ the standard names a dominating type per pair of nets
  // (LRM 23.3.3.7 Table 23-1), which does not extend to the set a chain of
  // connections joins -- the relation it tabulates is not transitive. The
  // report names what the design wrote rather than which net type won, because
  // which net type a net is, is not something below the net's declaration knows
  // or should learn.
  //
  // The other net may be of another integral type, as nets of two widths
  // joined over some of their positions are: what the two share is positions,
  // which carry no type.
  void Join(
      ConnectableNet* other, std::int64_t here, std::int64_t there,
      std::int64_t count) {
    name_.Join(
        other->Name(), static_cast<std::uint32_t>(here),
        static_cast<std::uint32_t>(there), static_cast<std::uint32_t>(count));
  }

  // Attaches a new driver at the strength its source drives at and returns its
  // handle. Its contribution starts at the non-driving one, so a driver that
  // has not yet driven leaves the resolution exactly as it was -- attaching is
  // not itself an act of driving. The contribution list only grows, so an index
  // into it is a stable identity. The handle is the net's own, so a source that
  // can hold one by value copies it out of the reference and one that cannot
  // keeps the reference itself.
  auto AttachDriver(std::int64_t strength) -> Driver<T>&;

 private:
  friend class Driver<T>;

  // This net as the physical nets it reaches see it. It is a part of the net
  // rather than something the net is, so the net stays the one thing a wait
  // registers on, at the address its storage has.
  class Name final : public NetName {
   public:
    explicit Name(ResolvedNet& net) : net_(&net) {
    }

   private:
    void FoldContributions(
        support::StrengthLevel level, value::NetResolution fold,
        std::uint32_t offset, NetPositions& group) const override {
      NetPositions contribution(
          group.Count(), value::FourStateBit::kHighImpedance);
      for (const DriveContribution<T>& driver : net_->contributions_) {
        if (driver.strength == level) {
          ReadPositions(driver.value, offset, contribution);
          value::Resolve(
              group.MutableView(), group.View(), contribution.View(), fold);
        }
      }
    }

    // The positions are written into the net's value where they lie, and what
    // is kept of them from before says which moved (LRM 9.4.2).
    void ShowPositions(
        RuntimeEffects& runtime, std::uint32_t offset,
        const NetPositions& positions) override {
      KeptPart<T> kept(
          net_->resolved_,
          value::BitPositions{.lsb = offset, .width = positions.Count()});
      WritePositions(net_->resolved_, offset, positions);
      if (const std::optional<Change> change = kept.ChangeTo(net_->resolved_)) {
        runtime.WakeParkedOn(*net_, PartPath{}, *change);
      }
    }

    ResolvedNet* net_;
  };

  void Install(
      std::int64_t count, std::int64_t fill, std::int64_t strength,
      value::NetResolution resolution, OwnContribution own_kind) {
    if (count_ != 0) {
      throw InternalError(
          "ResolvedNet: the net's declared type is already fixed");
    }
    count_ = static_cast<std::uint32_t>(count);
    nondriving_ = FilledOver<T>(count_, value::FourStateBit::kHighImpedance);
    resolved_ = nondriving_;
    WritePositions(
        resolved_, 0,
        name_.Declare(
            static_cast<support::StrengthLevel>(strength), NetFillOf(fill),
            resolution, own_kind, count_));
  }

  auto DriveTakeoverFromView(
      std::int64_t level, std::int64_t generation,
      const value::ConstIntegralView& forced) -> bool {
    PhysicalNet& physical = WholePhysicalNet();
    if (!physical.DriveTakeover(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), forced)) {
      return false;
    }
    // The first override to cover a net one driver decides finds in it what
    // that driver last produced, which is kept beneath for the net to show
    // again once nothing covers it (LRM 10.6.2).
    if (DecidedByOneDriver() && kept_beneath_ == nullptr) {
      kept_beneath_ = std::make_unique<T>(resolved_);
    }
    physical.Reresolve(current_runtime());
    return true;
  }

  // The physical net this name reaches, where it reaches one covering the whole
  // of it. An operation written on a name rather than on some of its positions
  // needs that, because a name reaching several has no way to say which it
  // meant.
  [[nodiscard]] auto WholePhysicalNet() -> PhysicalNet& {
    const std::vector<NetReach>& reaches = name_.Reaches();
    if (reaches.size() != 1 || reaches.front().count != count_) {
      throw SimulationError(
          "a procedural continuous assignment names a net that a connection "
          "reaches over part of one resolution, so what it overrides is part "
          "of a physical net, which is not yet supported (LRM 10.6.2)");
    }
    return *reaches.front().physical;
  }

  // What decides this net, settled the first time it is asked.
  [[nodiscard]] auto DecidedByOneDriver() -> bool {
    switch (decision_) {
      case NetDecision::kOneDriver:
        return true;
      case NetDecision::kResolution:
        return false;
      case NetDecision::kUnsettled:
        break;
    }
    name_.SettleReaches();
    const bool one_driver = name_.OnlyOwnDriversContribute() &&
                            contributions_.size() == 1 &&
                            LevelBit(contributions_.front().strength) != 0U;
    decision_ = one_driver ? NetDecision::kOneDriver : NetDecision::kResolution;
    return one_driver;
  }

  // Where a write of the driver `index` names lands.
  [[nodiscard]] auto LandingOf(std::size_t index) -> DriverLanding<T> {
    if (!DecidedByOneDriver()) {
      return Contributing<T>{.contribution = &ContributionOf(index).value};
    }
    if (kept_beneath_ != nullptr) {
      return KeptBeneath<T>{.kept = kept_beneath_.get()};
    }
    return Showing<T>{.shown = &resolved_, .watched = this->HasMembers()};
  }

  // A driver gives its whole value.
  void SetDriver(RuntimeEffects& runtime, std::size_t index, const T& value) {
    std::visit(
        Overloaded{
            [&](const Contributing<T>& to) {
              *to.contribution = value;
              ReresolveJoint(runtime, Change::Whole());
            },
            [&](const Showing<T>& to) {
              ShowDriverValue(
                  runtime, *this, *to.shown,
                  [&] { return to.shown->IsBitIdentical(value); },
                  [&] { *to.shown = value; });
            },
            [&](const KeptBeneath<T>& to) { *to.kept = value; }},
        LandingOf(index));
  }

  // The same of a value handed as its bytes, as wide as the driver's value,
  // taken into the words that value has. A contribution already holding them
  // moves no resolution.
  void SetDriverBytes(
      RuntimeEffects& runtime, std::size_t index, const void* bytes)
    requires value::TakesBytesInPlace<T>
  {
    std::visit(
        Overloaded{
            [&](const Contributing<T>& to) {
              if (to.contribution->HoldsBytes(bytes)) {
                return;
              }
              to.contribution->TakeBytes(bytes);
              ReresolveJoint(runtime, Change::Whole());
            },
            [&](const Showing<T>& to) {
              ShowDriverValue(
                  runtime, *this, *to.shown,
                  [&] { return to.shown->HoldsBytes(bytes); },
                  [&] { to.shown->TakeBytes(bytes); });
            },
            [&](const KeptBeneath<T>& to) { to.kept->TakeBytes(bytes); }},
        LandingOf(index));
  }

  // A write into part of a driver's value is over, having changed it. What the
  // change says of a contribution's bits holds only where the write landed on
  // the whole of it.
  void DriverWrote(
      RuntimeEffects& runtime, std::size_t index, const PartPath& reached,
      const Change& change) {
    std::visit(
        Overloaded{
            [&](const Contributing<T>&) {
              ReresolveJoint(
                  runtime, reached.IsWhole() ? change : Change::Whole());
            },
            [&](const Showing<T>&) {
              runtime.WakeParkedOn(*this, reached, change);
            },
            [](const KeptBeneath<T>&) {}},
        LandingOf(index));
  }

  // A driver writes wherever this net's own positions are, so every resolution
  // any of them takes part in recomputes -- except a reach whose positions
  // `change` shows unchanged, where a contribution that did not move leaves
  // the resolution over it where it was. A net no connection split has one
  // reach, which is the same walk over one.
  void ReresolveJoint(RuntimeEffects& runtime, const Change& change) {
    for (const NetReach& reach : name_.Reaches()) {
      if (change.KnownUnchanged(
              {.lsb = reach.net_offset, .width = reach.count})) {
        continue;
      }
      reach.physical->Reresolve(runtime);
    }
  }

  // The contribution a driver names. Every driver reaches its own through the
  // index the net issued it, so an index the net never issued is a lowering
  // defect rather than a value to answer.
  [[nodiscard]] auto ContributionOf(std::size_t index)
      -> DriveContribution<T>& {
    if (index >= contributions_.size()) {
      throw InternalError("ResolvedNet: driver names no attached contribution");
    }
    return contributions_[index];
  }

  T resolved_{};
  T nondriving_{};
  // How many positions the declared type has, none until the net is
  // installed.
  std::uint32_t count_ = 0;
  NetDecision decision_ = NetDecision::kUnsettled;
  // What the one driver deciding this net produced while a procedural
  // continuous assignment covers the net (LRM 10.6.2), absent otherwise.
  std::unique_ptr<T> kept_beneath_;
  Name name_{*this};
  std::vector<DriveContribution<T>> contributions_;
  // The handles this net has issued. They are the net's rather than each
  // source's so that a source reaching its driver by address holds nothing that
  // points into the contributions above: those stay the net's to reorganize,
  // and what a reorganization would have to rewrite is these, which it can.
  // Growth therefore must not move what has already been handed out.
  std::deque<Driver<T>> drivers_;
};

// A net of an unpacked aggregate (LRM 6.7.1): a structure, a union, or a
// fixed-size array, every element of which is a valid net data type. It
// resolves element by element and bit by bit under the same tables and
// strengths an integral net does, which the aggregate's own operations carry
// out. It keeps no positions another net could reach, so what resolves is the
// net itself.
template <NetValue T>
  requires(!BitAddressed<T>)
class ResolvedNet<T> : public Observable {
 public:
  ResolvedNet();

  // Fixes what the net's declaration gives it: the declared type, from a value
  // carrying it, the scalar the net type contributes in every bit and the
  // strength it holds it at (LRM 6.7.1), and which truth table resolves
  // contributions of equal strength -- one entry per table, and one that
  // resolves tri-state and keeps what the drivers last decided (LRM 6.6.4).
  // Installing twice is a lowering defect.
  void InitializeTriState(
      const T& prototype, std::int64_t fill, std::int64_t strength) {
    Install(
        prototype, fill, strength, value::NetResolution::kTriState,
        OwnContribution::kFixed);
  }
  void InitializeWiredAnd(
      const T& prototype, std::int64_t fill, std::int64_t strength) {
    Install(
        prototype, fill, strength, value::NetResolution::kWiredAnd,
        OwnContribution::kFixed);
  }
  void InitializeWiredOr(
      const T& prototype, std::int64_t fill, std::int64_t strength) {
    Install(
        prototype, fill, strength, value::NetResolution::kWiredOr,
        OwnContribution::kFixed);
  }
  void InitializeRetaining(
      const T& prototype, std::int64_t fill, std::int64_t strength) {
    Install(
        prototype, fill, strength, value::NetResolution::kTriState,
        OwnContribution::kRetained);
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet();

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return resolved_;
  }

  // Puts the net under a procedural continuous assignment and takes it back
  // out (LRM 10.6.2). A `force` overrides every driver, so what it changes is
  // what the net shows, never the contributions, which are what the net
  // answers with again once released.
  auto BeginTakeover(std::int64_t level) -> std::int64_t {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<Takeovers<T>>();
    }
    return std::int64_t{takeovers_->Begin(TakeoverLevelOf(level))};
  }

  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& value) -> bool {
    if (takeovers_ == nullptr) {
      return false;
    }
    const bool was_covered = Overridden();
    if (!takeovers_->Drive(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), value)) {
      return false;
    }
    // The first override to cover a net one driver decides finds in it what
    // that driver last produced, which is kept beneath for the net to show
    // again once nothing covers it (LRM 10.6.2).
    if (!was_covered && DecidedByOneDriver()) {
      takeovers_->KeepBeneath(resolved_);
    }
    Reresolve(current_runtime());
    return true;
  }

  void EndTakeover(std::int64_t level) {
    if (takeovers_ == nullptr) {
      return;
    }
    takeovers_->End(TakeoverLevelOf(level));
    RuntimeEffects& runtime = current_runtime();
    if (Overridden() || !DecidedByOneDriver()) {
      Reresolve(runtime);
      return;
    }
    // Nothing covers a net one driver decides any longer, so it shows what
    // that driver produced meanwhile (LRM 10.6.2), where an override ever
    // gave a value at all.
    if (const std::optional<T> kept = takeovers_->TakeBeneath()) {
      ShowDriverValue(
          runtime, *this, resolved_,
          [&] { return resolved_.IsBitIdentical(*kept); },
          [&] { resolved_ = *kept; });
    }
  }

  // Attaches a new driver at the strength its source drives at and returns its
  // handle, which is the net's own. Its contribution starts at the non-driving
  // one, so attaching is not itself an act of driving.
  auto AttachDriver(std::int64_t strength) -> Driver<T>&;

 private:
  friend class Driver<T>;

  void Install(
      const T& prototype, std::int64_t fill, std::int64_t strength,
      value::NetResolution resolution, OwnContribution own_kind) {
    if constexpr (RepresentedAtRunTime<T>) {
      if (!resolved_.IsUninitialized()) {
        throw InternalError(
            "ResolvedNet: the net's declared type is already fixed");
      }
    }
    nondriving_ = T::FilledLike(
        prototype, value::Logic::Filled(value::FourStateBit::kHighImpedance));
    own_ = DriveContribution<T>{
        .value =
            T::FilledLike(prototype, value::Logic::Filled(NetFillOf(fill))),
        .strength = static_cast<support::StrengthLevel>(strength)};
    fold_ = resolution;
    own_kind_ = own_kind;
    resolved_ = Resolve();
  }

  // What the net resolves to: between levels the stronger contribution
  // determines every bit it drives (LRM 28.12.1), and within one the net
  // type's table folds them (LRM 6.6.1, 6.6.3), each level starting from the
  // all-`z` value every fold treats as its identity.
  [[nodiscard]] auto Resolve() const -> T {
    const OccupiedLevels occupied = occupied_ | LevelBit(own_.strength);
    T resolved = nondriving_;
    for (std::size_t level = support::kStrengthLevelCount; level-- > 0;) {
      const auto at = static_cast<support::StrengthLevel>(level);
      if ((occupied & LevelBit(at)) == 0U) {
        continue;
      }
      T group = own_.strength == at ? own_.value : nondriving_;
      for (const DriveContribution<T>& driver : contributions_) {
        if (driver.strength == at) {
          group = value::Resolve(fold_, group, driver.value);
        }
      }
      resolved = resolved.Dominating(group);
    }
    return resolved;
  }

  // Recomputes the net and publishes if what it shows moved. A net type that
  // stores a value takes the resolution it just took part in as its own
  // contribution (LRM 6.6.4), and a takeover displaces what the net shows and
  // not what it holds (LRM 10.6.2).
  void Reresolve(RuntimeEffects& runtime) {
    const T resolved = Resolve();
    if (own_kind_ == OwnContribution::kRetained) {
      own_.value = resolved;
    }
    const T* forced = takeovers_ == nullptr ? nullptr : takeovers_->Highest();
    PublishIfChanged(runtime, forced == nullptr ? resolved : *forced);
  }

  // What decides this net, settled the first time it is asked. No connection
  // reaches part of an aggregate, so it is one driver that decides it wherever
  // there is exactly one, driving, and the net type contributes at high
  // impedance, which takes no part (LRM 28.12.1), and keeps nothing it was
  // driven with (LRM 6.6.4).
  [[nodiscard]] auto DecidedByOneDriver() -> bool {
    switch (decision_) {
      case NetDecision::kOneDriver:
        return true;
      case NetDecision::kResolution:
        return false;
      case NetDecision::kUnsettled:
        break;
    }
    const bool one_driver = LevelBit(own_.strength) == 0U &&
                            own_kind_ == OwnContribution::kFixed &&
                            contributions_.size() == 1 &&
                            LevelBit(contributions_.front().strength) != 0U;
    decision_ = one_driver ? NetDecision::kOneDriver : NetDecision::kResolution;
    return one_driver;
  }

  // Whether a procedural continuous assignment in force covers the net (LRM
  // 10.6.2).
  [[nodiscard]] auto Overridden() const -> bool {
    return takeovers_ != nullptr && takeovers_->Highest() != nullptr;
  }

  // Where a write of the driver `index` names lands.
  [[nodiscard]] auto LandingOf(std::size_t index) -> DriverLanding<T> {
    if (!DecidedByOneDriver()) {
      return Contributing<T>{.contribution = &ContributionOf(index).value};
    }
    if (T* kept = takeovers_ == nullptr ? nullptr : takeovers_->Beneath()) {
      return KeptBeneath<T>{.kept = kept};
    }
    return Showing<T>{.shown = &resolved_, .watched = this->HasMembers()};
  }

  // A driver gives its whole value.
  void SetDriver(RuntimeEffects& runtime, std::size_t index, const T& value) {
    std::visit(
        Overloaded{
            [&](const Contributing<T>& to) {
              *to.contribution = value;
              Reresolve(runtime);
            },
            [&](const Showing<T>& to) {
              ShowDriverValue(
                  runtime, *this, *to.shown,
                  [&] { return to.shown->IsBitIdentical(value); },
                  [&] { *to.shown = value; });
            },
            [&](const KeptBeneath<T>& to) { *to.kept = value; }},
        LandingOf(index));
  }

  // A write into part of a driver's value is over, having changed it. A
  // contribution resolves again unless the write landed on the whole of it and
  // is known to have moved nothing.
  void DriverWrote(
      RuntimeEffects& runtime, std::size_t index, const PartPath& reached,
      const Change& change) {
    std::visit(
        Overloaded{
            [&](const Contributing<T>&) {
              if (!reached.IsWhole() || !change.KnownUnchanged({})) {
                Reresolve(runtime);
              }
            },
            [&](const Showing<T>&) {
              runtime.WakeParkedOn(*this, reached, change);
            },
            [](const KeptBeneath<T>&) {}},
        LandingOf(index));
  }

  // The contribution a driver names, through the index the net issued it.
  [[nodiscard]] auto ContributionOf(std::size_t index)
      -> DriveContribution<T>& {
    if (index >= contributions_.size()) {
      throw InternalError("ResolvedNet: driver names no attached contribution");
    }
    return contributions_[index];
  }

  // Stores the resolved value and wakes subscribers only when it actually
  // changed (LRM 9.4.2).
  void PublishIfChanged(RuntimeEffects& runtime, const T& next) {
    if (resolved_.IsBitIdentical(next)) {
      return;
    }
    resolved_ = next;
    runtime.WakeParkedOn(*this, PartPath{}, Change::Whole());
  }

  T resolved_{};
  T nondriving_{};
  // The contribution the net type makes to its own resolution, the table
  // contributions of equal strength fold under, and whether resolution keeps
  // that contribution current (LRM 6.7.1, 6.6.4).
  DriveContribution<T> own_{};
  value::NetResolution fold_{};
  OwnContribution own_kind_{};
  // The procedural continuous assignments the net has been put under (LRM
  // 10.6.2), absent until the first one starts.
  std::unique_ptr<Takeovers<T>> takeovers_;
  // Whether any contribution of a driver sits at a given level.
  OccupiedLevels occupied_{};
  NetDecision decision_ = NetDecision::kUnsettled;
  std::vector<DriveContribution<T>> contributions_;
  // The handles this net has issued, which growth must not move.
  std::deque<Driver<T>> drivers_;
};

// Defaulted here rather than where they are declared, for the reason a variable
// cell's are: one defaulted on its first declaration is defined by every unit
// that holds a net.
template <NetValue T>
  requires BitAddressed<T>
ResolvedNet<T>::ResolvedNet() : ConnectableNet(name_) {
}

template <NetValue T>
  requires BitAddressed<T>
ResolvedNet<T>::~ResolvedNet() = default;

template <NetValue T>
  requires(!BitAddressed<T>)
ResolvedNet<T>::ResolvedNet() = default;

template <NetValue T>
  requires(!BitAddressed<T>)
ResolvedNet<T>::~ResolvedNet() = default;

// The drive capability for a net: a handle to one driver of a `ResolvedNet`.
// The net issues it and owns it, so a source may hold it either by value or by
// address; a source's slot starts unbound and the net binds it when it
// attaches, during Resolve. A driver's value is given only through this
// handle, and where it lies is the net's to say (LRM 6.5).
//
// A source that drives only part of the net opens the same partial-write
// bracket a variable cell offers: the write lands in this driver's own value
// and reaches only the positions it names, so the ones it does not drive stay
// at high impedance, deferring to whoever does drive them.
template <NetValue T>
class Driver {
 public:
  using ValueType = T;

  Driver();
  Driver(ResolvedNet<T>& net, std::size_t contribution)
      : net_(&net), contribution_(contribution) {
  }

  // Gives this driver's whole value, which every name it reaches publishes on a
  // real change. It carries the capability family's store name because a store
  // through a handle reaches whatever that handle addresses, and what this one
  // addresses is a driver's value.
  void Set(const T& value) const {
    Net().SetDriver(current_runtime(), contribution_, value);
  }

  // The same of a value handed as its bytes, as wide as the driver's value.
  void SetBytes(const void* bytes) const
    requires value::TakesBytesInPlace<T>
  {
    Net().SetDriverBytes(current_runtime(), contribution_, bytes);
  }

  // This driver's own value as it currently stands.
  [[nodiscard]] auto Get() const -> const T& {
    return DriverValueAt(Landing());
  }

  [[nodiscard]] auto Mutate() const -> ScopedMutation<Driver> {
    return ScopedMutation<Driver>{*this};
  }

  // The `MutationSink` surface, each answer read off where this driver's
  // writes land.
  [[nodiscard]] auto AdmitsWrite() const -> bool {
    return std::visit(
        Overloaded{
            [](const Contributing<T>&) { return true; },
            [](const Showing<T>&) { return true; },
            [](const KeptBeneath<T>&) { return false; }},
        Landing());
  }
  [[nodiscard]] auto DisplacedStorage() const -> T* {
    return std::visit(
        Overloaded{
            [](const Contributing<T>&) -> T* { return nullptr; },
            [](const Showing<T>&) -> T* { return nullptr; },
            [](const KeptBeneath<T>& to) -> T* { return to.kept; }},
        Landing());
  }
  [[nodiscard]] auto MutationStorage() const -> T& {
    return DriverValueAt(Landing());
  }
  [[nodiscard]] auto Watched() const -> bool {
    return std::visit(
        Overloaded{
            [](const Contributing<T>&) { return true; },
            [](const Showing<T>& to) { return to.watched; },
            [](const KeptBeneath<T>&) { return false; }},
        Landing());
  }
  void PublishTransition(const PartPath& reached, const Change& change) const {
    Net().DriverWrote(current_runtime(), contribution_, reached, change);
  }

 private:
  [[nodiscard]] auto Landing() const -> DriverLanding<T> {
    return Net().LandingOf(contribution_);
  }

  [[nodiscard]] auto Net() const -> ResolvedNet<T>& {
    if (net_ == nullptr) {
      throw InternalError("Driver: driver is not attached");
    }
    return *net_;
  }

  ResolvedNet<T>* net_ = nullptr;
  std::size_t contribution_ = 0;
};

// Defaulted here rather than where it is declared, for the reason a net's own
// constructor is: a source holding a driver slot constructs one.
template <NetValue T>
Driver<T>::Driver() = default;

template <NetValue T>
  requires BitAddressed<T>
auto ResolvedNet<T>::AttachDriver(std::int64_t strength) -> Driver<T>& {
  RequireUnsettled(decision_);
  const auto level = static_cast<support::StrengthLevel>(strength);
  contributions_.push_back(
      DriveContribution<T>{.value = nondriving_, .strength = level});
  name_.Occupy(level);
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

template <NetValue T>
  requires(!BitAddressed<T>)
auto ResolvedNet<T>::AttachDriver(std::int64_t strength) -> Driver<T>& {
  RequireUnsettled(decision_);
  const auto level = static_cast<support::StrengthLevel>(strength);
  contributions_.push_back(
      DriveContribution<T>{.value = nondriving_, .strength = level});
  occupied_ |= LevelBit(level);
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

static_assert(MutationSink<Driver<value::Logic>>);

}  // namespace lyra::runtime
