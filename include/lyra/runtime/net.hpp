#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <memory>
#include <optional>
#include <vector>

#include "lyra/base/fixed_array.hpp"
#include "lyra/base/internal_error.hpp"
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
    typename T::Words words;
    value::FillScalar(words.Write(), count, fill);
    return T::FromWords(words);
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
  [[nodiscard]] auto Read() const -> value::ConstPlanes;
  [[nodiscard]] auto Write() -> value::Planes;

  // `count` of these positions from `from`.
  [[nodiscard]] auto Part(std::uint32_t from, std::uint32_t count) const
      -> NetPositions;

 private:
  std::uint32_t count_;
  // The value plane's words, then the unknown plane's.
  base::FixedArray<std::uint64_t, 2> words_;
};

// Rewrites a bit-addressed value through `write`, which is handed the value's
// planes and how many positions they hold.
template <BitAddressed T, class Write>
void RewritePlanes(T& bits, Write write) {
  if constexpr (value::IntegralValue<T>) {
    typename T::Words words = bits.Load();
    write(words.Write(), std::uint64_t{T::kWidth});
    bits = T::FromWords(words);
  } else {
    write(bits.Write(), bits.Width());
  }
}

// As many of a net value's positions as `positions` holds, from `offset`. A
// position the value does not reach reads x, which no connection names.
template <BitAddressed T>
void ReadPositions(
    const T& value, std::uint32_t offset, NetPositions& positions) {
  ReadPlanes(value, [&](value::ConstPlanes planes, std::uint64_t width) {
    value::Extract(positions.Write(), positions.Count(), planes, width, offset);
  });
}

// A net value with the positions from `offset` replaced by `positions`, every
// other position left as it stands.
template <BitAddressed T>
void WritePositions(
    T& into, std::uint32_t offset, const NetPositions& positions) {
  RewritePlanes(into, [&](value::Planes planes, std::uint64_t width) {
    value::Insert(planes, width, positions.Read(), positions.Count(), offset);
  });
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
// A name keeps its own contributions, its own observers, and a copy of what
// these positions produced over the ones it reaches, so reading a net reaches
// its own storage directly and never follows a pointer to get there.
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
  // level is driven with the planes of a value of `width` bits, of which it
  // keeps as many positions as resolve here.
  auto BeginTakeover(support::TakeoverLevel level) -> std::uint32_t;
  auto DriveTakeover(
      support::TakeoverLevel level, std::uint32_t generation,
      value::ConstPlanes forced, std::uint64_t width) -> bool;
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
// value reaches it by a driver updating its own contribution, and what it shows
// is what every contribution reaching it resolves to, published on a real
// change (LRM 9.4.2). The net owns the contribution storage; a `Driver<T>`
// names one contribution by an index the net issued, so the storage stays the
// net's to reorganize.
//
// LRM 6.7.1 admits two kinds of data type, and a net of each is a class of its
// own: an integral type, whose positions a connection may name some of, and an
// unpacked aggregate, which no connection names any position of.
template <NetValue T>
class ResolvedNet;

// A net of an integral type: a name for some positions, the contributions the
// sources in its own unit make to them, and what those positions last resolved
// to. A procedural continuous assignment may override what the contributions
// resolve to (LRM 10.6.2).
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
  // and its value at that point comes from the same resolution every later
  // value comes from. Installing twice is a lowering defect.
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
    return ReadPlanes(
        value, [&](value::ConstPlanes forced, std::uint64_t width) {
          return DriveTakeoverPlanes(level, generation, forced, width);
        });
  }

  // The same of a value handed as its bytes, as wide as what the net holds.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes) -> bool
    requires value::TakesBytesInPlace<T>
  {
    return DriveTakeoverPlanes(
        level, generation, resolved_.PlanesAt(bytes), resolved_.Width());
  }

  void EndTakeover(std::int64_t level) {
    PhysicalNet& physical = WholePhysicalNet();
    if (physical.EndTakeover(TakeoverLevelOf(level))) {
      physical.Reresolve(current_runtime());
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
              group.Write(), group.Read(), contribution.Read(), fold);
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
        runtime.WakeParkedOn(net_->Members(), *change);
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

  auto DriveTakeoverPlanes(
      std::int64_t level, std::int64_t generation, value::ConstPlanes forced,
      std::uint64_t width) -> bool {
    PhysicalNet& physical = WholePhysicalNet();
    if (!physical.DriveTakeover(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), forced,
            width)) {
      return false;
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

  void UpdateContribution(
      RuntimeEffects& runtime, std::size_t index, const T& value) {
    ContributionOf(index).value = value;
    ReresolveJoint(runtime, Change::Whole());
  }

  // The same of a value handed as its bytes, as wide as the contribution,
  // taken into the words the contribution has. Bytes it already holds move no
  // resolution.
  void UpdateContributionBytes(
      RuntimeEffects& runtime, std::size_t index, const void* bytes)
    requires value::TakesBytesInPlace<T>
  {
    T& contribution = ContributionOf(index).value;
    if (contribution.HoldsBytes(bytes)) {
      return;
    }
    contribution.TakeBytes(bytes);
    ReresolveJoint(runtime, Change::Whole());
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

  T resolved_{};
  T nondriving_{};
  // How many positions the declared type has, none until the net is
  // installed.
  std::uint32_t count_ = 0;
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
    if (takeovers_ == nullptr ||
        !takeovers_->Drive(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), value)) {
      return false;
    }
    Reresolve(current_runtime());
    return true;
  }

  void EndTakeover(std::int64_t level) {
    if (takeovers_ == nullptr) {
      return;
    }
    takeovers_->End(TakeoverLevelOf(level));
    Reresolve(current_runtime());
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

  void UpdateContribution(
      RuntimeEffects& runtime, std::size_t index, const T& value) {
    ContributionOf(index).value = value;
    Reresolve(runtime);
  }

  // A write into part of a contribution resolves the net again unless it is
  // known to have moved nothing.
  void ReresolveJoint(RuntimeEffects& runtime, const Change& change) {
    if (change.KnownUnchanged({})) {
      return;
    }
    Reresolve(runtime);
  }

  // The contribution a driver names, through the index the net issued it. A
  // driver reads its contribution back to write part of it without disturbing
  // the rest (LRM 6.6.1).
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
    runtime.WakeParkedOn(this->Members(), Change::Whole());
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
template <NetValue T>
class Driver {
 public:
  using ValueType = T;

  Driver();
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

  // The same of a value handed as its bytes, as wide as the contribution.
  void SetBytes(const void* bytes) const
    requires value::TakesBytesInPlace<T>
  {
    Net().UpdateContributionBytes(current_runtime(), contribution_, bytes);
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
  // the re-resolution that follows a write to it. The net always reads what a
  // write did, but only positions whose contribution moved move the resolution
  // over them, so those are the ones that resolve again. A contribution goes on
  // being updated while its net is forced, since the net takes what its drivers
  // determine the moment it is released (LRM 10.6.2), so a write always lands.
  [[nodiscard]] static auto AdmitsWrite() -> bool {
    return true;
  }
  [[nodiscard]] static auto DisplacedStorage() -> T* {
    return nullptr;
  }
  [[nodiscard]] auto MutationStorage() const -> T& {
    return Net().ContributionOf(contribution_).value;
  }
  [[nodiscard]] static auto Watched() -> bool {
    return true;
  }
  void PublishTransition(const Change& change) const {
    Net().ReresolveJoint(current_runtime(), change);
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

// Defaulted here rather than where it is declared, for the reason a net's own
// constructor is: a source holding a driver slot constructs one.
template <NetValue T>
Driver<T>::Driver() = default;

template <NetValue T>
  requires BitAddressed<T>
auto ResolvedNet<T>::AttachDriver(std::int64_t strength) -> Driver<T>& {
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
  const auto level = static_cast<support::StrengthLevel>(strength);
  contributions_.push_back(
      DriveContribution<T>{.value = nondriving_, .strength = level});
  occupied_ |= LevelBit(level);
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

static_assert(MutationSink<Driver<value::Logic>>);

}  // namespace lyra::runtime
