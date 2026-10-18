#pragma once

#include <concepts>
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
#include "lyra/runtime/bound_members.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/small_box.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// `into` made a value of an integral type of `count` positions with every one
// holding `fill` (LRM 6.7.1), as the type `T` lays it out: `fill` in each of
// its positions and every position above them clear, written where `into`
// lies, so a wide value is never made in the caller's frame first.
template <BitAddressed T>
void FillOver(T& into, std::uint64_t count, value::FourStateBit fill) {
  if constexpr (value::IntegralValue<T>) {
    value::FillScalar(into.MutableView(), value::FourStateBit::kZero);
    value::FillScalar(into.MutableView().Truncated(count), fill);
  } else {
    into = T::Filled(count, fill);
  }
}

// A value of an integral type of `count` positions with every one holding
// `fill`, as `FillOver` writes it.
template <BitAddressed T>
[[nodiscard]] auto FilledOver(std::uint64_t count, value::FourStateBit fill)
    -> T {
  T out{};
  FillOver(out, count, fill);
  return out;
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
// 6-4), and strength decides only between drivers (LRM 28.12.1). Where that one
// driver carries the whole of another object of the net's own type unchanged,
// the net's value is that object's, so the net shows it where it lies and
// nothing runs to carry it (LRM 23.3.3 has a port connection be a continuous
// assignment, and LRM 4.8 leaves open when its target sees the update). Any
// other net shows what its contributions resolve to.
enum class NetDecision : std::uint8_t { kShowsSource, kOneDriver, kResolution };

// Refuses a driver attaching to a net that has settled what decides it: every
// driver attaches while the design resolves, which is before any gives a value.
void RequireUnsettled(const std::optional<NetDecision>& decision);

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
  typename StorageFor<T>::Type* shown;
  bool watched;
};
template <class T>
struct KeptBeneath {
  T* kept;
};
template <class T>
using DriverLanding = std::variant<Contributing<T>, Showing<T>, KeptBeneath<T>>;

// The driver's value as it stands, wherever its writes land, of a type the
// net holds as the value itself.
template <class T>
  requires(!DynamicallySized<T>)
[[nodiscard]] auto DriverValueAt(const DriverLanding<T>& landing) -> T& {
  return std::visit(
      Overloaded{
          [](const Contributing<T>& to) -> T& { return *to.contribution; },
          [](const Showing<T>& to) -> T& { return to.shown->Storage(); },
          [](const KeptBeneath<T>& to) -> T& { return *to.kept; }},
      landing);
}

// Where the driver's value lies, as a reference names it.
template <class T>
[[nodiscard]] auto DriverBytesAt(const DriverLanding<T>& landing) -> void* {
  return std::visit(
      Overloaded{
          [](const Contributing<T>& to) -> void* {
            return HandleTo(*to.contribution);
          },
          [](const Showing<T>& to) -> void* { return to.shown->Bytes(); },
          [](const KeptBeneath<T>& to) -> void* { return HandleTo(*to.kept); }},
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

// A net (LRM 6.5, 6.6): readable and observable like a `Var<T>` (it extends
// `Observable`, so a process can wait on it), but never written directly. A
// value reaches it through a driver, and what it shows is what its drivers
// determine -- the value of the one driver that decides it, the object that
// driver carries, or what every contribution reaching it resolves to --
// published on a real change (LRM 9.4.2). The net owns where each driver's
// value lies; a `Driver<T>` names one by an index the net issued, so the
// storage stays the net's to reorganize.
//
// LRM 6.7.1 admits two kinds of data type, and a net of each is a class of its
// own: an integral type, whose positions a connection may name some of, and an
// unpacked aggregate, which no connection names any position of.
template <NetValue T>
class ResolvedNet;

// Where the value a net shows lies, named by a member of the design's bindings
// -- the kind a port standing for what its connection drives is (LRM 23.3.3):
// the net's own value, or, for a net whose one driver carries the whole of
// another object of its type, that object's, where it lies.
//
// The member names the net's own value until that driver's turn at time zero,
// as a net whose driver has given nothing shows (LRM 4.9.1). The turn binds it
// to the object, so whoever waits on the net waits on the object, and whatever
// is bound from the net follows (LRM 4.8). A procedural continuous assignment
// covering the net is a force of the member onto the net's own value, which
// first holds what the net showed, so it never reaches the object; its release
// binds the member back (LRM 10.6.2).
//
// The member names a value where it lies, as every reference does, and the net
// holds its own value as `HeldAs` holds it, which reads and takes a value
// lying anywhere as its own is laid out.
template <class HeldAs>
class NetShowing {
 public:
  explicit NetShowing(Observable& net)
      : own_{.holder = HeldByNet{&net}, .storage = nullptr, .whole = true},
        shown_(own_) {
    shown_.member = &shown_;
  }
  NetShowing(const NetShowing&) = delete;
  auto operator=(const NetShowing&) -> NetShowing& = delete;
  NetShowing(NetShowing&&) = delete;
  auto operator=(NetShowing&&) -> NetShowing& = delete;
  ~NetShowing() = default;

  // The net's own value is `own`, built after this and before anything names
  // the net, so the member names it from the net's construction on.
  void ShowOwn(HeldAs& own) {
    own_value_ = &own;
    own_.storage = own.Bytes();
    shown_.storage = own_.storage;
  }

  // Where what the net shows lies.
  [[nodiscard]] auto Shown() const noexcept -> const void* {
    return shown_.storage;
  }

  // Where the address of what the net shows is kept, which is what generated
  // code reads it through.
  [[nodiscard]] auto ShownThrough() const noexcept -> void* const* {
    return &shown_.storage;
  }

  // The member, which a wait on the net is enrolled through and another net
  // showing this one is bound from.
  [[nodiscard]] auto Member() const noexcept -> const ErasedReference& {
    return shown_;
  }

  // Records, while the design resolves, the object the one driver carries the
  // whole of: a variable, named by a reference to all of it, or the member
  // another net or an input port names its value through, which is then
  // followed as it is bound and forced.
  void Record(const ErasedReference& variable) {
    carried_ = variable;
    source_ = &*carried_;
  }
  void Record(const ErasedReference* member) {
    source_ = member;
  }

  // What decides the net once its drivers have all attached, `one_driver`
  // saying whether one driver alone decides it: that driver named an object it
  // carries, or it did not.
  [[nodiscard]] auto Decide(bool one_driver) const -> NetDecision {
    if (!one_driver) {
      return NetDecision::kResolution;
    }
    return source_ != nullptr ? NetDecision::kShowsSource
                              : NetDecision::kOneDriver;
  }

  // Whether the driver's turn has come.
  [[nodiscard]] auto Started() const -> bool {
    return started_;
  }

  // The driver's turn has come: from here on the net shows its source wherever
  // nothing covers it. `covered` says whether something does now.
  void Start(bool covered) {
    started_ = true;
    if (!covered) {
      Uncover();
    }
  }

  // Nothing covers the net any longer: the member is bound to the source, or
  // released back to it. The net's own value first takes the source's, as any
  // write, so whoever waits on the net -- and on nothing else, since only waits
  // through the member are enrolled there -- is told exactly when what it shows
  // changed, measured against the value it shows from then on.
  void Uncover() {
    BoundMembers& bound = current_runtime().Bound();
    const bool forced = bound.Forced(shown_);
    const void* next =
        forced ? bound.DriverOf(shown_).storage : LiveSource().storage;
    ShowDriverValue(
        current_runtime(), *std::get<HeldByNet>(own_.holder).net,
        own_value_->Get(), [&] { return own_value_->IsBitIdenticalTo(next); },
        [&] { own_value_->AssignBytes(next); });
    if (forced) {
      bound.Release(shown_);
    } else {
      bound.Bind(shown_, LiveSource());
    }
  }

  // Something now covers the net: its own value takes what it showed, and the
  // member and whatever is bound from it name that, where a covering value then
  // lands.
  void Cover() {
    BoundMembers& bound = current_runtime().Bound();
    if (!started_ || bound.Forced(shown_)) {
      return;
    }
    own_value_->AssignBytes(Shown());
    bound.Force(shown_, own_, nullptr);
  }

 private:
  // The source as it names its storage now.
  [[nodiscard]] auto LiveSource() const -> const ErasedReference& {
    if (source_ == nullptr) {
      throw InternalError(
          "NetShowing: a net shows a source its driver never named");
    }
    return *source_;
  }

  // The net's own value, and the reference to it, which the member is until the
  // driver's turn and while something covers the net.
  HeldAs* own_value_ = nullptr;
  ErasedReference own_;
  ErasedReference shown_;
  // A reference to the whole of the variable the driver carries, where that is
  // what it carries.
  std::optional<ErasedReference> carried_;
  // What the driver carries, none where it carries no whole object.
  const ErasedReference* source_ = nullptr;
  bool started_ = false;
};

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
// its one driver's value where that decides it -- so reading a net never
// reaches into a physical net.
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
  // takeover is driven with a value, of which it keeps as many positions as
  // resolve here.
  auto BeginTakeover(support::TakeoverLevel level, value::BitPositions held)
      -> std::uint32_t;
  auto DriveTakeover(
      support::TakeoverLevel level, std::uint32_t generation,
      const value::ConstIntegralView& forced) -> bool;
  auto EndTakeover(support::TakeoverLevel level, value::BitPositions released)
      -> bool;

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

// A net of an integral type: a name for some positions, the contributions the
// sources in its own unit make to them, and the value it shows -- what those
// positions last resolved to, what its one driver last gave, or the object that
// driver carries. A procedural continuous assignment may override what the
// drivers determine (LRM 10.6.2).
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
  using HeldAs = typename StorageFor<T>::Type;

  // A net is built told how many positions its declared type has, which the
  // type laying its value out does not say where it lays out a narrower one;
  // one of a type that does say it is built at that. A net of a value wider
  // than a word lays the words out after itself at that width.
  ResolvedNet()
    requires(!value::HeldAsWords<T>);
  explicit ResolvedNet(std::uint64_t count);

  // Fixes what the net's declared net type states: which truth table resolves
  // contributions of equal strength, and the contribution the net type itself
  // makes, as the scalar it shows where nothing drives it and the strength it
  // holds that scalar at (LRM 6.7.1). The net is therefore a readable
  // observable of its declared count before any driver attaches, showing what
  // its net type's own contribution resolves to. Installing twice is a
  // lowering defect.
  //
  // One entry per resolution: tri-state for `wire` / `tri` (LRM 6.6.1 Table
  // 6-2), wired-and for `wand` / `triand` and wired-or for `wor` / `trior` (LRM
  // 6.6.3 Tables 6-3 and 6-4), and one that resolves tri-state and leaves its
  // own contribution holding what the drivers last decided, which is how a net
  // stores a value (LRM 6.6.4).
  void InitializeTriState(std::int64_t fill, std::int64_t strength) {
    Install(
        fill, strength, value::NetResolution::kTriState,
        OwnContribution::kFixed);
  }
  void InitializeWiredAnd(std::int64_t fill, std::int64_t strength) {
    Install(
        fill, strength, value::NetResolution::kWiredAnd,
        OwnContribution::kFixed);
  }
  void InitializeWiredOr(std::int64_t fill, std::int64_t strength) {
    Install(
        fill, strength, value::NetResolution::kWiredOr,
        OwnContribution::kFixed);
  }
  void InitializeRetaining(std::int64_t fill, std::int64_t strength) {
    Install(
        fill, strength, value::NetResolution::kTriState,
        OwnContribution::kRetained);
  }

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet();

  // Where what the net shows lies, as a reference names it.
  [[nodiscard]] auto Shown() const noexcept -> const void* {
    return showing_.Shown();
  }
  // What the net shows, of a type it holds as the value itself.
  [[nodiscard]] auto Get() const noexcept -> const T&
    requires(!value::HeldAsWords<T>)
  {
    return *static_cast<const T*>(showing_.Shown());
  }
  [[nodiscard]] auto ShownThrough() const noexcept -> void* const* {
    return showing_.ShownThrough();
  }
  [[nodiscard]] auto Member() const noexcept -> const ErasedReference& {
    return showing_.Member();
  }

  // Arms the net to answer for its sampled value (LRM 16.5.1), whose default
  // is the type's default (LRM 6.8 Table 6-7) rather than anything the net
  // shows before time zero. Arming an armed net changes nothing.
  void ArmSampling() {
    if (sampled_ != nullptr) {
      return;
    }
    sampled_ = std::make_unique<SampledValueOf<TakenShown>>(
        TakenShown(*this),
        [&] { return FilledOver<T>(count_, value::FourStateBit::kUnknown); },
        Trigger(WatchedPlace::Through(Member()), Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto SampledGet() const -> const T& {
    return SampledOf(sampled_.get());
  }
  // Assigns what the net shows to `out`, as its sampled value takes it.
  void TakeShownInto(T& out) const {
    if constexpr (value::HeldAsWords<T>) {
      out = held_.ToOwned(showing_.Shown());
    } else {
      out = Get();
    }
  }

  // Puts the positions this net reaches under a procedural continuous
  // assignment and takes them back out (LRM 10.6.2). A `force` on a net
  // overrides every driver rather than joining them, so what these change is
  // what the positions show, never the contributions -- which go on being
  // updated underneath and are what they answer with again once released.
  //
  // What is forced is the physical net, so the value shows under every name
  // reaching those positions.
  auto BeginTakeover(std::int64_t level, std::int64_t lsb, std::int64_t count)
      -> std::int64_t {
    return std::int64_t{WholePhysicalNet().BeginTakeover(
        TakeoverLevelOf(level), PositionsNamed(lsb, count, AllPositions()))};
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
    return DriveTakeoverFromView(
        level, generation, value::WideViewAt(bytes, count_, T::kDomain));
  }

  void EndTakeover(std::int64_t level, std::int64_t lsb, std::int64_t count) {
    PhysicalNet& physical = WholePhysicalNet();
    if (!physical.EndTakeover(
            TakeoverLevelOf(level),
            PositionsNamed(lsb, count, AllPositions()))) {
      return;
    }
    RuntimeEffects& runtime = current_runtime();
    if (name_.Overridden()) {
      physical.Reresolve(runtime);
      return;
    }
    switch (Decision()) {
      case NetDecision::kResolution:
        physical.Reresolve(runtime);
        return;
      // Nothing covers a net one driver decides any longer, so it shows what
      // that driver produced meanwhile (LRM 10.6.2), where an override ever
      // gave a value at all.
      case NetDecision::kOneDriver:
        if (const std::unique_ptr<T> kept = std::move(kept_beneath_)) {
          ShowDriverValue(
              runtime, *this, held_.Get(),
              [&] { return held_.IsBitIdentical(*kept); },
              [&] { held_.Assign(*kept); });
        }
        return;
      // The object its driver carries is what such a net shows once nothing
      // covers it, from the driver's turn on; before that it shows what an
      // undriven net does.
      case NetDecision::kShowsSource:
        if (showing_.Started()) {
          showing_.Uncover();
        } else {
          physical.Reresolve(runtime);
        }
        return;
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
      auto&& own = net_->Own();
      KeptPart<std::remove_cvref_t<decltype(own)>> kept(
          own, value::BitPositions{.lsb = offset, .width = positions.Count()});
      WritePositions(own, offset, positions);
      if (const std::optional<Change> change = kept.ChangeTo(own)) {
        runtime.WakeParkedOn(*net_, PartPath{}, *change);
      }
    }

    ResolvedNet* net_;
  };

  void Install(
      std::int64_t fill, std::int64_t strength, value::NetResolution resolution,
      OwnContribution own_kind) {
    if (installed_) {
      throw InternalError(
          "ResolvedNet: the net's declared net type is already fixed");
    }
    installed_ = true;
    FillOver(nondriving_, count_, value::FourStateBit::kHighImpedance);
    held_.Assign(nondriving_);
    auto&& own = Own();
    WritePositions(
        own, 0,
        name_.Declare(
            static_cast<support::StrengthLevel>(strength), NetFillOf(fill),
            resolution, own_kind, count_));
  }

  // The storage of a net of `count` positions, which keeps words at its tail
  // at that width and holds any other value as itself.
  [[nodiscard]] static auto HeldFor(std::uint64_t count) -> HeldAs {
    if constexpr (value::HeldAsWords<T>) {
      return HeldAs(count);
    } else {
      return HeldAs();
    }
  }

  // The net's own value, to write positions into where it lies: the value
  // itself, or its words with their width.
  [[nodiscard]] auto Own() -> decltype(auto) {
    if constexpr (value::HeldAsWords<T>) {
      return held_.Get();
    } else {
      return held_.Storage();
    }
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
    // again once nothing covers it (LRM 10.6.2). One showing the object its
    // driver carries goes back to its own storage, which the forced value then
    // lands in, so the object is never forced with it.
    switch (Decision()) {
      case NetDecision::kResolution:
        break;
      case NetDecision::kOneDriver:
        if (kept_beneath_ == nullptr) {
          kept_beneath_ = std::make_unique<T>(held_.AsOwned());
        }
        break;
      case NetDecision::kShowsSource:
        showing_.Cover();
        break;
    }
    physical.Reresolve(current_runtime());
    return true;
  }

  // Every position of this net.
  [[nodiscard]] auto AllPositions() const -> value::BitPositions {
    return {.lsb = 0, .width = count_};
  }

  // The physical net this name reaches, where it reaches one covering the whole
  // of it. An operation written on a name rather than on some of its positions
  // needs that, because a name reaching several has no way to say which it
  // meant.
  [[nodiscard]] auto WholePhysicalNet() -> PhysicalNet& {
    const std::vector<NetReach>& reaches = name_.Reaches();
    if (reaches.size() != 1 || reaches.front().count != count_) {
      // LRM 10.6.2.
      throw SimulationError(
          "a procedural continuous assignment names a net that a connection "
          "reaches over part of one resolution, so what it overrides is part "
          "of a physical net, which is not yet supported");
    }
    return *reaches.front().physical;
  }

  // What decides this net, settled the first time it is asked.
  [[nodiscard]] auto Decision() -> NetDecision {
    if (!decision_.has_value()) {
      name_.SettleReaches();
      const bool one_driver = name_.OnlyOwnDriversContribute() &&
                              contributions_.size() == 1 &&
                              LevelBit(contributions_.front().strength) != 0U;
      decision_ = showing_.Decide(one_driver);
    }
    return *decision_;
  }

  // The turn of this net's drivers has come at time zero. One that shows the
  // object its one driver carries does from here on, and that driver has
  // nothing left to give.
  auto ShowSource() -> bool {
    switch (Decision()) {
      case NetDecision::kResolution:
      case NetDecision::kOneDriver:
        return false;
      case NetDecision::kShowsSource:
        showing_.Start(name_.Overridden());
        return true;
    }
    throw InternalError("ResolvedNet: unknown net decision");
  }

  // Where a write of the driver `index` names lands.
  [[nodiscard]] auto LandingOf(std::size_t index) -> DriverLanding<T> {
    switch (Decision()) {
      case NetDecision::kResolution:
        return Contributing<T>{.contribution = &ContributionOf(index).value};
      case NetDecision::kOneDriver:
        if (kept_beneath_ != nullptr) {
          return KeptBeneath<T>{.kept = kept_beneath_.get()};
        }
        return Showing<T>{.shown = &held_, .watched = this->HasMembers()};
      case NetDecision::kShowsSource:
        throw InternalError(
            "ResolvedNet: a driver whose net shows the object it carries is "
            "written, though its drive ends at its turn");
    }
    throw InternalError("ResolvedNet: unknown net decision");
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
                  runtime, *this, to.shown->Get(),
                  [&] { return to.shown->IsBitIdentical(value); },
                  [&] { to.shown->Assign(value); });
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
              if (to.contribution->IsBitIdenticalTo(bytes)) {
                return;
              }
              to.contribution->AssignBytes(bytes);
              ReresolveJoint(runtime, Change::Whole());
            },
            [&](const Showing<T>& to) {
              ShowDriverValue(
                  runtime, *this, to.shown->Get(),
                  [&] { return to.shown->IsBitIdenticalTo(bytes); },
                  [&] { to.shown->AssignBytes(bytes); });
            },
            [&](const KeptBeneath<T>& to) { to.kept->AssignBytes(bytes); }},
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

  NetShowing<HeldAs> showing_{*this};
  T nondriving_{};
  // How many positions the declared type has.
  std::uint32_t count_;
  bool installed_ = false;
  std::optional<NetDecision> decision_;
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
  // What the net's sampled value takes what the net shows through.
  using TakenShown = TakenFrom<ResolvedNet, T, &ResolvedNet::TakeShownInto>;
  // The net's sampled value, absent until something samples it.
  std::unique_ptr<SampledValueOf<TakenShown>> sampled_;
  // The net's own value, last, so words it lays out after itself lie where
  // the net ends.
  HeldAs held_;
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
  using HeldAs = typename StorageFor<T>::Type;

  ResolvedNet()
    requires(!value::LaidOutByType<T>);
  explicit ResolvedNet(const value::ValueType& type)
    requires value::LaidOutByType<T>;

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

  // Where what the net shows lies, as a reference names it.
  [[nodiscard]] auto Shown() const noexcept -> const void* {
    return showing_.Shown();
  }
  // What the net shows, of a type it holds as the value itself.
  [[nodiscard]] auto Get() const noexcept -> const T&
    requires(!value::LaidOutByType<T>)
  {
    return *static_cast<const T*>(showing_.Shown());
  }
  [[nodiscard]] auto ShownThrough() const noexcept -> void* const* {
    return showing_.ShownThrough();
  }
  [[nodiscard]] auto Member() const noexcept -> const ErasedReference& {
    return showing_.Member();
  }

  // Arms the net to answer for its sampled value (LRM 16.5.1), whose default
  // is the type's default (LRM 6.8 Table 6-7) in every bit rather than anything
  // the net shows before time zero. Arming an armed net changes nothing.
  void ArmSampling() {
    if (sampled_ != nullptr) {
      return;
    }
    sampled_ = std::make_unique<SampledValueOf<TakenShown>>(
        TakenShown(*this),
        [&] {
          return T::FilledLike(
              nondriving_, value::Logic::Filled(value::FourStateBit::kUnknown));
        },
        Trigger(WatchedPlace::Through(Member()), Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto SampledGet() const -> const T& {
    return SampledOf(sampled_.get());
  }
  // Assigns what the net shows to `out`, as its sampled value takes it.
  void TakeShownInto(T& out) const {
    if constexpr (value::LaidOutByType<T>) {
      out = ToOwned(showing_.Shown());
    } else {
      out = *static_cast<const T*>(showing_.Shown());
    }
  }

  // A value of the net's own type handed as its bytes, as the net keeps one of
  // its own: a tuple, a union or an array is laid out by the type the net was
  // built with.
  [[nodiscard]] auto ToOwned(const void* bytes) const -> T
    requires value::LaidOutByType<T>
  {
    return held_.ToOwned(bytes);
  }

  // Puts the net under a procedural continuous assignment and takes it back
  // out (LRM 10.6.2). A `force` overrides every driver, so what it changes is
  // what the net shows, never the contributions, which are what the net
  // answers with again once released. A net of this kind holds an unpacked
  // value, of which a force names no bits (LRM 10.6.2), so what a force names
  // of it is always the whole.
  auto BeginTakeover(std::int64_t level, std::int64_t lsb, std::int64_t count)
      -> std::int64_t {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<Takeovers<T>>();
    }
    return std::int64_t{takeovers_->Begin(
        TakeoverLevelOf(level), PositionsNamed(lsb, count, kHeldWhole))};
  }

  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& value) -> bool {
    return RecordTakeover(level, generation, value);
  }

  // The same of a value handed as its bytes, laid out by the net's type.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes) -> bool
    requires value::LaidOutByType<T>
  {
    return RecordTakeover(level, generation, held_.ToOwned(bytes));
  }
  void EndTakeover(std::int64_t level, std::int64_t lsb, std::int64_t count) {
    if (takeovers_ == nullptr) {
      return;
    }
    takeovers_->End(
        TakeoverLevelOf(level), PositionsNamed(lsb, count, kHeldWhole));
    RuntimeEffects& runtime = current_runtime();
    if (Overridden()) {
      Reresolve(runtime);
      return;
    }
    switch (Decision()) {
      case NetDecision::kResolution:
        Reresolve(runtime);
        return;
      // Nothing covers a net one driver decides any longer, so it shows what
      // that driver produced meanwhile (LRM 10.6.2), where an override ever
      // gave a value at all.
      case NetDecision::kOneDriver:
        if (const T* kept = takeovers_->Beneath()) {
          ShowDriverValue(
              runtime, *this, held_.Get(),
              [&] { return held_.IsBitIdentical(*kept); },
              [&] { held_.Assign(*kept); });
        }
        takeovers_->DropBeneath();
        return;
      // The object its driver carries is what such a net shows once nothing
      // covers it, from the driver's turn on; before that it shows what an
      // undriven net does.
      case NetDecision::kShowsSource:
        if (showing_.Started()) {
          showing_.Uncover();
        } else {
          Reresolve(runtime);
        }
        return;
    }
  }

  // Attaches a new driver at the strength its source drives at and returns its
  // handle, which is the net's own. Its contribution starts at the non-driving
  // one, so attaching is not itself an act of driving.
  auto AttachDriver(std::int64_t strength) -> Driver<T>&;

 private:
  friend class Driver<T>;

  // Has the takeover at `level` record what it evaluated to, and shows
  // whatever the highest level in effect then holds.
  auto RecordTakeover(
      std::int64_t level, std::int64_t generation, const T& evaluated) -> bool {
    if (takeovers_ == nullptr) {
      return false;
    }
    const bool was_covered = Overridden();
    if (!takeovers_->Drive(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation),
            evaluated)) {
      return false;
    }
    // The first override to cover a net one driver decides finds in it what
    // that driver last produced, which is kept beneath for the net to show
    // again once nothing covers it (LRM 10.6.2). One showing the object its
    // driver carries goes back to its own storage, so the object is never
    // forced with it.
    if (!was_covered) {
      switch (Decision()) {
        case NetDecision::kResolution:
          break;
        case NetDecision::kOneDriver:
          takeovers_->KeepBeneath(held_.AsOwned());
          break;
        case NetDecision::kShowsSource:
          showing_.Cover();
          break;
      }
    }
    Reresolve(current_runtime());
    return true;
  }

  void Install(
      const T& prototype, std::int64_t fill, std::int64_t strength,
      value::NetResolution resolution, OwnContribution own_kind) {
    if (installed_) {
      throw InternalError(
          "ResolvedNet: the net's declaration is installed twice");
    }
    installed_ = true;
    value::RebuildFrom(nondriving_, [&] {
      return T::FilledLike(
          prototype, value::Logic::Filled(value::FourStateBit::kHighImpedance));
    });
    value::RebuildFrom(own_.value, [&] {
      return T::FilledLike(prototype, value::Logic::Filled(NetFillOf(fill)));
    });
    own_.strength = static_cast<support::StrengthLevel>(strength);
    fold_ = resolution;
    own_kind_ = own_kind;
    held_.Assign(*Resolve());
  }

  // What the net resolves to: between levels the stronger contribution
  // determines every bit it drives (LRM 28.12.1), and within one the net
  // type's table folds them (LRM 6.6.1, 6.6.3), each level starting from the
  // all-`z` value every fold treats as its identity.
  // Each value it works through is kept where a value of the net's type may
  // be kept, so a net too large for a frame keeps none in one.
  [[nodiscard]] auto Resolve() const -> value::SmallBox<T> {
    const OccupiedLevels occupied = occupied_ | LevelBit(own_.strength);
    value::SmallBox<T> resolved(nondriving_);
    for (std::size_t level = support::kStrengthLevelCount; level-- > 0;) {
      const auto at = static_cast<support::StrengthLevel>(level);
      if ((occupied & LevelBit(at)) == 0U) {
        continue;
      }
      value::SmallBox<T> group(own_.strength == at ? own_.value : nondriving_);
      for (const DriveContribution<T>& driver : contributions_) {
        if (driver.strength == at) {
          group = value::SmallBox<T>::BuiltBy(
              [&] { return value::Resolve(fold_, *group, driver.value); });
        }
      }
      resolved = value::SmallBox<T>::BuiltBy(
          [&] { return resolved->Dominating(*group); });
    }
    return resolved;
  }

  // Recomputes the net and publishes if what it shows moved. A net type that
  // stores a value takes the resolution it just took part in as its own
  // contribution (LRM 6.6.4), and a takeover displaces what the net shows and
  // not what it holds (LRM 10.6.2).
  void Reresolve(RuntimeEffects& runtime) {
    value::SmallBox<T> resolved = Resolve();
    if (own_kind_ == OwnContribution::kRetained) {
      own_.value = *resolved;
    }
    if (takeovers_ != nullptr) {
      takeovers_->Overlay(*resolved);
    }
    PublishIfChanged(runtime, *resolved);
  }

  // What decides this net, settled the first time it is asked. No connection
  // reaches part of an aggregate, so it is one driver that decides it wherever
  // there is exactly one, driving, and the net type contributes at high
  // impedance, which takes no part (LRM 28.12.1), and keeps nothing it was
  // driven with (LRM 6.6.4).
  [[nodiscard]] auto Decision() -> NetDecision {
    if (!decision_.has_value()) {
      const bool one_driver = LevelBit(own_.strength) == 0U &&
                              own_kind_ == OwnContribution::kFixed &&
                              contributions_.size() == 1 &&
                              LevelBit(contributions_.front().strength) != 0U;
      decision_ = showing_.Decide(one_driver);
    }
    return *decision_;
  }

  // The turn of this net's drivers has come at time zero. One that shows the
  // object its one driver carries does from here on, and that driver has
  // nothing left to give.
  auto ShowSource() -> bool {
    switch (Decision()) {
      case NetDecision::kResolution:
      case NetDecision::kOneDriver:
        return false;
      case NetDecision::kShowsSource:
        showing_.Start(Overridden());
        return true;
    }
    throw InternalError("ResolvedNet: unknown net decision");
  }

  // Whether a procedural continuous assignment in force covers the net (LRM
  // 10.6.2).
  [[nodiscard]] auto Overridden() const -> bool {
    return takeovers_ != nullptr && takeovers_->Shows();
  }

  // Where a write of the driver `index` names lands.
  [[nodiscard]] auto LandingOf(std::size_t index) -> DriverLanding<T> {
    switch (Decision()) {
      case NetDecision::kResolution:
        return Contributing<T>{.contribution = &ContributionOf(index).value};
      case NetDecision::kOneDriver:
        if (T* kept = takeovers_ == nullptr ? nullptr : takeovers_->Beneath()) {
          return KeptBeneath<T>{.kept = kept};
        }
        return Showing<T>{.shown = &held_, .watched = this->HasMembers()};
      case NetDecision::kShowsSource:
        throw InternalError(
            "ResolvedNet: a driver whose net shows the object it carries is "
            "written, though its drive ends at its turn");
    }
    throw InternalError("ResolvedNet: unknown net decision");
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
                  runtime, *this, to.shown->Get(),
                  [&] { return to.shown->IsBitIdentical(value); },
                  [&] { to.shown->Assign(value); });
            },
            [&](const KeptBeneath<T>& to) { *to.kept = value; }},
        LandingOf(index));
  }

  // The same of a value handed as its bytes, laid out as the driver's value
  // is, taken into the storage that value has. A contribution already holding
  // them moves no resolution.
  void SetDriverBytes(
      RuntimeEffects& runtime, std::size_t index, const void* bytes)
    requires value::TakesBytesInPlace<T>
  {
    std::visit(
        Overloaded{
            [&](const Contributing<T>& to) {
              if (to.contribution->IsBitIdenticalTo(bytes)) {
                return;
              }
              to.contribution->AssignBytes(bytes);
              Reresolve(runtime);
            },
            [&](const Showing<T>& to) {
              ShowDriverValue(
                  runtime, *this, to.shown->Get(),
                  [&] { return to.shown->IsBitIdenticalTo(bytes); },
                  [&] { to.shown->AssignBytes(bytes); });
            },
            [&](const KeptBeneath<T>& to) { to.kept->AssignBytes(bytes); }},
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
    if (held_.IsBitIdentical(next)) {
      return;
    }
    held_.Assign(next);
    runtime.WakeParkedOn(*this, PartPath{}, Change::Whole());
  }

  NetShowing<HeldAs> showing_{*this};
  T nondriving_{};
  // The contribution the net type makes to its own resolution, the table
  // contributions of equal strength fold under, and whether resolution keeps
  // that contribution current (LRM 6.7.1, 6.6.4).
  DriveContribution<T> own_{};
  value::NetResolution fold_{};
  OwnContribution own_kind_{};
  bool installed_ = false;
  // The procedural continuous assignments the net has been put under (LRM
  // 10.6.2), absent until the first one starts.
  std::unique_ptr<Takeovers<T>> takeovers_;
  // Whether any contribution of a driver sits at a given level.
  OccupiedLevels occupied_{};
  std::optional<NetDecision> decision_;
  std::vector<DriveContribution<T>> contributions_;
  // The handles this net has issued, which growth must not move.
  std::deque<Driver<T>> drivers_;
  // What the net's sampled value takes what the net shows through.
  using TakenShown = TakenFrom<ResolvedNet, T, &ResolvedNet::TakeShownInto>;
  // The net's sampled value, absent until something samples it.
  std::unique_ptr<SampledValueOf<TakenShown>> sampled_;
  // The net's own value, last, so a value it lays out after itself lies where
  // the net ends.
  HeldAs held_;
};

// Defaulted here rather than where they are declared, for the reason a variable
// cell's are: one defaulted on its first declaration is defined by every unit
// that holds a net.
template <NetValue T>
  requires BitAddressed<T>
ResolvedNet<T>::ResolvedNet()
  requires(!value::HeldAsWords<T>)
    : ResolvedNet(T::kWidth) {
}

template <NetValue T>
  requires BitAddressed<T>
ResolvedNet<T>::ResolvedNet(std::uint64_t count)
    : ConnectableNet(name_),
      count_(static_cast<std::uint32_t>(count)),
      held_(HeldFor(count)) {
  showing_.ShowOwn(held_);
}

template <NetValue T>
  requires BitAddressed<T>
ResolvedNet<T>::~ResolvedNet() = default;

template <NetValue T>
  requires(!BitAddressed<T>)
ResolvedNet<T>::ResolvedNet()
  requires(!value::LaidOutByType<T>)
{
  showing_.ShowOwn(held_);
}

template <NetValue T>
  requires(!BitAddressed<T>)
ResolvedNet<T>::ResolvedNet(const value::ValueType& type)
  requires value::LaidOutByType<T>
    : held_(type) {
  showing_.ShowOwn(held_);
}

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

  // The same of a value handed as its bytes, laid out as the contribution is.
  void SetBytes(const void* bytes) const
    requires value::TakesBytesInPlace<T>
  {
    Net().SetDriverBytes(current_runtime(), contribution_, bytes);
  }

  // States, while the design resolves, that what this driver gives is the
  // whole of `source`, unchanged: a variable, a net, or an input port declared
  // as a variable, of the net's own type. Where the driver alone decides its
  // net, the net then shows `source` where it lies. A variable is named as a
  // reference to the whole of it names it.
  void DriveFrom(Var<T>* source) const
    requires(!DynamicallySized<T>)
  {
    RecordVariable(WholeReference(*source));
  }
  template <class HeldAs>
  void DriveFrom(TailCell<HeldAs>* source) const
    requires std::same_as<HeldAs, typename StorageFor<T>::Type>
  {
    RecordVariable(WholeReference(*source));
  }
  void DriveFrom(ResolvedNet<T>* source) const {
    RequireUnsettled(Net().decision_);
    Net().showing_.Record(&source->Member());
  }
  void DriveFrom(ErasedReference* source) const {
    RequireUnsettled(Net().decision_);
    Net().showing_.Record(source);
  }
  void DriveFrom(VariableRef<T>* source) const {
    DriveFrom(&source->AsMember());
  }

  // This driver's turn has come at time zero: answers whether its net shows
  // the object the driver carries from here on, which leaves the driver
  // nothing to give.
  [[nodiscard]] auto ShowSource() const -> bool {
    return Net().ShowSource();
  }

  // This driver's own value as it currently stands, of a type the net holds as
  // the value itself.
  [[nodiscard]] auto Get() const -> const T&
    requires(!DynamicallySized<T>)
  {
    return DriverValueAt(Landing());
  }

  // Where this driver's own value lies, as a reference names it, and how many
  // positions an integral one has.
  [[nodiscard]] auto Bytes() const -> void* {
    return DriverBytesAt(Landing());
  }
  [[nodiscard]] auto Width() const -> std::uint64_t
    requires BitAddressed<T>
  {
    return Net().count_;
  }
  // How the net holds a value it keeps after itself, which says how wide it
  // is or its type.
  [[nodiscard]] auto HeldStorage() const -> const typename StorageFor<T>::Type&
    requires DynamicallySized<T>
  {
    return Net().held_;
  }

  [[nodiscard]] auto Mutate() const -> ScopedMutation<Driver>
    requires(!DynamicallySized<T>)
  {
    return ScopedMutation<Driver>{*this};
  }

  // The `MutationSink` surface, each answer read off where this driver's
  // writes land. A driver's write is never overridden: under an override it
  // lands in what the net keeps beneath it, which nothing watches.
  [[nodiscard]] auto MutationStorage() const -> T&
    requires(!DynamicallySized<T>)
  {
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
  void RecordVariable(const ErasedReference& variable) const {
    RequireUnsettled(Net().decision_);
    Net().showing_.Record(variable);
  }

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
  contributions_.emplace_back(nondriving_, level);
  name_.Occupy(level);
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

template <NetValue T>
  requires(!BitAddressed<T>)
auto ResolvedNet<T>::AttachDriver(std::int64_t strength) -> Driver<T>& {
  RequireUnsettled(decision_);
  const auto level = static_cast<support::StrengthLevel>(strength);
  contributions_.emplace_back(nondriving_, level);
  occupied_ |= LevelBit(level);
  drivers_.emplace_back(*this, contributions_.size() - 1);
  return drivers_.back();
}

static_assert(MutationSink<Driver<value::Logic>>);

// What a wait on a net enrols on: whatever the net's member names, reached
// through the member, so the wait follows it as it is bound and forced.
template <NetValue T>
auto ReportsTo(const ResolvedNet<T>& net) -> WatchedPlace {
  return WatchedPlace::Through(net.Member());
}

}  // namespace lyra::runtime
