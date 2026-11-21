#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <memory>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/fixed_array.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/bound_members.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/strength_level.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/frame_bound.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/positions.hpp"
#include "lyra/value/small_box.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// A value of an integral type of `count` positions with every one holding
// `fill` (LRM 6.7.1), as the type `T` lays it out: `fill` in each of its
// positions and every position above them clear.
template <BitAddressed T>
[[nodiscard]] auto FilledOver(std::uint64_t count, value::FourStateBit fill)
    -> T {
  if constexpr (value::IntegralValue<T>) {
    T out{};
    value::FillScalar(out.MutableView(), value::FourStateBit::kZero);
    value::FillScalar(out.MutableView().Truncated(count), fill);
    return out;
  } else {
    return T::Filled(count, fill);
  }
}

// The scalar a net type contributes at each position (LRM 6.7.1), which
// arrives at a net's install as the machine integer every runtime scalar is.
[[nodiscard]] auto NetFillOf(std::int64_t fill) -> value::FourStateBit;

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
// 6-4), strength decides only between drivers (LRM 28.12.1), and a net with
// no resolution function has its one driver's value (LRM 6.6.7). Where that one
// driver carries the whole of another object of the net's own type unchanged,
// the net's value is that object's, so the net shows it where it lies and
// nothing runs to carry it (LRM 23.3.3 has a port connection be a continuous
// assignment, and LRM 4.8 leaves open when its target sees the update). Any
// other net shows what its contributions resolve to.
enum class NetDecision : std::uint8_t { kShowsSource, kOneDriver, kResolution };

// Refuses a driver attaching to a net that has settled what decides it: every
// driver attaches while the design resolves, which is before any gives a value.
void RequireUnsettled(const std::optional<NetDecision>& decision);

// Replaces the whole of the value `net` shows, which `shown` holds, by what
// the one driver deciding it gives, as a whole write of a variable is taken
// (LRM 4.3): a net no wait is enrolled on has no one to tell, so nothing is
// compared; a value it already shows moves nothing; and otherwise whoever
// waits is told what moved (LRM 9.4.2). `already_shown` answers whether the
// net shows the same bits, and `overwrite` replaces what it shows.
template <class HeldAs, class AlreadyShown, class Overwrite>
void ShowDriverValue(
    RuntimeEffects& runtime, Observable& net, const HeldAs& shown,
    AlreadyShown already_shown, Overwrite overwrite) {
  if (!net.HasMembers()) {
    overwrite();
    return;
  }
  if (already_shown()) {
    return;
  }
  ReplaceHeld(
      shown, net.WatchedInside(PartPath{}), overwrite,
      [&](const Change& change) {
        runtime.WakeParkedOn(net, PartPath{}, change);
      });
}

// Where a write of the one driver of a net of a nettype with no resolution
// function lands, which says what follows it. It writes the value the net
// shows, and whoever waits on what moved is told, where a wait is enrolled at
// all (LRM 4.3). While a procedural continuous assignment covers the net the
// write lands in the driver's contribution, and the net shows it again, so the
// positions no takeover holds show it and the rest show it once released (LRM
// 10.6.2).
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
using DriverLanding = std::variant<Contributing<T>, Showing<T>>;

// The driver's value as it stands, wherever its writes land, of a type the
// net holds as the value itself.
template <class T>
  requires(!DynamicallySized<T>)
[[nodiscard]] auto DriverValueAt(const DriverLanding<T>& landing) -> T& {
  return std::visit(
      Overloaded{
          [](const Contributing<T>& to) -> T& { return *to.contribution; },
          [](const Showing<T>& to) -> T& { return to.shown->Storage(); }},
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
          [](const Showing<T>& to) -> void* { return to.shown->Bytes(); }},
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

// A net value with the positions from `offset` replaced by `positions`, every
// other position left as it stands.
template <BitAddressed T>
void WritePositions(
    T& into, std::uint32_t offset, const NetPositions& positions) {
  value::Insert(into.MutableView(), positions.View(), offset);
}

class NetName;
class PhysicalNet;

// A second driver reached a net with no resolution function (LRM 6.6.7).
[[noreturn]] void NetHasNoResolution();

// What a net of an unpacked type asks of the type: how many positions a value
// has, and its positions read out whole and written in part, which is how what
// the net's positions resolve to reaches the value it shows (LRM 6.6.7).
// A type the library is compiled with states these itself, and one no net
// type resolves has none.
template <class T>
struct NetPositionNumbering {};

template <value::NetResolvable T>
struct NetPositionNumbering<T> {
  [[nodiscard]] auto PositionCount() const -> std::int64_t {
    return value::PositionCountOf<T>();
  }
  // Every position of the value lying at `value`, written into `out`. They
  // are as many as the value's bits, so they are made where a frame does not
  // keep them.
  void ToPositions(const void* value, const value::IntegralView& out) const {
    const auto positions = value::SmallBox<decltype(value::ToPositionsOf(
        std::declval<const T&>()))>::BuiltBy([&] {
      return value::ToPositionsOf(*static_cast<const T*>(value));
    });
    value::Extract(out, positions->AsWords().View(), 0);
  }
  // The positions `in` reaches of the value lying at `value`, its first
  // position standing at `start` of the value, replaced in place; answers
  // whether any changed.
  auto WritePositions(
      void* value, const value::ConstIntegralView& in, std::int64_t start,
      std::int64_t count) const -> bool {
    return value::WritePositionsOf(*static_cast<T*>(value), in, start, count);
  }
};

// A type the library was compiled without states its positions through the
// table its net was built holding, since nothing but a net asks them.
template <value::LaidOutByType T>
struct NetPositionNumbering<T> {
  const value::ValueType* type = nullptr;
  const value::NetWitnessTable* table = nullptr;

  [[nodiscard]] auto PositionCount() const -> std::int64_t {
    return table->position_count(*type);
  }
  void ToPositions(const void* value, const value::IntegralView& out) const {
    table->to_positions(*type, value, out);
  }
  auto WritePositions(
      void* value, const value::ConstIntegralView& in, std::int64_t start,
      std::int64_t count) const -> bool {
    return table->write_positions(*type, value, &in, start, count);
  }
};

// A type whose net resolves positions: one stating its positions itself, and
// one laid out by a type the design is compiled with, whose positions the
// table the net is built holding states, which a net of a type no net type
// resolves is never asked.
template <class T>
concept ResolvedNetValue = value::NetResolvable<T> || value::LaidOutByType<T>;

// A value a net holds: an integral one, whose positions resolve wherever a
// connection put them, or any other value a variable holds that the net can
// build with nothing told, since it builds the values it starts from before it
// is installed. The net types' tables resolve an integral value and an
// aggregate of them position by position (LRM 6.6.1, 6.6.3); a net of any
// other type is what an input port written with no port kind is (LRM
// 23.2.2.3), run as a net of a nettype with no resolution function (LRM
// 6.6.7).
template <class T>
concept NetValue = value::LaidOutByType<T> ||
                   (value::LyraValue<T> && std::default_initializable<T>);

template <NetValue T>
class Driver;

template <NetValue T>
class UnresolvedNet;

// What a net is told of the object its one driver carries: kept by every net
// as a part of itself, for the reason its name is (a base with virtual
// functions would put the net's address apart from its observable's).
class CarriedObjectSink {
 public:
  CarriedObjectSink(const CarriedObjectSink&) = delete;
  auto operator=(const CarriedObjectSink&) -> CarriedObjectSink& = delete;
  CarriedObjectSink(CarriedObjectSink&&) = delete;
  auto operator=(CarriedObjectSink&&) -> CarriedObjectSink& = delete;

  // States, while the design resolves, that what a driver of the whole net
  // gives is the whole of an object of the net's own type, unchanged: a
  // variable, named by a reference to all of it, or the member another net or
  // an input port names its value through (LRM 23.3.3, 4.8). Only a net that
  // driver alone decides then shows that object.
  virtual void CarryVariable(const ErasedReference& variable) = 0;
  virtual void CarryMember(const ErasedReference* member) = 0;

  // The turn of the net's drivers has come at time zero: answers whether the
  // net shows the object its one driver carries from here on, which leaves
  // that driver nothing to give.
  virtual auto ShowSource() -> bool = 0;

 protected:
  CarriedObjectSink();
  ~CarriedObjectSink();
};

// What a driver of a net resolving positions gives through: the net, which
// says where what it gives lands. A driver drives some of the net's positions
// and gives them whole each time (LRM 10.3.2), so what it hands over is those
// positions, whatever type it names them by.
class DrivenNet : public CarriedObjectSink {
 public:
  DrivenNet(const DrivenNet&) = delete;
  auto operator=(const DrivenNet&) -> DrivenNet& = delete;
  DrivenNet(DrivenNet&&) = delete;
  auto operator=(DrivenNet&&) -> DrivenNet& = delete;

  // The driver of the contribution `index` now gives `given`, whose positions
  // from `skip` are the ones it drives.
  virtual void Give(
      std::uint32_t index, const value::ConstIntegralView& given,
      std::uint32_t skip) = 0;

 protected:
  DrivenNet();
  ~DrivenNet();
};

// One driver as the net it drives keeps it: the net, which of its
// contributions is the driver's, how many positions wide the value it gives
// is, and how many of that value's lowest positions fall below the net. A
// select of a name a modport joins declarations under writes each declaration
// the whole value at where the select starts, counted from where that
// declaration begins, and a bit outside the declaration writes nothing (LRM
// 25.5.4, 11.5.1), so such a driver drives only the positions of its value
// that land in the net. The net keeps these where they never move, so a source
// may hold one by address.
struct DriverSlot {
  DrivenNet* net = nullptr;
  std::uint32_t contribution = 0;
  std::uint32_t width = 0;
  std::uint32_t skip = 0;
};

// A net (LRM 6.5, 6.6): readable and observable like a `Var<T>` (it extends
// `Observable`, so a process can wait on it), but never written directly. A
// value reaches it through a driver, and what it shows is what its drivers
// determine -- the value of the one driver that decides it, the object that
// driver carries, or what every contribution reaching it resolves to --
// published on a real change (LRM 9.4.2). The net owns where each driver's
// value lies; a `Driver<T>` names one by an index the net issued, so the
// storage stays the net's to reorganize.
//
// A net of an integral type and one of an unpacked aggregate a net type
// resolves are each a class of their own over what every net resolving
// positions shares: one holds its positions as an integral value, the other
// lays them into the parts of the aggregate it shows.
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
  void RecordVariable(const ErasedReference& variable) {
    carried_ = variable;
    source_ = &*carried_;
  }
  void RecordMember(const ErasedReference* member) {
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
        current_runtime(), *std::get<HeldByNet>(own_.holder).net, *own_value_,
        [&] { return own_value_->IsBitIdenticalTo(next); },
        [&] { own_value_->AssignBytes(next); });
    if (forced) {
      bound.Release(shown_);
    } else {
      bound.Bind(shown_, LiveSource());
    }
  }

  // Something now covers the net: its own value takes what it showed, and the
  // member and whatever is bound from it name that, where a covering value then
  // lands. Each change to the source from then on runs what `follow` makes of
  // it, so the positions nothing covers go on showing it.
  void Cover(
      const std::function<OwnedCall(const ErasedReference& source)>& follow) {
    BoundMembers& bound = current_runtime().Bound();
    if (!started_ || bound.Forced(shown_)) {
      return;
    }
    own_value_->AssignBytes(Shown());
    bound.Force(shown_, own_, follow);
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

// Some of a declared net's own positions and the physical net it reaches there,
// with which of the name's own contributions drive them. A net's reaches cover
// it exactly and in order, so a name whose positions were never split reaches
// one. A contribution drives every position of a reach or none of it, since
// attaching one cuts the reaches where its positions end.
struct NetReach {
  std::shared_ptr<PhysicalNet> physical;
  std::uint32_t net_offset{};
  std::uint32_t count{};
  std::vector<std::uint32_t> covering;
};

// One contribution to a resolution -- a driver's, or the one a net type makes
// to its own: the positions it drives, from `first`, the value it last gave
// there, and the strength it drives at (LRM 28.11). A position a driver does
// not drive is no input of its resolution there (LRM 6.6.7), so a contribution
// holds no others. Strength rides the contribution rather than the resolved
// value, because all it decides is which contribution determines a position,
// and nothing that reads a net asks how strongly it got there.
struct NetContribution {
  std::uint32_t first{};
  support::StrengthLevel strength{};
  NetPositions positions;
};

// A declared net as the physical nets it reaches see it: which of them it
// reaches and over which of its own positions, what each of its own drivers
// contributes, and the two things a resolution asks of it -- its contributions
// as they stand over some of its positions, and to show what those positions
// resolved to. What type the net's own value is does not appear, which is what
// lets nets of different types reach one physical net.
//
// Positions resolve together exactly where they agree on what reaches them
// (LRM 6.6.7 resolves each on its own): a connection placing some of them in
// a resolution with another name's cuts the reaches where its positions end,
// and so does a driver of some of them, so a write by one driver recomputes
// only the positions it drives, from only what else drives them.
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

  // Adds a contribution over `count` of this name's positions from `first`, at
  // `strength`, holding high impedance until its driver first gives, and
  // answers which it is. The reaches are cut where its positions end.
  auto Attach(
      support::StrengthLevel strength, std::uint32_t first, std::uint32_t count)
      -> std::uint32_t;

  // The positions the contribution `index` holds, for its driver to give into.
  [[nodiscard]] auto PositionsOf(std::uint32_t index) -> NetPositions&;
  [[nodiscard]] auto FirstOf(std::uint32_t index) const -> std::uint32_t;

  // Fixes which names reach the positions this name reaches: no connection
  // joins or cuts them, and no driver attaches, from here on.
  void SettleReaches();

  // Whether the contribution `index` alone decides every position it drives:
  // nothing else drives them, no other name reaches them, and the net type
  // contributes there at high impedance, which takes no part (LRM 28.12.1),
  // and keeps nothing it was driven with (LRM 6.6.4). Every fold leaves a lone
  // contribution as it stands, so such positions show it as it is.
  [[nodiscard]] auto DecidesAlone(std::uint32_t index) const -> bool;

  // How many contributions this name's drivers make.
  [[nodiscard]] auto ContributionCount() const -> std::size_t;

  // How many positions this name has.
  [[nodiscard]] auto Count() const -> std::uint32_t;

  // Recomputes every resolution some of `within` takes part in.
  void Reresolve(RuntimeEffects& runtime, value::BitPositions within);

  // Whether a procedural continuous assignment in force covers any of the
  // positions this name reaches, or any of `within` (LRM 10.6.2).
  [[nodiscard]] auto Overridden() const -> bool;
  [[nodiscard]] auto OverriddenWithin(value::BitPositions within) const -> bool;

  // Puts `held` of this name's positions under a procedural continuous
  // assignment at `level`, drives it with `forced`, a value as wide as the
  // name, and takes some of them back out (LRM 10.6.2). What a takeover
  // overrides is the drivers of the physical nets those positions are, so it
  // shows under every name reaching them; one over positions of several
  // physical nets is one takeover of each, under one generation of this
  // name's. Driving answers the positions the takeover spans, none where it no
  // longer holds any, and ending whether there was anything to end.
  auto BeginTakeover(support::TakeoverLevel level, value::BitPositions held)
      -> std::uint32_t;
  auto DriveTakeover(
      support::TakeoverLevel level, std::uint32_t generation,
      const value::ConstIntegralView& forced)
      -> std::optional<value::BitPositions>;
  auto EndTakeover(support::TakeoverLevel level, value::BitPositions released)
      -> bool;

 protected:
  NetName();
  ~NetName();

 private:
  friend class PhysicalNet;

  // One takeover of this name's, as the takeovers of each physical net it
  // reaches carry it: the generation each gave it there.
  struct TakeoverPart {
    PhysicalNet* physical;
    std::uint32_t net_offset;
    std::uint32_t generation;
  };
  struct NameTakeover {
    support::TakeoverLevel level{};
    std::uint32_t generation{};
    std::vector<TakeoverPart> parts;
  };

  // One reach, the name's own, until a connection or a driver of some of these
  // positions cuts them where its positions end, since what resolves together
  // is what agrees on what reaches it.
  std::vector<NetReach> reaches_;
  std::vector<NetContribution> contributions_;
  // Whether any of this net's own contributions sits at a given level, which a
  // resolution reads from every name reaching it. Recording it per name is what
  // leaves a driver attached after a connection needing nothing propagated, and
  // a connection made after one needing nothing rebuilt.
  OccupiedLevels occupied_{};
  std::vector<NameTakeover> takeovers_;
  std::uint32_t takeover_generation_ = 0;
  // Set once what reaches these positions is fixed, after which no driver
  // attaches.
  bool settled_ = false;

  // Folds each of this name's contributions at `level` that drives the
  // positions `group` holds into `group` under `fold`, those positions
  // beginning at `offset` among this name's own.
  void FoldContributions(
      support::StrengthLevel level, value::NetResolution fold,
      std::uint32_t offset, NetPositions& group) const;

  // Forgets each of this name's takeovers at `level` that holds no position of
  // any physical net any more: a later one at its level took them all, or a
  // release ended it, whether or not its evaluation runs again (LRM 10.6.2).
  void ForgetEndedTakeovers(support::TakeoverLevel level);

  // Shows `positions` over this name's own from `offset`, publishing under
  // this name if what it shows moved (LRM 23.3.3.7).
  virtual void ShowPositions(
      RuntimeEffects& runtime, std::uint32_t offset,
      const NetPositions& positions) = 0;

  // The reach holding `position`.
  [[nodiscard]] auto ReachHolding(std::uint32_t position) const -> std::size_t;

  // The reaches holding some of `within`, as the first and one past the last.
  [[nodiscard]] auto ReachesWithin(value::BitPositions within) const
      -> std::pair<std::size_t, std::size_t>;

  // Which of this net's reaches begins at `position`, cutting what it reaches
  // so that one does and so that it covers no more than `within` positions. A
  // connection or a driver names some of a net's own positions; what the net
  // reaches there was fixed by whatever came before, so the two are made to
  // agree here.
  auto ReachAt(std::uint32_t position, std::uint32_t within) -> std::size_t;

  // Cuts a physical net in two, `cut` positions from its start, and gives every
  // name reaching it the two reaches that replace the one, each driven by what
  // drove the one.
  static void CutPhysical(
      std::shared_ptr<PhysicalNet> physical, std::uint32_t cut);

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
      NetContribution own, value::FourStateBit own_fill,
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
  // resolve here, from `from`.
  auto BeginTakeover(support::TakeoverLevel level, value::BitPositions held)
      -> std::uint32_t;
  auto DriveTakeover(
      support::TakeoverLevel level, std::uint32_t generation,
      const value::ConstIntegralView& forced, std::uint32_t from) -> bool;
  auto EndTakeover(support::TakeoverLevel level, value::BitPositions released)
      -> bool;

 private:
  friend class NetName;

  // Adds a name over these positions, at the offset among its own that the
  // first of them stands at. A name already here at that offset is already
  // saying this, which is what an alias repeated in two statements states.
  void Admit(const NetPlacement& placement);

  // These positions cut in two at `cut`, which keeps the low part and hands
  // back the high one. Every name here covers the whole of these positions, so
  // each is placed in both halves, the high one at that name's own offset
  // advanced by the cut. A cut is what a connection reaching part of these
  // positions asks for: the part it reaches goes on to resolve with whatever it
  // is coupled to, and the rest of these positions is a resolution of its own.
  [[nodiscard]] auto Split(std::uint32_t cut) -> std::shared_ptr<PhysicalNet>;

  NetContribution own_;
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

// Every net, whichever way its value is arrived at: storage nothing writes,
// observed as a variable is, and told as the design is built of the object
// its one driver carries.
class Net : public Observable {
 public:
  Net(const Net&) = delete;
  auto operator=(const Net&) -> Net& = delete;
  Net(Net&&) = delete;
  auto operator=(Net&&) -> Net& = delete;

  // States, while the design resolves, that what the net's driver of the whole
  // of it gives is the whole of `source`, unchanged: a variable, a net, or an
  // input port declared as a variable, of the net's own type. Where that
  // driver alone decides the net, the net then shows `source` where it lies
  // (LRM 23.3.3, 4.8). A variable is named as a reference to the whole of it
  // names it.
  template <class V>
  void DriveFrom(Var<V>* source)
    requires(!DynamicallySized<V>)
  {
    carried_->CarryVariable(WholeReference(*source));
  }
  template <class HeldAs>
  void DriveFrom(TailCell<HeldAs>* source) {
    carried_->CarryVariable(WholeReference(*source));
  }
  template <class V>
  void DriveFrom(ResolvedNet<V>* source) {
    carried_->CarryMember(&source->Member());
  }
  template <class V>
  void DriveFrom(UnresolvedNet<V>* source) {
    carried_->CarryMember(&source->Member());
  }
  void DriveFrom(ErasedReference* source) {
    carried_->CarryMember(source);
  }
  template <class V>
  void DriveFrom(VariableRef<V>* source) {
    DriveFrom(&source->AsMember());
  }

  // The turn of the net's drivers has come at time zero: answers whether the
  // net shows the object its one driver carries from here on, which leaves
  // that driver nothing to give.
  auto ShowSource() -> bool {
    return carried_->ShowSource();
  }

 protected:
  explicit Net(CarriedObjectSink& carried);
  ~Net();

 private:
  CarriedObjectSink* carried_;
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
class ConnectableNet : public Net {
 public:
  ConnectableNet(const ConnectableNet&) = delete;
  auto operator=(const ConnectableNet&) -> ConnectableNet& = delete;
  ConnectableNet(ConnectableNet&&) = delete;
  auto operator=(ConnectableNet&&) -> ConnectableNet& = delete;

  [[nodiscard]] auto Name() -> NetName&;

  // Attaches a new driver giving a value of `width` positions, which lands on
  // the net's positions from `first`, at the strength its source drives at,
  // and answers where the net keeps it, which never moves. Only the positions
  // of the value that land in the net and within the `reached_count` from
  // `reached_first` are driven: the run of the part the driver names that the
  // net has, which a select's position may leave short of the whole value
  // (LRM 7.4.5, 11.5.1). Its contribution starts at the non-driving one, so a
  // driver that has not yet driven leaves the resolution exactly as it was --
  // attaching is not itself an act of driving.
  // The positions are what a driver names of the net, whatever type its value
  // is, so the net attaches one the same way whatever type its own value is.
  auto AttachDriverSlot(
      std::int64_t strength, std::int64_t first, std::int64_t width,
      std::int64_t reached_first, std::int64_t reached_count,
      base::SourceLocation at) -> DriverSlot&;

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
  // The other net may be of another type, as nets of two widths joined over
  // some of their positions are, or an unpacked aggregate one: what the two
  // share is positions, which carry no type.
  //
  // Each side joins only what lies within the run of the part it names that
  // its net has, from `*_reached_first`, which a select's position may leave
  // short of the whole part (LRM 7.4.5, 11.5.1), so the positions joined are
  // those both sides have.
  void Join(
      ConnectableNet* other, std::int64_t here, std::int64_t there,
      std::int64_t count, std::int64_t here_reached_first,
      std::int64_t here_reached_count, std::int64_t there_reached_first,
      std::int64_t there_reached_count, base::SourceLocation at);

 protected:
  ConnectableNet(NetName& name, DrivenNet& driven);
  ~ConnectableNet();

 private:
  NetName* name_;
  DrivenNet* driven_;
  // The drivers this net has issued, which growth must not move.
  std::deque<DriverSlot> slots_;
};

// What a net resolving positions is, whatever type it shows them as: a name
// for some positions, the contributions its drivers make to the positions each
// drives, and the value it shows -- what those positions last resolved to, or
// the object its one driver carries. A procedural continuous assignment may
// override what the drivers determine (LRM 10.6.2). `Self` is the net, which
// says how what its positions resolve to is laid into the value it shows and
// how the positions of a value of its type are read out.
//
// What resolves is not the net. A bidirectional connection and an `alias` each
// state that positions across several nets are the same physical net
// (LRM 23.3.3.7, 10.11), so what resolves is that and every name reaching it
// shows what it produced. A net no connection reached is the one name of the
// physical nets covering it, which is the same walk over them.
template <class Self, NetValue T>
class PositionalNet : public ConnectableNet {
 public:
  using HeldAs = typename StorageFor<T>::Type;

  PositionalNet(const PositionalNet&) = delete;
  auto operator=(const PositionalNet&) -> PositionalNet& = delete;
  PositionalNet(PositionalNet&&) = delete;
  auto operator=(PositionalNet&&) -> PositionalNet& = delete;

  // Where what the net shows lies, as a reference names it.
  [[nodiscard]] auto Shown() const noexcept -> const void* {
    return showing_.Shown();
  }
  [[nodiscard]] auto ShownThrough() const noexcept -> void* const* {
    return showing_.ShownThrough();
  }
  [[nodiscard]] auto Member() const noexcept -> const ErasedReference& {
    return showing_.Member();
  }
  // Where a wait on the net enrols: whatever its member names, reached through
  // the member, so the wait follows it as it is bound and forced.
  [[nodiscard]] auto ReportsTo() const -> WatchedPlace {
    return WatchedPlace::Through(Member());
  }

  // Puts the positions `first` to `first + count` under a procedural
  // continuous assignment and takes them back out (LRM 10.6.2). A `force` on a
  // net overrides every driver rather than joining them, so what these change
  // is what the positions show, never the contributions -- which go on being
  // updated underneath and are what they answer with again once released.
  //
  // What is forced is the physical nets those positions are, so the value
  // shows under every name reaching them.
  auto BeginTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at) -> std::int64_t {
    const base::SourceLocationScope bound(at);
    return std::int64_t{name_.BeginTakeover(
        TakeoverLevelOf(level), PositionsNamed(first, count))};
  }

  void EndTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at) {
    const base::SourceLocationScope bound(at);
    const value::BitPositions released = PositionsNamed(first, count);
    if (!name_.EndTakeover(TakeoverLevelOf(level), released)) {
      return;
    }
    // The object its driver carries is what such a net shows once nothing
    // covers it, from the driver's turn on; before that it shows what an
    // undriven net does. Any other net shows what its drivers produced
    // meanwhile (LRM 10.6.2), which their contributions took while covered.
    if (!name_.Overridden() && Decision() == NetDecision::kShowsSource &&
        showing_.Started()) {
      showing_.Uncover();
      return;
    }
    name_.Reresolve(current_runtime(), released);
  }

  // Fixes, once, what the net's declared net type states, one entry per
  // resolution: the scalar it contributes at each position and the strength
  // it holds it at (LRM 6.7.1), which truth table resolves contributions of
  // equal strength -- tri-state, wired-and or wired-or (LRM 6.6.1 Table 6-2,
  // 6.6.3 Tables 6-3 and 6-4) -- and whether that contribution keeps what the
  // drivers last decided, which is how a net stores a value (LRM 6.6.4). The
  // net is then a readable observable before any driver attaches, showing what
  // that contribution resolves to. Installing twice is a lowering defect.
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

  // Attaches a new driver giving a value of `width` positions that lands on
  // the net's positions from `first`, driving those of them within the
  // `reached_count` from `reached_first`, and answers a handle to it giving
  // them as a value of type `U`.
  template <NetValue U>
  auto AttachDriver(
      std::int64_t strength, std::int64_t first, std::int64_t width,
      std::int64_t reached_first, std::int64_t reached_count,
      base::SourceLocation at) -> Driver<U> {
    return Driver<U>(AttachDriverSlot(
        strength, first, width, reached_first, reached_count, at));
  }

 protected:
  PositionalNet() : ConnectableNet(name_, driven_) {
  }
  ~PositionalNet() = default;

  // Has the takeover at `level` of `generation` record `forced`, the positions
  // of what it evaluated to, as many as the net has, and shows whatever the
  // highest level in effect then holds.
  auto DriveTakeoverPositions(
      std::int64_t level, std::int64_t generation,
      const value::ConstIntegralView& forced) -> bool {
    const bool was_covered = name_.Overridden();
    const std::optional<value::BitPositions> spans = name_.DriveTakeover(
        TakeoverLevelOf(level), TakeoverGenerationOf(generation), forced);
    if (!spans.has_value()) {
      return false;
    }
    if (!was_covered) {
      Covered();
    }
    name_.Reresolve(current_runtime(), *spans);
    return true;
  }

  // The contribution `index` holds, for the net to set from a value.
  [[nodiscard]] auto ContributionPositions(std::uint32_t index)
      -> NetPositions& {
    return name_.PositionsOf(index);
  }

  // What the net shows and where, as a net of every kind keeps it.
  [[nodiscard]] auto Showing() -> NetShowing<HeldAs>& {
    return showing_;
  }
  [[nodiscard]] auto Showing() const -> const NetShowing<HeldAs>& {
    return showing_;
  }

 private:
  // This net as the physical nets it reaches see it. It is a part of the net
  // rather than something the net is, so the net stays the one thing a wait
  // registers on, at the address its storage has.
  class NamePart final : public NetName {
   public:
    explicit NamePart(PositionalNet& net) : net_(&net) {
    }

   private:
    void ShowPositions(
        RuntimeEffects& runtime, std::uint32_t offset,
        const NetPositions& positions) override {
      net_->ShowPositions(runtime, offset, positions);
    }

    PositionalNet* net_;
  };

  // This net as its drivers see it, a part of the net for the reason its name
  // is.
  class DrivenPart final : public DrivenNet {
   public:
    explicit DrivenPart(PositionalNet& net) : net_(&net) {
    }

   private:
    void Give(
        std::uint32_t index, const value::ConstIntegralView& given,
        std::uint32_t skip) override {
      net_->Give(index, given, skip);
    }
    void CarryVariable(const ErasedReference& variable) override {
      RequireUnsettled(net_->decision_);
      net_->showing_.RecordVariable(variable);
    }
    void CarryMember(const ErasedReference* member) override {
      RequireUnsettled(net_->decision_);
      net_->showing_.RecordMember(member);
    }
    auto ShowSource() -> bool override {
      return net_->StartShowingSource();
    }

    PositionalNet* net_;
  };

  [[nodiscard]] auto Itself() -> Self& {
    return static_cast<Self&>(*this);
  }

  // The declaration's own contribution, over as many positions as the net's
  // type numbers, which the net then shows.
  void Install(
      std::int64_t fill, std::int64_t strength, value::NetResolution resolution,
      OwnContribution own_kind) {
    if (installed_) {
      throw InternalError(
          "ResolvedNet: the net's declaration is installed twice");
    }
    installed_ = true;
    // Nothing is yet enrolled to be told of it.
    Itself().LayPositions(
        0,
        name_.Declare(
            static_cast<support::StrengthLevel>(strength), NetFillOf(fill),
            resolution, own_kind, Itself().PositionCount()),
        [](const Change&) {});
  }

  // Shows `positions` over the net's own from `offset`, telling whoever waits
  // on the net what moved (LRM 9.4.2).
  void ShowPositions(
      RuntimeEffects& runtime, std::uint32_t offset,
      const NetPositions& positions) {
    Itself().LayPositions(offset, positions, [&](const Change& change) {
      runtime.WakeParkedOn(*this, PartPath{}, change);
    });
  }

  // An override has come to cover the net. A driver keeps its contribution
  // whatever covers it, so only a net showing the object its driver carries
  // has anything to do: it goes back to its own storage, so the object is
  // never forced with it, and that driver's contribution then holds what the
  // object holds, which the cover keeps following the object (LRM 10.6.2).
  void Covered() {
    if (Decision() != NetDecision::kShowsSource) {
      return;
    }
    showing_.Cover([this](const ErasedReference& source) {
      return OwnedCall([this, &source] {
        Itself().TakeIntoDriverContribution(source.storage);
        name_.Reresolve(current_runtime(), {.lsb = 0, .width = name_.Count()});
      });
    });
    Itself().TakeIntoDriverContribution(Itself().OwnBytes());
  }

  // What decides this net, settled the first time it is asked: the object its
  // one driver carries, where that driver alone decides every position of the
  // net and carries one; otherwise what its contributions resolve to, which a
  // contribution alone deciding its positions gives as it is.
  [[nodiscard]] auto Decision() -> NetDecision {
    if (!decision_.has_value()) {
      name_.SettleReaches();
      alone_.resize(name_.ContributionCount());
      for (std::uint32_t index = 0; index < alone_.size(); ++index) {
        alone_[index] = name_.DecidesAlone(index);
      }
      const bool one_driver =
          name_.ContributionCount() == 1 && name_.FirstOf(0) == 0 &&
          name_.PositionsOf(0).Count() == name_.Count() && alone_.front();
      decision_ = showing_.Decide(one_driver);
    }
    return *decision_;
  }

  // The turn of this net's drivers has come at time zero. One that shows the
  // object its one driver carries does from here on, and that driver has
  // nothing left to give.
  auto StartShowingSource() -> bool {
    if (Decision() != NetDecision::kShowsSource) {
      return false;
    }
    showing_.Start(name_.Overridden());
    return true;
  }

  // The driver of the contribution `index` gives `given`, of which the
  // positions from `skip` are its own. They show at once where it alone
  // decides them and nothing covers them, and otherwise resolve again with
  // whatever else drives them (LRM 6.6.7).
  void Give(
      std::uint32_t index, const value::ConstIntegralView& given,
      std::uint32_t skip) {
    if (Decision() == NetDecision::kShowsSource) {
      throw InternalError(
          "ResolvedNet: a driver whose net shows the object it carries is "
          "written, though its drive ends at its turn");
    }
    RuntimeEffects& runtime = current_runtime();
    NetPositions& contribution = name_.PositionsOf(index);
    value::Extract(contribution.MutableView(), given, skip);
    const value::BitPositions drives{
        .lsb = name_.FirstOf(index), .width = contribution.Count()};
    if (alone_[index] && !name_.OverriddenWithin(drives)) {
      ShowPositions(runtime, name_.FirstOf(index), contribution);
      return;
    }
    name_.Reresolve(runtime, drives);
  }

  NetShowing<HeldAs> showing_{*this};
  bool installed_ = false;
  std::optional<NetDecision> decision_;
  // Whether each contribution alone decides the positions it drives, settled
  // with the decision.
  std::vector<bool> alone_;
  NamePart name_{*this};
  DrivenPart driven_{*this};
};

// A net of an integral type, whose positions are its value's bits.
template <NetValue T>
  requires BitAddressed<T>
class ResolvedNet<T> : public PositionalNet<ResolvedNet<T>, T> {
  using Positional = PositionalNet<ResolvedNet<T>, T>;

 public:
  using typename Positional::HeldAs;

  // A net is built told how many positions its declared type has, which the
  // type laying its value out does not say where it lays out a narrower one;
  // one of a type that does say it is built at that. A net of a value wider
  // than a word lays the words out after itself at that width.
  ResolvedNet()
    requires(!value::HeldAsWords<T>);
  explicit ResolvedNet(std::uint64_t count);

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet();

  // What the net shows, of a type it holds as the value itself.
  [[nodiscard]] auto Get() const noexcept -> const T&
    requires(!value::HeldAsWords<T>)
  {
    return *static_cast<const T*>(this->Showing().Shown());
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
        Trigger(
            WatchedPlace::Through(this->Member()), Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto SampledGet(base::SourceLocation at) const -> const T& {
    const base::SourceLocationScope bound(at);
    return SampledOf(sampled_.get());
  }
  // Assigns what the net shows to `out`, as its sampled value takes it.
  void TakeShownInto(T& out) const {
    out = held_.ToOwned(this->Showing().Shown());
  }

  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& value,
      base::SourceLocation at) -> bool {
    const base::SourceLocationScope bound(at);
    return this->DriveTakeoverPositions(
        level, generation, value.AsWords().View());
  }

  // The same of a value handed as its bytes, as wide as what the net holds.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes,
      base::SourceLocation at) -> bool
    requires value::TakesBytesInPlace<T>
  {
    const base::SourceLocationScope bound(at);
    return this->DriveTakeoverPositions(
        level, generation, value::WideViewAt(bytes, count_, T::kDomain));
  }

 private:
  friend Positional;

  [[nodiscard]] auto PositionCount() const -> std::uint32_t {
    return count_;
  }

  // Writes the positions into the net's value where they lie, and has `tell`
  // tell what moved, which what is kept of them from before says.
  template <class Tell>
  void LayPositions(
      std::uint32_t offset, const NetPositions& positions, Tell tell) {
    auto&& own = Own();
    WriteOriginal<std::remove_cvref_t<decltype(own)>> kept(
        own, value::BitPositions{.lsb = offset, .width = positions.Count()});
    WritePositions(own, offset, positions);
    if (const std::optional<Change> change = kept.ChangeTo(own)) {
      tell(*change);
    }
  }

  // Sets the one driver's contribution to the value lying at `bytes`, of the
  // net's own type.
  void TakeIntoDriverContribution(const void* bytes) {
    NetPositions& contribution = this->ContributionPositions(0);
    if constexpr (value::HeldAsWords<T>) {
      value::Extract(
          contribution.MutableView(),
          value::WideViewAt(bytes, count_, T::kDomain), 0);
    } else {
      value::Extract(
          contribution.MutableView(),
          static_cast<const T*>(bytes)->AsWords().View(), 0);
    }
  }

  [[nodiscard]] auto OwnBytes() -> void* {
    return held_.Bytes();
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

  // How many positions the declared type has.
  std::uint32_t count_;
  // What the net's sampled value takes what the net shows through.
  using TakenShown = TakenFrom<ResolvedNet, T, &ResolvedNet::TakeShownInto>;
  // The net's sampled value, absent until something samples it.
  std::unique_ptr<SampledValueOf<TakenShown>> sampled_;
  // The net's own value, last, so words it lays out after itself lie where
  // the net ends.
  HeldAs held_;
};

// A net of an unpacked aggregate a net type resolves (LRM 6.7.1): a structure,
// an untagged union, or a fixed-size array, every bit of which is a bit of an
// integral value. It resolves position by position as an integral net does,
// each position of each part on its own (LRM 6.6.7), numbered part by part as
// a procedural continuous assignment numbers them, and lays what its positions
// resolve to into the aggregate it shows in place, reaching only the parts
// they lie in. So a driver of one element costs that element whatever the
// aggregate's size.
template <NetValue T>
  requires(!BitAddressed<T> && ResolvedNetValue<T>)
class ResolvedNet<T> : public PositionalNet<ResolvedNet<T>, T> {
  using Positional = PositionalNet<ResolvedNet<T>, T>;

 public:
  using typename Positional::HeldAs;

  ResolvedNet()
    requires(!value::LaidOutByType<T>);
  ResolvedNet(
      const value::ElementType& held, const value::NetWitnessTable& witnesses)
    requires value::LaidOutByType<T>;

  ResolvedNet(const ResolvedNet&) = delete;
  auto operator=(const ResolvedNet&) -> ResolvedNet& = delete;
  ResolvedNet(ResolvedNet&&) = delete;
  auto operator=(ResolvedNet&&) -> ResolvedNet& = delete;
  ~ResolvedNet();

  // What the net shows, of a type it holds as the value itself.
  [[nodiscard]] auto Get() const noexcept -> const T&
    requires(!value::LaidOutByType<T>)
  {
    return *static_cast<const T*>(this->Showing().Shown());
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
          T unknown = held_.AsOwned();
          numbering_.WritePositions(
              HandleTo(unknown),
              NetPositions(PositionCount(), value::FourStateBit::kUnknown)
                  .View(),
              0, PositionCount());
          return unknown;
        },
        Trigger(
            WatchedPlace::Through(this->Member()), Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto SampledGet(base::SourceLocation at) const -> const T& {
    const base::SourceLocationScope bound(at);
    return SampledOf(sampled_.get());
  }
  // Assigns what the net shows to `out`, as its sampled value takes it.
  void TakeShownInto(T& out) const {
    out = held_.ToOwned(this->Showing().Shown());
  }

  // What a force names of the net is positions of its value, which its type
  // numbers part by part, and it forces those of the physical nets they are.
  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& value,
      base::SourceLocation at) -> bool {
    return DriveTakeoverBytes(level, generation, HandleTo(value), at);
  }

  // The same of a value handed as its bytes, laid out by the net's type.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes,
      base::SourceLocation at) -> bool {
    const base::SourceLocationScope bound(at);
    NetPositions forced(
        this->Name().Count(), value::FourStateBit::kHighImpedance);
    numbering_.ToPositions(bytes, forced.MutableView());
    return this->DriveTakeoverPositions(level, generation, forced.View());
  }

 private:
  friend Positional;

  // How many positions the net's type numbers: every bit of its value is one
  // (LRM 6.7.1).
  [[nodiscard]] auto PositionCount() const -> std::uint32_t {
    return static_cast<std::uint32_t>(numbering_.PositionCount());
  }

  // Lays the positions into the aggregate where it lies, reaching only the
  // parts they lie in, which says whether what it shows moved, and has `tell`
  // tell what did: where a wait watches a part of it, the aggregate it showed
  // is kept until that is told, so each such wait is told whether its own part
  // changed.
  template <class Tell>
  void LayPositions(
      std::uint32_t offset, const NetPositions& positions, Tell tell) {
    const auto lay = [&] {
      return numbering_.WritePositions(
          OwnBytes(), positions.View(), offset, positions.Count());
    };
    if (!this->WatchedInside(PartPath{})) {
      if (lay()) tell(Change::Whole());
      return;
    }
    const value::ValueType& type = held_.Type();
    const value::KeptCopy before(type, OwnBytes());
    if (!lay()) return;
    if (const std::optional<Change> change =
            Change::Between(type, before.Bytes(), OwnBytes())) {
      tell(*change);
    }
  }

  // Sets the one driver's contribution to the positions of the value of the
  // net's own type lying at `bytes`.
  void TakeIntoDriverContribution(const void* bytes) {
    numbering_.ToPositions(bytes, this->ContributionPositions(0).MutableView());
  }

  // The net's own value where it lies, which its positions are written into.
  [[nodiscard]] auto OwnBytes() -> void* {
    return held_.Bytes();
  }

  [[no_unique_address]] NetPositionNumbering<T> numbering_;
  // What the net's sampled value takes what the net shows through.
  using TakenShown = TakenFrom<ResolvedNet, T, &ResolvedNet::TakeShownInto>;
  // The net's sampled value, absent until something samples it.
  std::unique_ptr<SampledValueOf<TakenShown>> sampled_;
  // The net's own value, last, so a value it lays out after itself lies where
  // the net ends.
  HeldAs held_;
};

// A net of a nettype with no resolution function (LRM 6.6.7): one of a type no
// net type resolves -- a real, a string, a class handle, or an aggregate
// holding one, none of which has bits a net type's table reads -- which only
// an input port written with no port kind declares (LRM 23.2.2.3). It has no
// strength levels and so no contribution of its own (LRM 28.12): its one driver
// decides it whole, where none does it shows its type's default, and a second
// driver is the design's error.
template <NetValue T>
class UnresolvedNet : public Net {
 public:
  using HeldAs = typename StorageFor<T>::Type;

  UnresolvedNet()
    requires(!value::LaidOutByType<T>);
  explicit UnresolvedNet(const value::ElementType& held)
    requires value::LaidOutByType<T>;

  // Fixes what the net shows where nothing drives it, `prototype`, its type's
  // default. Installing twice is a lowering defect.
  void InitializeUnresolved(const T& prototype) {
    if (installed_) {
      throw InternalError(
          "UnresolvedNet: the net's declaration is installed twice");
    }
    installed_ = true;
    value::RebuildFrom(nondriving_, [&] { return prototype; });
    held_.Assign(nondriving_);
  }

  UnresolvedNet(const UnresolvedNet&) = delete;
  auto operator=(const UnresolvedNet&) -> UnresolvedNet& = delete;
  UnresolvedNet(UnresolvedNet&&) = delete;
  auto operator=(UnresolvedNet&&) -> UnresolvedNet& = delete;
  ~UnresolvedNet();

  [[nodiscard]] auto Shown() const noexcept -> const void* {
    return showing_.Shown();
  }
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
  [[nodiscard]] auto ReportsTo() const -> WatchedPlace {
    return WatchedPlace::Through(Member());
  }

  // Arms the net to answer for its sampled value (LRM 16.5.1), whose default
  // is the type's default (LRM 6.8 Table 6-7), which is what the net shows
  // undriven.
  void ArmSampling() {
    if (sampled_ != nullptr) {
      return;
    }
    sampled_ = std::make_unique<SampledValueOf<TakenShown>>(
        TakenShown(*this), [&] { return nondriving_; },
        Trigger(WatchedPlace::Through(Member()), Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto SampledGet(base::SourceLocation at) const -> const T& {
    const base::SourceLocationScope bound(at);
    return SampledOf(sampled_.get());
  }
  void TakeShownInto(T& out) const {
    out = held_.ToOwned(showing_.Shown());
  }

  [[nodiscard]] auto ToOwned(const void* bytes) const -> T
    requires value::LaidOutByType<T>
  {
    return held_.ToOwned(bytes);
  }

  // Puts the net under a procedural continuous assignment and takes it back
  // out (LRM 10.6.2), positions of its value as its type numbers them.
  auto BeginTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at, TakePositionsOf<T> take_positions = {})
      -> std::int64_t {
    const base::SourceLocationScope bound(at);
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<Takeovers<T>>(take_positions);
    }
    return std::int64_t{takeovers_->Begin(
        TakeoverLevelOf(level), PositionsNamed(first, count))};
  }
  // Has the takeover at `level` record what it evaluated to, and shows
  // whatever the highest level in effect then holds.
  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& value,
      base::SourceLocation at) -> bool {
    const base::SourceLocationScope bound(at);
    if (takeovers_ == nullptr) {
      return false;
    }
    const bool was_covered = Overridden();
    if (!takeovers_->Drive(
            TakeoverLevelOf(level), TakeoverGenerationOf(generation), value)) {
      return false;
    }
    if (!was_covered) {
      Covered();
    }
    Reresolve(current_runtime());
    return true;
  }
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes,
      base::SourceLocation at) -> bool
    requires value::LaidOutByType<T>
  {
    return DriveTakeover(level, generation, held_.ToOwned(bytes), at);
  }
  void EndTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at) {
    const base::SourceLocationScope bound(at);
    if (takeovers_ == nullptr) {
      return;
    }
    takeovers_->End(TakeoverLevelOf(level), PositionsNamed(first, count));
    // The object its driver carries is what such a net shows once nothing
    // covers it, from the driver's turn on; before that it shows what an
    // undriven net does. Any other shows what its driver produced meanwhile
    // (LRM 10.6.2), which its contribution took while covered.
    if (!Overridden() && Decision() == NetDecision::kShowsSource &&
        showing_.Started()) {
      showing_.Uncover();
      return;
    }
    Reresolve(current_runtime());
  }

  // Attaches the net's one driver and returns its handle, which is the net's
  // own. Its value starts at the type's default, so attaching is not itself an
  // act of driving. A second driver is the design's error (LRM 6.6.7),
  // reported where it is written.
  auto AttachWholeDriver(base::SourceLocation at) -> Driver<T>&;

 private:
  friend class Driver<T>;

  // This net as what its driver carries is stated to.
  class SinkPart final : public CarriedObjectSink {
   public:
    explicit SinkPart(UnresolvedNet& net) : net_(&net) {
    }

   private:
    void CarryVariable(const ErasedReference& variable) override {
      RequireUnsettled(net_->decision_);
      net_->showing_.RecordVariable(variable);
    }
    void CarryMember(const ErasedReference* member) override {
      RequireUnsettled(net_->decision_);
      net_->showing_.RecordMember(member);
    }
    auto ShowSource() -> bool override {
      return net_->StartShowingSource();
    }

    UnresolvedNet* net_;
  };

  // An override has come to cover the net. Its one driver gave what the net
  // shows, which its contribution now holds, so that what it gives from here
  // on shows wherever the override holds no position (LRM 10.6.2). One showing
  // the object its driver carries goes back to its own storage, so the object
  // is never forced with it; the driver's contribution then holds what the
  // object holds, and the cover keeps it following the object.
  void Covered() {
    switch (Decision()) {
      case NetDecision::kResolution:
        return;
      case NetDecision::kOneDriver:
        TakeIntoDriverContribution(held_.Bytes());
        return;
      case NetDecision::kShowsSource:
        showing_.Cover([this](const ErasedReference& source) {
          return OwnedCall([this, &source] {
            TakeIntoDriverContribution(source.storage);
            Reresolve(current_runtime());
          });
        });
        TakeIntoDriverContribution(held_.Bytes());
        return;
    }
  }

  // Sets the one driver's contribution to the value lying at `bytes`, of the
  // net's own type, built where the contribution lies, since a value of it may
  // be larger than a frame keeps.
  void TakeIntoDriverContribution(const void* bytes) {
    value::RebuildFrom(
        DriverValue(), [&] -> T { return held_.ToOwned(bytes); });
  }

  // Shows the one driver's value, or the type's default where none drives
  // the net, displaced by a takeover (LRM 10.6.2), published only where it
  // moved (LRM 9.4.2).
  void Reresolve(RuntimeEffects& runtime) {
    value::SmallBox<T> shown(driven_.has_value() ? *driven_ : nondriving_);
    if (takeovers_ != nullptr) {
      takeovers_->Overlay(*shown);
    }
    if (held_.IsBitIdentical(*shown)) {
      return;
    }
    ReplaceHeld(
        held_, this->WatchedInside(PartPath{}), [&] { held_.Assign(*shown); },
        [&](const Change& change) {
          runtime.WakeParkedOn(*this, PartPath{}, change);
        });
  }

  // What decides this net, settled the first time it is asked: its one driver
  // wherever it has one, or the object that driver carries.
  [[nodiscard]] auto Decision() -> NetDecision {
    if (!decision_.has_value()) {
      decision_ = showing_.Decide(driven_.has_value());
    }
    return *decision_;
  }

  auto StartShowingSource() -> bool {
    if (Decision() != NetDecision::kShowsSource) {
      return false;
    }
    showing_.Start(Overridden());
    return true;
  }

  [[nodiscard]] auto Overridden() const -> bool {
    return takeovers_ != nullptr && takeovers_->Shows();
  }

  // Where a write of the net's one driver lands.
  [[nodiscard]] auto Landing() -> DriverLanding<T> {
    switch (Decision()) {
      case NetDecision::kResolution:
        return Contributing<T>{.contribution = &DriverValue()};
      case NetDecision::kOneDriver:
        if (Overridden()) {
          return Contributing<T>{.contribution = &DriverValue()};
        }
        return Showing<T>{.shown = &held_, .watched = this->HasMembers()};
      case NetDecision::kShowsSource:
        throw InternalError(
            "UnresolvedNet: a driver whose net shows the object it carries is "
            "written, though its drive ends at its turn");
    }
    throw InternalError("UnresolvedNet: unknown net decision");
  }

  void SetDriver(RuntimeEffects& runtime, const T& value) {
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
                  [&] { to.shown->Assign(value); });
            }},
        Landing());
  }

  // The same of a value handed as its bytes, laid out as the driver's value
  // is. A contribution already holding them moves nothing.
  void SetDriverBytes(RuntimeEffects& runtime, const void* bytes)
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
                  runtime, *this, *to.shown,
                  [&] { return to.shown->IsBitIdenticalTo(bytes); },
                  [&] { to.shown->AssignBytes(bytes); });
            }},
        Landing());
  }

  // A write into part of the driver's value is over, having changed it. A
  // contribution is shown again unless the write landed on the whole of it and
  // is known to have moved nothing.
  void DriverWrote(
      RuntimeEffects& runtime, const PartPath& reached, const Change& change) {
    std::visit(
        Overloaded{
            [&](const Contributing<T>&) {
              if (!reached.IsWhole() || !change.KnownUnchanged({})) {
                Reresolve(runtime);
              }
            },
            [&](const Showing<T>&) {
              runtime.WakeParkedOn(*this, reached, change);
            }},
        Landing());
  }

  // What the net's one driver last gave, where its writes land while it does
  // not show them itself.
  [[nodiscard]] auto DriverValue() -> T& {
    if (!driven_.has_value()) {
      throw InternalError("UnresolvedNet: the net has no driver attached");
    }
    return *driven_;
  }

  NetShowing<HeldAs> showing_{*this};
  // What the net shows where nothing drives it, its type's default.
  T nondriving_{};
  bool installed_ = false;
  std::optional<NetDecision> decision_;
  SinkPart sink_{*this};
  // The procedural continuous assignments the net has been put under (LRM
  // 10.6.2), absent until the first one starts.
  std::unique_ptr<Takeovers<T>> takeovers_;
  // What the net's one driver last gave, and the driver, absent until one
  // attaches.
  std::optional<T> driven_;
  std::optional<Driver<T>> driver_;
  using TakenShown = TakenFrom<UnresolvedNet, T, &UnresolvedNet::TakeShownInto>;
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
    : count_(static_cast<std::uint32_t>(count)), held_(HeldFor(count)) {
  // A value the type lays out wider than the net's positions holds nothing
  // above them, as every integral value holds nothing above its width; the
  // install lays the positions themselves.
  value::FillScalar(Own().MutableView(), value::FourStateBit::kZero);
  this->Showing().ShowOwn(held_);
}

template <NetValue T>
  requires BitAddressed<T>
ResolvedNet<T>::~ResolvedNet() = default;

template <NetValue T>
  requires(!BitAddressed<T> && ResolvedNetValue<T>)
ResolvedNet<T>::ResolvedNet()
  requires(!value::LaidOutByType<T>)
{
  this->Showing().ShowOwn(held_);
}

template <NetValue T>
UnresolvedNet<T>::UnresolvedNet()
  requires(!value::LaidOutByType<T>)
    : Net(sink_) {
  showing_.ShowOwn(held_);
}

template <NetValue T>
UnresolvedNet<T>::UnresolvedNet(const value::ElementType& held)
  requires value::LaidOutByType<T>
    : Net(sink_), held_(held) {
  showing_.ShowOwn(held_);
}

template <NetValue T>
UnresolvedNet<T>::~UnresolvedNet() = default;

template <NetValue T>
  requires(!BitAddressed<T> && ResolvedNetValue<T>)
ResolvedNet<T>::ResolvedNet(
    const value::ElementType& held, const value::NetWitnessTable& witnesses)
  requires value::LaidOutByType<T>
    : numbering_{.type = &held.Type(), .table = &witnesses}, held_(held) {
  this->Showing().ShowOwn(held_);
}

template <NetValue T>
  requires(!BitAddressed<T> && ResolvedNetValue<T>)
ResolvedNet<T>::~ResolvedNet() = default;

// The drive capability for a net resolving positions: a handle to one driver,
// which gives the positions it drives as one integral value of type `U` --
// the part of an integral net they are, or an aggregate part's positions laid
// out as one value (LRM 10.3.2). The net keeps the driver, so a source may
// hold the handle either by value or by the driver's address; a source's
// handle starts unbound and the net binds it when it attaches, during Resolve.
// A driver's value is given only through this handle, and where it lies is the
// net's to say (LRM 6.5).
template <NetValue U>
  requires BitAddressed<U>
class Driver<U> {
 public:
  using ValueType = U;

  Driver();
  explicit Driver(const DriverSlot& slot) : slot_(&slot) {
  }

  // Gives this driver's whole value, which every name its positions reach
  // publishes on a real change. It carries the capability family's store name
  // because a store through a handle reaches whatever that handle addresses,
  // and what this one addresses is a driver's value.
  void Set(const U& value) const {
    Slot().net->Give(Slot().contribution, value.AsWords().View(), Slot().skip);
  }

  // The same of a value handed as its bytes, as wide as the part it drives.
  void SetBytes(const void* bytes) const
    requires value::TakesBytesInPlace<U>
  {
    Slot().net->Give(
        Slot().contribution, value::WideViewAt(bytes, Slot().width, U::kDomain),
        Slot().skip);
  }

 private:
  [[nodiscard]] auto Slot() const -> const DriverSlot& {
    if (slot_ == nullptr) {
      throw InternalError("Driver: driver is not attached");
    }
    return *slot_;
  }

  const DriverSlot* slot_ = nullptr;
};

// The drive capability for a net of a nettype with no resolution function: a
// handle to the one driver of an `UnresolvedNet`, issued and kept by the net
// as a driver of a net resolving positions is.
//
// A source that drives only part of the net opens the same partial-write
// bracket a variable cell offers: the write lands in this driver's own value
// and reaches only the part it names, so the rest holds the type's default,
// since no other driver may drive it (LRM 6.6.7).
template <NetValue T>
  requires(!BitAddressed<T>)
class Driver<T> {
 public:
  using ValueType = T;

  Driver();
  explicit Driver(UnresolvedNet<T>& net) : net_(&net) {
  }

  // Gives this driver's whole value, which every name it reaches publishes on a
  // real change. It carries the capability family's store name because a store
  // through a handle reaches whatever that handle addresses, and what this one
  // addresses is a driver's value.
  void Set(const T& value) const {
    Driven().SetDriver(current_runtime(), value);
  }

  // The same of a value handed as its bytes, laid out as the driver's value is.
  void SetBytes(const void* bytes) const
    requires value::TakesBytesInPlace<T>
  {
    Driven().SetDriverBytes(current_runtime(), bytes);
  }

  // This driver's own value as it currently stands, of a type the net holds as
  // the value itself.
  [[nodiscard]] auto Get() const -> const T&
    requires(!DynamicallySized<T>)
  {
    return DriverValueAt(Landing());
  }

  // Where this driver's own value lies, as a reference names it.
  [[nodiscard]] auto Bytes() const -> void* {
    return DriverBytesAt(Landing());
  }
  // How the net holds a value it keeps after itself, which says how wide it
  // is or its type.
  [[nodiscard]] auto HeldStorage() const -> const typename StorageFor<T>::Type&
    requires DynamicallySized<T>
  {
    return Driven().held_;
  }

  [[nodiscard]] auto Mutate() const -> ScopedMutation<Driver>
    requires(!DynamicallySized<T>)
  {
    return ScopedMutation<Driver>{*this};
  }

  // The `MutationSink` surface, each answer read off where this driver's
  // writes land. A driver's write is never overridden: under an override it
  // lands in its contribution.
  [[nodiscard]] auto MutationStorage() const -> T&
    requires(!DynamicallySized<T>)
  {
    return DriverValueAt(Landing());
  }
  [[nodiscard]] auto Watched() const -> bool {
    return std::visit(
        Overloaded{
            [](const Contributing<T>&) { return true; },
            [](const Showing<T>& to) { return to.watched; }},
        Landing());
  }
  // A driver's write announces nowhere, so no interval accumulated it.
  void AfterModify(
      const PartPath& reached, const Change& change, bool accumulated) const {
    if (accumulated) {
      throw InternalError(
          "Driver: an interval accumulated a driver's write -- please "
          "report this as a bug");
    }
    Driven().DriverWrote(current_runtime(), reached, change);
  }
  // A net keeps no original of what it shows: it is resolved by the
  // scheduler, never modified by an interval.
  static auto WrittenPart() -> runtime::WrittenPart {
    return {};
  }

 private:
  [[nodiscard]] auto Landing() const -> DriverLanding<T> {
    return Driven().Landing();
  }

  [[nodiscard]] auto Driven() const -> UnresolvedNet<T>& {
    if (net_ == nullptr) {
      throw InternalError("Driver: driver is not attached");
    }
    return *net_;
  }

  UnresolvedNet<T>* net_ = nullptr;
};

// Defaulted here rather than where they are declared, for the reason a net's
// own constructor is: a source holding a driver slot constructs one.
template <NetValue U>
  requires BitAddressed<U>
Driver<U>::Driver() = default;

template <NetValue T>
  requires(!BitAddressed<T>)
Driver<T>::Driver() = default;

template <NetValue T>
auto UnresolvedNet<T>::AttachWholeDriver(base::SourceLocation at)
    -> Driver<T>& {
  const base::SourceLocationScope bound(at);
  RequireUnsettled(decision_);
  if (driver_.has_value()) {
    NetHasNoResolution();
  }
  driven_.emplace(nondriving_);
  return driver_.emplace(*this);
}

}  // namespace lyra::runtime
