#pragma once

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <memory>
#include <optional>
#include <ranges>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/interval_accumulator.hpp"
#include "lyra/runtime/laid_out_storage.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/sampled_value.hpp"
#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/tail.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/runtime/words_storage.hpp"
#include "lyra/support/region.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/container_class.hpp"
#include "lyra/value/container_lease.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/kept_copy.hpp"
#include "lyra/value/small_box.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::value {

template <typename K, typename V>
class AssociativeArray;
class RuntimeAssociativeArray;

}  // namespace lyra::value

namespace lyra::runtime {

namespace detail {

// Whether `Container` is an array whose size changes at run time, and whether
// a write one past its last element appends one, each of which an array laid
// out by its type states by its kind.
template <typename Container>
concept ChangesSizeByKind = requires { Container::kKind; } &&
                            value::ChangesSizeAtRunTime(Container::kKind);
template <typename Container>
concept AppendsPastItsLastByKind = requires { Container::kKind; } &&
                                   value::AppendsPastItsLast(Container::kKind);

template <typename Container>
inline constexpr bool kIsAssociative = false;
template <typename K, typename V>
inline constexpr bool kIsAssociative<value::AssociativeArray<K, V>> = true;
template <>
inline constexpr bool kIsAssociative<value::RuntimeAssociativeArray> = true;

}  // namespace detail

// An array whose size changes at run time -- a dynamic array, an associative
// array, a queue (LRM 7.5, 7.8, 7.10) -- and so moves and ends its elements
// while it lies where it is itself.
template <class T>
concept ResizableArray =
    detail::ChangesSizeByKind<T> || detail::kIsAssociative<T>;

// A value a wait reads by bit position (LRM 9.4.2): an integral value, of a
// type known where it is compiled or one held as its words at a width it was
// told. A write into one says which of its bits it reached; a write into any
// other value says only that it changed it.
template <class T>
concept BitAddressed = value::IntegralValue<T> || value::HeldAsWords<T>;

// A bit-addressed value that is one word in each plane it carries.
template <class T>
concept HeldInOneWord = value::IntegralValue<T> && T::kWords == 1;

// The bits of `storage` at `reached` kept as they stand before a write that
// reaches them, every bit of it where none are named.
template <BitAddressed T>
auto KeptBefore(
    const T& storage, std::optional<value::BitPositions> reached = std::nullopt)
    -> Change {
  if constexpr (HeldInOneWord<T>) {
    const auto read = storage.View();
    return Change::ReachingInWord(
        read.value[0], value::WordOrZero(read.unknown, 0),
        reached.value_or(value::BitPositions{.lsb = 0, .width = T::kWidth}));
  } else {
    const value::ConstIntegralView read = storage.View().AsDynamic();
    return Change::Reaching(
        read,
        reached.value_or(value::BitPositions{.lsb = 0, .width = read.width}));
  }
}

// The same bits of `storage` once the write has landed.
template <BitAddressed T>
void KeepAfter(Change& change, const T& storage) {
  if constexpr (HeldInOneWord<T>) {
    const auto read = storage.View();
    change.SetAfterInWord(read.value[0], value::WordOrZero(read.unknown, 0));
  } else {
    change.SetAfter(storage.View().AsDynamic());
  }
}

// What a write into part of an owner's storage writes through (LRM 11.5.1).
// The write reaches the owner's storage and lands its part there directly; the
// owner is told once, when the write is over, whether it changed. Four things
// that takes, named for the role rather than for either owner's vocabulary.
//
// `MutationStorage` is the storage itself, so a write reaches the part it
// names and disturbs nothing else. Two things follow: a write cannot lose one
// performed through another while it was open, and what writing one element
// costs does not scale with the size of the whole value.
//
// `Watched` is whether anything reads the answer. A variable cell no wait is
// enrolled on has no one to tell (LRM 4.3), so the write keeps nothing from
// before it. A driver of a net that resolves always has its net, which
// resolves again only where the driver's own contribution moved (LRM 6.5); the
// one driver deciding a net has the waits enrolled on that net, as a variable
// has its own.
//
// `AfterModify` tells the owner the write changed it: which part of the
// owner's storage the write reached, what can be said about which bits of that
// part moved, and whether the open interval accumulated the write.
//
// `WrittenPart` is where each modification the write makes is announced,
// immediately before it: the variable `MutationStorage` is a part of, at the
// path of that part, which keeps the part's original for the open interval and
// answers whether the interval accumulated the modification. A net's driver
// announces nowhere: a net is resolved by the scheduler, never modified by an
// interval (LRM 10.3, 10.4).
//
// Each sink claims the contract with a `static_assert(MutationSink<...>)`
// beside its own definition, the way a value type claims its `lyra::value`
// concepts; the bracket itself takes the sink unconstrained, because a sink
// names the handle in its own partial-write entry's return type and checking
// the constraint there would depend on the sink being complete.
struct WrittenPart;

template <class S>
concept MutationSink = requires(
    S sink, const PartPath& reached, const Change& change, bool accumulated) {
  typename S::ValueType;
  { sink.MutationStorage() } -> std::same_as<typename S::ValueType&>;
  { sink.Watched() } -> std::same_as<bool>;
  sink.AfterModify(reached, change, accumulated);
  { sink.WrittenPart() } -> std::same_as<WrittenPart>;
};

// A sink whose writes a procedural continuous assignment can override (LRM
// 10.6), which only a variable is. Three more things that takes.
//
// `OverridesWrites` is whether a write is overridden rather than going into
// the owner as it is: one is while an assignment shows at any of a variable's
// positions, or while what a release reestablished is still to be reevaluated.
//
// `OverriddenWriteStorage` is where an overridden write lands so that it is
// kept: what a variable driven continuously holds beneath the assignment
// covering it (LRM 10.6.2). Where there is none the write lands in a copy of
// the storage it keeps itself.
//
// `ApplyOverriddenWrite` is handed what such a write left when it is over, the
// whole value it landed in, for the owner to show at every position no
// assignment shows at.
template <class S>
concept OverridableSink =
    MutationSink<S> && requires(S sink, const typename S::ValueType& left) {
      { sink.OverridesWrites() } -> std::same_as<bool>;
      { sink.OverriddenWriteStorage() } -> std::same_as<typename S::ValueType*>;
      sink.ApplyOverriddenWrite(left);
    };

// The ordinal a position names an element at (LRM 7.4.5), whether it arrives
// as the position or as the ordinal read out of one.
[[nodiscard]] inline auto OrdinalOf(const value::Position& key)
    -> std::optional<std::int64_t> {
  return value::ReadPosition(key);
}
[[nodiscard]] inline auto OrdinalOf(std::optional<std::int64_t> key)
    -> std::optional<std::int64_t> {
  return key;
}

// The path a write reports of the one step to what `key` selects in a
// `Container`: the element at the ordinal the position names, or, in an
// associative array, the stop there (LRM 7.8), whatever type its index has.
template <typename Container, typename Key>
[[nodiscard]] auto StepTo(const Key& key) -> PartPath {
  if constexpr (detail::kIsAssociative<Container>) {
    return PartPath::Written(support::PartStep::kAssocElement, std::nullopt);
  } else {
    return PartPath::Written(support::PartStep::kElement, OrdinalOf(key));
  }
}

// The same step as a reference keeps it (LRM 13.5.2). A reference to an
// element of an array whose size changes goes on naming that element while
// the array grows, shrinks or is replaced around it, where it becomes an
// outdated reference to an element the array no longer holds (LRM 13.5.2,
// 7.10.3). The position it was taken at says nothing of where the element is
// later, so its path stops at the array, inexact.
template <typename Container, typename Key>
[[nodiscard]] auto RetainedStepTo(const Key& key) -> PartPath {
  if constexpr (ResizableArray<Container>) {
    return PartPath::Inexact();
  } else {
    return StepTo<Container>(key);
  }
}

// The positions of an array of `size` elements that a slice of `count`
// elements from `start` writes: those it names that the array holds, an
// element past the last skipped (LRM 7.4.5, 7.6), so they cost the elements
// written however far the slice reaches. None where `start` names no position.
[[nodiscard]] auto PositionsASliceWrites(
    std::optional<std::int64_t> start, std::int64_t count, std::int64_t size)
    -> std::ranges::iota_view<std::int64_t, std::int64_t>;

// Whether a step into the element `key` names can change the shape of
// `container` and not only one element of it: the entry of an associative
// array a write makes (LRM 7.8.7), or the element a write just past a queue's
// last appends (LRM 7.10.1). What the container held before such a step is
// kept whole.
template <typename Container, typename Key>
[[nodiscard]] auto StepCanReshape(const Container& container, const Key& key)
    -> bool {
  if constexpr (detail::AppendsPastItsLastByKind<Container>) {
    return StepTo<Container>(key) ==
           PartPath::Element(static_cast<std::int64_t>(container.Count()));
  } else {
    return detail::kIsAssociative<Container>;
  }
}

template <class Sink>
class ScopedMutation;

template <value::LyraValue T>
class VariableRef;

template <value::LyraValue T>
class Var;

template <class HeldAs>
class Cell;

// What only a variable something samples or takes over keeps. Almost no
// variable is either, so this exists only once one is, behind the variable. A
// write asks it whether a procedural continuous assignment overrides the write
// (LRM 10.6), and one through a reference to part of the variable, which knows
// the part's type and not the variable's, has it take the write where it
// lands.
class RareWriteState {
 public:
  RareWriteState();
  RareWriteState(const RareWriteState&) = delete;
  auto operator=(const RareWriteState&) -> RareWriteState& = delete;
  RareWriteState(RareWriteState&&) = delete;
  auto operator=(RareWriteState&&) -> RareWriteState& = delete;
  virtual ~RareWriteState();

  [[nodiscard]] virtual auto OverridesWrites() const -> bool = 0;

  // A write of part of the variable while a procedural continuous assignment
  // overrides its writes: `write` lands it in the part where it lies, and the
  // variable then shows each takeover over what it holds again, telling
  // whoever waits on it what moved. A takeover may hold some of a part and not
  // the rest (LRM 10.6.2), so the write shows wherever none holds.
  virtual void WritePart(OwnedCall write) = 0;
};

// What every variable is whatever it holds: the waits enrolled on it, and
// whether a procedural continuous assignment holds any of it. A write that
// knows the variable only as this -- one through a reference to a part of it,
// which knows the part's type and not the variable's -- asks it everything it
// needs.
class VariableCell : public Observable {
 public:
  VariableCell(const VariableCell&) = delete;
  auto operator=(const VariableCell&) -> VariableCell& = delete;
  VariableCell(VariableCell&&) = delete;
  auto operator=(VariableCell&&) -> VariableCell& = delete;

  // Written here rather than in the library's own source because every write
  // asks them, and for a variable nothing samples or takes over each is a null
  // test.
  [[nodiscard]] auto OverridesWrites() const -> bool {
    return rare_ != nullptr && rare_->OverridesWrites();
  }
  // A write of part of the variable, asked only where one overrides it.
  void WritePart(OwnedCall write);
  // Whether a write into the variable has anyone to tell what it did: a wait
  // enrolled here (LRM 4.3). A write a procedural continuous assignment
  // overrides lands aside, and the variable tells what changed once it is
  // handed what the write left (LRM 10.6), so the write has nothing to tell.
  [[nodiscard]] auto Watched() const -> bool {
    return HasMembers() && !OverridesWrites();
  }
  // Where a wait on the variable enrols: here, where every write to it is
  // told (LRM 9.4.2).
  [[nodiscard]] auto ReportsTo() const -> WatchedPlace;

 protected:
  VariableCell();
  ~VariableCell();

  // Every store asks whether one is installed, so it is read where the store
  // is written.
  [[nodiscard]] auto Rare() const -> RareWriteState* {
    return rare_.get();
  }
  void InstallRare(std::unique_ptr<RareWriteState> rare);

 private:
  std::unique_ptr<RareWriteState> rare_;
};

// What a sampled or taken-over variable keeps, over how the variable holds its
// value: its sampled value (LRM 16.5.1), and the procedural continuous
// assignments it has been put under (LRM 10.6). Each part is empty until its
// feature is used.
template <class HeldAs>
class CellRareState final : public RareWriteState {
 public:
  using Owned = typename HeldAs::Owned;

  // Assigns what the variable holds to `out`, a value of the owned type, as its
  // sampled value takes it.
  void TakeHeldInto(Owned& out) const {
    out = cell_->Held().AsOwned();
  }
  using TakenHeld =
      TakenFrom<CellRareState, Owned, &CellRareState::TakeHeldInto>;

  explicit CellRareState(Cell<HeldAs>& cell) : cell_(&cell) {
  }
  CellRareState(const CellRareState&) = delete;
  auto operator=(const CellRareState&) -> CellRareState& = delete;
  CellRareState(CellRareState&&) = delete;
  auto operator=(CellRareState&&) -> CellRareState& = delete;
  ~CellRareState() override;

  [[nodiscard]] auto OverridesWrites() const -> bool override {
    return takeovers_ != nullptr && takeovers_->OverridesWrites();
  }

  void WritePart(OwnedCall write) override;

  // Keeps the cell's sampled value from now on, its default sampled value
  // being what it holds now. Keeping one already kept changes nothing.
  void ArmSampling() {
    if (sampled_ != nullptr) {
      return;
    }
    sampled_ = std::make_unique<SampledValueOf<TakenHeld>>(
        TakenHeld(*this), [&] -> Owned { return cell_->Held().AsOwned(); },
        Trigger(cell_->ReportsTo(), Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto Sampled() const -> const SampledValueOf<TakenHeld>* {
    return sampled_.get();
  }

  [[nodiscard]] auto Takeovers(TakePositionsOf<Owned> take_positions)
      -> runtime::Takeovers<Owned>& {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<runtime::Takeovers<Owned>>(take_positions);
    }
    return *takeovers_;
  }
  [[nodiscard]] auto ExistingTakeovers() const -> runtime::Takeovers<Owned>* {
    return takeovers_.get();
  }

 private:
  Cell<HeldAs>* cell_;
  std::unique_ptr<SampledValueOf<TakenHeld>> sampled_;
  std::unique_ptr<runtime::Takeovers<Owned>> takeovers_;
};

template <class HeldAs>
CellRareState<HeldAs>::~CellRareState() = default;

// Replaces the whole of `storage` by `overwrite`, a write already known to
// change it, and answers what its waits are told. Of a bit-addressed value the
// change keeps its words from before, so a wait reading bits the new value
// leaves as they were is passed over; of any other value nothing but that it
// changed can be shown, so nothing is kept.
template <class T, class Overwrite>
auto ReplaceWhole(const T& storage, Overwrite overwrite) -> Change {
  if constexpr (BitAddressed<T>) {
    Change change = KeptBefore(storage);
    overwrite();
    KeepAfter(change, storage);
    return change;
  } else {
    overwrite();
    return Change::Whole();
  }
}

// A variable (LRM 6.8) as the library keeps one: what every variable is,
// followed by the storage of its value. That storage is the cell's last part,
// so a value whose size the library is told lies at the cell's tail. Every
// write a variable takes follows one protocol whatever it holds, which is
// written here once; what depends on the value is asked of the storage.
template <class HeldAs>
class Cell : public VariableCell {
 public:
  // A value of the cell's type that the cell keeps for itself: what an
  // overridden write lands in, what a takeover holds, what sampling retains.
  using Owned = typename HeldAs::Owned;

  Cell(const Cell&) = delete;
  auto operator=(const Cell&) -> Cell& = delete;
  Cell(Cell&&) = delete;
  auto operator=(Cell&&) -> Cell& = delete;

  // Commits a whole-variable write and, on a real change (LRM 4.3 update
  // event), wakes whoever waits through the engine. The engine is the ambient
  // one: it has the standing of a stack pointer, so a store does not carry it.
  //
  // A procedural continuous assignment overrides the procedural writes a cell
  // takes on its own at the positions it holds (LRM 10.6), so while one is in
  // effect the value arriving here is overridden there.
  void Set(const Owned& new_val);

  // The same write of what another cell of this storage holds.
  void SetFrom(const Cell& other) {
    if (OverridesWrites()) {
      ApplyOverriddenWrite(other.storage_.AsOwned());
      return;
    }
    StoreBy(
        [&] { return storage_.IsBitIdentical(other.storage_); },
        [&] { storage_.Assign(other.storage_); });
  }

  // The same write of a value handed as its bytes, laid out as what the cell
  // holds is, which the cell compares with and takes into the storage it has.
  void SetBytes(const void* bytes)
    requires requires(HeldAs& held, Owned& owned) {
      held.AssignBytes(bytes);
      owned.AssignBytes(bytes);
    }
  {
    if (OverridesWrites()) {
      ApplyOverriddenWriteBytes(bytes);
      return;
    }
    StoreBy(
        [&] { return storage_.IsBitIdenticalTo(bytes); },
        [&] { storage_.AssignBytes(bytes); });
  }

  // A whole value a procedural continuous assignment overrode, having
  // arrived while one is in effect (LRM 10.6): what a continuous driver
  // produced is kept beneath the takeovers, and the cell shows it at every
  // position no takeover shows at, which is the whole of a write where nothing
  // drives the cell continuously. A write of part of the cell arrives here as
  // the whole value it left. Kept out of line, so that a write nothing
  // overrides, which only asks whether something does, stays small enough to
  // be folded into whatever writes.
  [[gnu::noinline]] void ApplyOverriddenWrite(const Owned& written);

  // The same of a value handed as its bytes.
  void ApplyOverriddenWriteBytes(const void* bytes)
    requires requires(Owned& owned) { owned.AssignBytes(bytes); }
  {
    Owned written = storage_.AsOwned();
    written.AssignBytes(bytes);
    ApplyOverriddenWrite(written);
  }

  // Starts a procedural continuous assignment at `level` over the `count`
  // positions from `first` of the cell's value, as its type numbers them,
  // superseding whatever was driving them at that level, and answers with the
  // generation its evaluation carries (LRM 10.6). `take_positions` is how the
  // takeover shows what it holds: a type the library is compiled with needs
  // none handed, and any other has to be.
  auto BeginTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at, TakePositionsOf<Owned> take_positions = {})
      -> std::int64_t;

  // States what the takeover at `level` has evaluated to, as a value of the
  // cell's type of which the positions it holds are read. It reaches the cell
  // only where no higher level covers it, and is recorded either way so that
  // ending the level above it needs nobody to recompute. Answers whether the
  // evaluation offering the value still holds any position, which is how an
  // evaluation superseded by a later takeover, or ended by a `deassign` or
  // `release`, learns to stop.
  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const Owned& new_val,
      base::SourceLocation at) -> bool;

  // The same of a value handed as its bytes, laid out as what the cell holds
  // is.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes,
      base::SourceLocation at) -> bool
    requires requires(const HeldAs& held) {
      { held.ToOwned(bytes) } -> std::same_as<Owned>;
    };

  // Ends the procedural continuous assignment at `level` over the `count`
  // positions from `first`. Where a lower takeover or a continuous driver lies
  // beneath, it is reestablished, and each position released shows what it
  // holds once its reevaluation runs; any other variable is left exactly as it
  // stands, which is what a released variable keeps (LRM 10.6.2).
  void EndTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at);

  // Whether a procedural continuous assignment holds any position of the cell.
  [[nodiscard]] auto TakenOver() const -> bool;

  // Where a write a procedural continuous assignment overrides lands so that
  // it is kept: what the cell's continuous driver last produced, held beneath
  // the assignment. None where nothing drives the cell continuously, or
  // nothing covers it.
  [[nodiscard]] auto OverriddenWriteStorage() const -> Owned* {
    const CellRareState<HeldAs>* rare = ExistingRareState();
    Takeovers<Owned>* takeovers =
        rare == nullptr ? nullptr : rare->ExistingTakeovers();
    return takeovers == nullptr ? nullptr : takeovers->Beneath();
  }

  // Arms the cell to answer for its sampled value (LRM 16.5.1). A static
  // variable's default sampled value is the value its declaration assigns,
  // which is in the cell once time-zero initialization has run (LRM 10.5), so
  // arming takes what it finds there.
  //
  // More than one read may name one cell, and they all want the same answer, so
  // arming an armed cell is not an error and changes nothing.
  void ArmSampling() {
    RareState().ArmSampling();
  }

  // What the cell held in the Preponed region of the current time slot -- its
  // value before anything in that slot ran (LRM 4.4.2.1, 16.5.1).
  [[nodiscard]] auto SampledGet(base::SourceLocation at) const -> const Owned& {
    const base::SourceLocationScope bound(at);
    const CellRareState<HeldAs>* rare = ExistingRareState();
    return SampledOf(rare == nullptr ? nullptr : rare->Sampled());
  }

  // Tells whoever waits on the part of the cell at `reached`, on what that part
  // lies in or on what lies in it that a write changed it (LRM 4.3), passing
  // over a wait on the part whose bits `change` shows the write left alone, and
  // a per-interval observer where the open interval accumulated the write.
  void AfterModify(
      const PartPath& reached, const Change& change, bool accumulated) {
    current_runtime().WakeParkedOn(*this, reached, change, accumulated);
  }

 protected:
  Cell()
    requires std::default_initializable<HeldAs>;
  // A storage built from `args`: told what it holds, or given the value.
  template <class... Args>
  explicit Cell(const Args&... args) : storage_(args...) {
  }
  ~Cell();

  [[nodiscard]] auto Held() -> HeldAs& {
    return storage_;
  }
  [[nodiscard]] auto Held() const -> const HeldAs& {
    return storage_;
  }

 private:
  // What the cell keeps only once it is sampled or taken over reads and
  // writes the storage it keeps that for.
  friend class CellRareState<HeldAs>;

  // The one path a whole value reaches the cell's storage by, whoever sent it.
  // The value arriving is compared with the one it would replace before
  // anything is written, so telling whoever waits keeps no copy of the old
  // value, except the old words a packed value's waits are passed over by.
  void Store(const Owned& new_val) {
    StoreBy(
        [&] { return storage_.IsBitIdentical(new_val); },
        [&] { storage_.Assign(new_val); });
  }

  // That path over however the arriving value is held: `already_held` answers
  // whether the cell holds the same bits, and `overwrite` replaces what it
  // holds.
  template <class AlreadyHeld, class Overwrite>
  void StoreBy(AlreadyHeld already_held, Overwrite overwrite) {
    if (!this->HasMembers()) {
      overwrite();
      return;
    }
    StoreTold(already_held, overwrite);
  }

  // The same where a wait is enrolled, which is told what the write did. Never
  // folded into the store nothing waits on, which every write of a variable
  // makes, so that one stays the size of the write itself and is folded into
  // whatever writes, as a cold path is kept out of a hot one.
  template <class AlreadyHeld, class Overwrite>
  [[gnu::noinline]] void StoreTold(
      AlreadyHeld already_held, Overwrite overwrite) {
    if (already_held()) {
      return;
    }
    const bool accumulated = this->BeforeModify(
        PartPath{},
        [&](EffectStorage& into) { return NewOriginalOf(into, storage_); });
    AfterModify(
        PartPath{}, ReplaceWhole(storage_.Get(), overwrite), accumulated);
  }

  // Shows what a release reestablished, as its reevaluation (LRM 10.6.2).
  void Reevaluate();

  // Has the takeover at `level` record what it evaluated to, and shows
  // whatever the highest level holding each position then holds there.
  auto RecordTakeover(
      std::int64_t level, std::int64_t generation, const Owned& evaluated)
      -> bool;

  // What only a sampled or taken-over cell keeps, made the first time either
  // is asked for. This cell is the only thing that makes it, so it is always
  // over the cell's own storage.
  [[nodiscard]] auto RareState() -> CellRareState<HeldAs>& {
    if (Rare() == nullptr) {
      InstallRare(std::make_unique<CellRareState<HeldAs>>(*this));
    }
    return static_cast<CellRareState<HeldAs>&>(*Rare());
  }
  [[nodiscard]] auto ExistingRareState() const -> CellRareState<HeldAs>* {
    return static_cast<CellRareState<HeldAs>*>(Rare());
  }

  // Comes last, so that a value whose size the library is told follows it.
  HeldAs storage_;
};

// A variable of a type known where it is compiled: the cell holding the value
// itself. What the language's own operations ask of it -- the value, the
// storage a write descends into, the write a declaration makes -- is typed by
// the value.
template <value::LyraValue T>
class Var final : public Cell<ValueStorageCore<T>> {
 public:
  // A variable of a value its type builds from nothing; a container's is
  // built only from its type, below. Generated code compiled apart from the
  // library calls this one by the name its own compiler gives it, so it
  // carries no constraint, which some compilers make part of that name.
  Var();
  // A variable holding the value `T` builds from `args` from the moment it
  // exists: a container's type, or a value to begin as.
  template <class... Args>
  explicit Var(const Args&... args);
  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var();

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return this->Held().Get();
  }
  [[nodiscard]] auto Storage() noexcept -> T& {
    return this->Held().Storage();
  }
  // The write a declaration makes, which may run more than once: a declaration
  // reached again begins a fresh variable in the one storage, over the value
  // the storage has held since it was built.
  void Initialize(const T& prototype) {
    this->Held().Assign(prototype);
  }
  void Initialize(T&& prototype) {
    this->Held().Assign(std::move(prototype));
  }

  // Opens a write into the cell for the full-expression that writes. The cell's
  // own storage is designated within it, the steps into parts are taken from
  // there, and the write lands where they are dereferenced, so a part is
  // written in the cell as it lies and the cell is told once whether it
  // changed. The write is non-copyable and non-movable, so keeping it past the
  // statement is rejected at compile time.
  auto Mutate() -> ScopedMutation<VariableRef<T>>;
};

// A variable whose value lies after it, held as `HeldAs` holds it -- the
// words of an integral value wider than a word, or a tuple, a union or a
// fixed-size unpacked array laid out by its type -- told its width or type
// where it is built. Generated code knows the type and reads and writes the
// value as its bytes, so that is all this answers with.
template <class HeldAs>
class TailCell final : public Cell<HeldAs> {
 public:
  template <class Told>
  explicit TailCell(const Told& told);
  TailCell(const TailCell&) = delete;
  auto operator=(const TailCell&) -> TailCell& = delete;
  TailCell(TailCell&&) = delete;
  auto operator=(TailCell&&) -> TailCell& = delete;
  ~TailCell();

  // How the value is held, which says how wide it is or its type.
  [[nodiscard]] auto HeldStorage() const -> const HeldAs& {
    return this->Held();
  }

  // The write a declaration makes, which may run more than once.
  void Initialize(const void* bytes) {
    this->Held().AssignBytes(bytes);
  }

  [[nodiscard]] auto Bytes() const -> void* {
    return this->Held().Bytes();
  }

  // What the cell held in the Preponed region of the current time slot (LRM
  // 4.4.2.1, 16.5.1), where it lies.
  [[nodiscard]] auto SampledBytes(base::SourceLocation at) const -> const
      void* {
    return this->SampledGet(at).Bytes();
  }
};

// The cell a variable of `T` is.
template <class T>
struct VariableOf {
  using Type = Var<T>;
};
template <DynamicallySized T>
struct VariableOf<T> {
  using Type = TailCell<typename StorageFor<T>::Type>;
};

// A variable the library makes for itself -- a force's storage, a cell a
// branch outlives its block for -- of a value held as `HeldAs` holds one,
// told `told`, in storage of the size such a cell takes.
template <class HeldAs, class Told>
auto MakeTailCell(const Told& told) -> std::shared_ptr<TailCell<HeldAs>> {
  return MakeWithTail<TailCell<HeldAs>>(HeldAs::ValueLayoutFor(told), told);
}

// What became of a part between `before` and `now`: the bits that moved where
// it is a packed value, so a wait reading only bits that moved back is passed
// over, the whole of it otherwise, and none where it is as it was.
template <class Part>
  requires value::LyraValue<Part> || BitAddressed<Part>
[[nodiscard]] auto ChangeBetween(const Part& before, const Part& now)
    -> std::optional<Change> {
  if constexpr (BitAddressed<Part>) {
    Change change = KeptBefore(before);
    KeepAfter(change, now);
    if (change.Unmoved()) {
      return std::nullopt;
    }
    return change;
  } else {
    if (before.IsBitIdentical(now)) {
      return std::nullopt;
    }
    return Change::Whole();
  }
}
// The same of a value laid out by `type` at `before` and at `now`, an integral
// one compared by its bits, read as its type lays them out, however narrow.
[[nodiscard]] auto ChangeBetween(
    const value::ValueType& type, const void* before, const void* now)
    -> std::optional<Change>;

// What a write keeps of the part it lands on, to tell afterwards what it did
// to it: a copy, of which nothing but whether it moved can be shown. The write
// lies in the frame of whatever writes, so the copy is kept as a frame keeps a
// value aside.
template <class Part>
class WriteOriginal {
 public:
  explicit WriteOriginal(const Part& part);
  WriteOriginal(const WriteOriginal&) = delete;
  auto operator=(const WriteOriginal&) -> WriteOriginal& = delete;
  WriteOriginal(WriteOriginal&&) = delete;
  auto operator=(WriteOriginal&&) -> WriteOriginal& = delete;
  ~WriteOriginal();

  // What the write did to `part`, none where it left it as it was.
  [[nodiscard]] auto ChangeTo(const Part& part) -> std::optional<Change>;

 private:
  value::SmallBox<Part> before_;
};

template <class Part>
WriteOriginal<Part>::WriteOriginal(const Part& part) : before_(part) {
}

template <class Part>
WriteOriginal<Part>::~WriteOriginal() = default;

template <class Part>
auto WriteOriginal<Part>::ChangeTo(const Part& part) -> std::optional<Change> {
  return ChangeBetween(*before_, part);
}

// A bit-addressed part keeps only the words it lies in, as a change reaching
// its bits, so a wait is compared on them by position. Some of a bit-addressed
// value's bits are a part a write lands on as well, kept as the words of the
// value they lie in.
template <BitAddressed Part>
class WriteOriginal<Part> {
 public:
  explicit WriteOriginal(const Part& part) : reached_(KeptBefore(part)) {
  }
  WriteOriginal(const Part& storage, value::BitPositions reached)
      : reached_(KeptBefore(storage, reached)) {
  }

  [[nodiscard]] auto ChangeTo(const Part& part) -> std::optional<Change> {
    KeepAfter(reached_, part);
    if (reached_.Unmoved()) {
      return std::nullopt;
    }
    return reached_;
  }

 private:
  Change reached_;
};

// Puts the value laid out by `type` at `kept` into `live`, and keeps what
// `live` held instead.
void ExchangeLaidOut(const value::ValueType& type, void* live, void* kept);

// A part's original, kept as the value it is: a copy, compared when the
// interval ends with the part it was copied from. The part is one a variable
// reaches by steps that fix where it lies -- the variable itself, a member, an
// element of a fixed-size array, an array whose size changes taken whole -- so
// it lies where it is for as long as the place does.
template <value::LyraValue Part>
class ValueOriginal final : public IntervalOriginal {
 public:
  explicit ValueOriginal(Part& live) : live_(&live), before_(live) {
  }

  [[nodiscard]] auto Changed() -> std::optional<Change> override {
    return ChangeBetween(*before_, *live_);
  }

  void Exchange() override {
    std::swap(*live_, *before_);
  }

 private:
  Part* live_;
  value::SmallBox<Part> before_;
};

// The same of an integral value wider than a word where it lies, named by a
// view of its words: what is kept is a copy of the words, since a copy of the
// view would name the part itself.
template <value::HeldAsWords Wide>
class WordsOriginal final : public IntervalOriginal {
 public:
  explicit WordsOriginal(Wide live) : live_(live), before_(live.ByteSize()) {
    std::memcpy(before_.data(), live_.bytes, before_.size());
  }

  [[nodiscard]] auto Changed() -> std::optional<Change> override {
    return ChangeBetween(
        Wide{.bytes = before_.data(), .width = live_.width}, live_);
  }

  void Exchange() override {
    std::swap_ranges(
        before_.begin(), before_.end(), static_cast<std::byte*>(live_.bytes));
  }

 private:
  Wide live_;
  std::vector<std::byte> before_;
};

// The same of a value laid out by a type the library is handed, which is its
// bytes where it lies.
class LaidOutOriginal final : public IntervalOriginal {
 public:
  LaidOutOriginal(const value::ValueType& type, void* live)
      : type_(&type), live_(live), before_(type, live) {
  }

  [[nodiscard]] auto Changed() -> std::optional<Change> override {
    return ChangeBetween(*type_, before_.Bytes(), live_);
  }

  void Exchange() override {
    ExchangeLaidOut(*type_, live_, before_.Bytes());
  }

 private:
  const value::ValueType* type_;
  void* live_;
  value::KeptCopy before_;
};

// The original each kind of storage keeps of the whole value it holds, in
// bytes `into` hands out.
template <value::LyraValue T>
auto NewOriginalOf(EffectStorage& into, ValueStorageCore<T>& storage)
    -> OwnedOriginal {
  return NewOriginal<ValueOriginal<T>>(into, storage.Storage());
}
template <value::StateDomain kDomain>
auto NewOriginalOf(EffectStorage& into, WordsStorage<kDomain>& storage)
    -> OwnedOriginal {
  return NewOriginal<WordsOriginal<value::WideAt<kDomain>>>(
      into, storage.Get());
}
inline auto NewOriginalOf(EffectStorage& into, LaidOutStorage& storage)
    -> OwnedOriginal {
  return NewOriginal<LaidOutOriginal>(into, storage.Type(), storage.Bytes());
}

// Writes `value` into the bits `bits` names (LRM 11.5.1) and answers what the
// write did to the bits of its value it reached, kept only where `watched` says
// something reads the answer: none where it moved none.
template <class Bits, class Value>
auto WriteBits(Bits& bits, const Value& value, bool watched)
    -> std::optional<Change> {
  const std::optional<value::BitPositions> reached =
      watched ? bits.Reached() : std::nullopt;
  if (!reached.has_value()) {
    bits.Set(value);
    return std::nullopt;
  }
  WriteOriginal<std::remove_cvref_t<decltype(bits.Root())>> kept(
      bits.Root(), *reached);
  bits.Set(value);
  return kept.ChangeTo(bits.Root());
}

// Writes `value` into storage a reference names, at the representation the
// storage already has; the first write into a value represented at run time
// that nothing has written yet is the one that gives it one, as a local's
// declaration does.
template <value::LyraValue T>
void StoreInto(T& storage, const T& value) {
  if constexpr (RepresentedAtRunTime<T>) {
    if (!storage.IsUninitialized() && !storage.SameRepresentation(value)) {
      throw InternalError(
          "StoreInto: a value's representation does not match the storage a "
          "reference names; a required conversion was not emitted");
    }
  }
  storage = value;
}

// The value a net shows of its own, held by that net (LRM 6.5). Only the net
// changes it -- resolving, taking its one driver's value, or a forced one --
// and the net tells its own waits when it does, so a wait reached through a
// reference to it enrols on the net, and a write through one never happens.
struct HeldByNet {
  Observable* net;
};

// What holds the storage a reference names, which is what a write through the
// reference is a write of (LRM 13.5.2): a variable something may wait on, the
// object a class property belongs to (LRM 9.4.2), a net, which nothing writes
// through a reference, or nothing anyone is told about -- a variable of a
// function, which nothing can wait on (LRM 13.4.4), a closure's own copy of a
// variable, which no other process reaches, and an element an index named none
// of.
using StorageHolder =
    std::variant<std::monostate, VariableCell*, GcObject*, HeldByNet>;

// A reference in a form that names no type: where a value lies, and what holds
// that storage. What a reference does that does not depend on the type of the
// value it names is done here, once, for every reference whichever side holds
// it.
struct ErasedReference {
  StorageHolder holder;
  void* storage = nullptr;
  // Whether the storage is the whole of a variable, which is when the
  // reference stands for the variable itself. Only where the reference is
  // formed can say so: a part can lie at the address its whole does.
  bool whole = false;
  // The member of a scope this reference was bound into, where it was: a port
  // that stands for what its connection drives (LRM 23.3.3). The member is what
  // a force acts on (LRM 10.6.2), and a copy of the reference -- one handed on
  // to a child's port, a `ref` port bound to it, one a wait was built from --
  // still says which member it is a copy of. None for a reference bound into no
  // member.
  ErasedReference* member = nullptr;
  // Which part of what holds it the storage is: the path the reference was
  // formed by, from the whole.
  PartPath part{};

  // Whether the reference says which part of what holds it it names: the
  // whole, or a part by the path it was formed by. One the solver forms, by a
  // walk over storage whose type it does not hold, says neither, so a write
  // through it keeps nothing.
  [[nodiscard]] auto NamesItsPart() const -> bool {
    return whole || !part.IsWhole();
  }

  // Written here rather than in the library's own source because every write
  // through a reference asks them, and for storage whose variable nothing
  // samples or takes over each is a test or two.

  // Whether a write through the reference is overridden rather than landing
  // where it names. Only a variable can be put under a procedural continuous
  // assignment (LRM 10.6).
  [[nodiscard]] auto OverridesWrites() const -> bool {
    VariableCell* const* variable = std::get_if<VariableCell*>(&holder);
    return variable != nullptr && (*variable)->OverridesWrites();
  }

  [[nodiscard]] auto Watched() const -> bool {
    if (VariableCell* const* variable = std::get_if<VariableCell*>(&holder)) {
      return (*variable)->Watched();
    }
    if (GcObject* const* object = std::get_if<GcObject*>(&holder)) {
      return (*object)->Watched();
    }
    const HeldByNet* net = std::get_if<HeldByNet>(&holder);
    return net != nullptr && net->net->HasMembers();
  }

  // The rest are defined in the library's own source: none depends on the type
  // of the value the reference names, and none is on every write's path.

  // Tells whoever waits on what holds the storage that a write through the
  // reference changed the part at `within` of what the reference names (LRM
  // 4.3), `change` saying what it did to that part and `accumulated` whether
  // the open interval accumulated the write. A wait on an object reevaluates
  // the expression that reached it, which decides whether the write was an
  // event, so it is told nothing about what moved.
  void AfterModify(
      const PartPath& within, const Change& change, bool accumulated) const;

  // A step can form what it reaches, which is a write into the variable, so
  // the variable admits it first.
  void AdmitStep() const;

  // What a wait on the storage enrols on: what `Report` tells, which is the
  // variable or the object's event source. Storage that belongs to nothing
  // answers with none, since no write to it is ever told.
  [[nodiscard]] auto ReportsTo() const -> Observable*;

  // States that something drives the storage continuously (LRM 10.3), which
  // is what decides what a variable shows once nothing overrides it (LRM
  // 10.6.2). Only a variable is ever overridden, so storage nothing holds, and
  // an object's, have nothing to be told.
  void DriveContinuously() const;

  // Lands a write of the part this reference names, which `write` makes where
  // the part lies, in the variable a procedural continuous assignment
  // overrides (LRM 10.6.2), which then shows each takeover over what it holds
  // again. Only a variable is ever overridden.
  void WritePart(OwnedCall write) const;

  // The reference to a part of what this one names, at `storage`, reached by
  // the further steps `within` and which forming did `formed` to. The part has
  // the same holder, which is told at once where forming it changed the
  // storage -- an associative entry made by being bound (LRM 7.8.7) -- with
  // `accumulated` saying whether the open interval accumulated that. An
  // index naming no element names storage that belongs to nothing (LRM 7.4.5).
  [[nodiscard]] auto Part(
      void* storage, value::Formation formed, const PartPath& within,
      bool accumulated) const -> ErasedReference;
};

// Where a write announces each modification it makes, immediately before it
// (LRM 9.2.2.2): the place that is told of the write, and the path of what the
// write names within that place. No place where the write modifies nothing a
// interval keeps an original of.
struct WrittenPart {
  Observable* place = nullptr;
  PartPath at{};

  // Whether a per-interval observer watches the place, the one place a
  // modification may keep an original of. Asked by every watched write as it
  // opens.
  [[nodiscard]] auto WatchedPerInterval() const -> bool {
    return place != nullptr && place->WatchedPerInterval();
  }

  // Announces that the part `within` of what the write names, which `copy`
  // copies, is about to be modified, and answers whether the open interval
  // accumulated the modification.
  template <class Copy>
  [[nodiscard]] auto BeforeModify(const PartPath& within, Copy copy) const
      -> bool {
    return place != nullptr && place->BeforeModify(at.Join(within), copy);
  }

  // The same of the bits `bits` of the integral value the part `within` is,
  // whose words `live` names.
  [[nodiscard]] auto BeforeModifyBits(
      const PartPath& within, value::IntegralView live,
      value::BitPositions bits) const -> bool;
};

// Where a write through `lent` announces its modifications: the variable it
// names a part of, where it says which part.
[[nodiscard]] auto WrittenPartThrough(const ErasedReference& lent)
    -> WrittenPart;

// What keeps storage a call binds by reference in existence until the call
// returns, whatever the program does meanwhile (LRM 13.5.2, 8.29): a share of
// the object a property lies in, or a lease on the element, or the generation
// of elements, a container keeps it in. A binding takes one for each such
// thing its reference reaches through; one taken where an index names no
// element holds nothing.
class StorageLease {
 public:
  StorageLease() = default;
  explicit StorageLease(std::shared_ptr<GcObject> object)
      : owner_(std::move(object)) {
  }
  explicit StorageLease(value::ContainerLease lease)
      : owner_(std::move(lease)) {
  }

 private:
  std::variant<std::monostate, std::shared_ptr<GcObject>, value::ContainerLease>
      owner_;
};

// A reference a call binds, and the lease keeping what it names in existence
// while the call runs. Whoever binds it holds this while the call runs and
// hands the call the reference alone, which owns nothing.
class LeasedReference {
 public:
  LeasedReference(StorageLease lease, const ErasedReference& reference)
      : lease_(std::move(lease)), reference_(reference) {
  }

  [[nodiscard]] auto Erased() const -> const ErasedReference& {
    return reference_;
  }
  template <value::LyraValue T>
  [[nodiscard]] auto Reference() const -> VariableRef<T> {
    return VariableRef<T>{reference_};
  }

 private:
  StorageLease lease_;
  ErasedReference reference_;
};

// A write of a whole value through `lent` (LRM 4.3, 10.6): one the variable
// overrides is handed to it as `overridden` hands it, one nobody waits on is
// written as `assign` writes it, and one that leaves the bits as `identical`
// finds them is no change. Otherwise `announce` announces the modification,
// answering whether the open interval accumulated it, and `replace`
// writes it and answers the change, which whoever waits through `lent` is
// told.
template <
    class Overridden, class Assign, class Identical, class Announce,
    class Replace>
void SetThrough(
    const ErasedReference& lent, Overridden overridden, Assign assign,
    Identical identical, Announce announce, Replace replace) {
  if (lent.OverridesWrites()) {
    overridden();
    return;
  }
  if (!lent.Watched()) {
    assign();
    return;
  }
  if (identical()) {
    return;
  }
  const bool accumulated = announce();
  lent.AfterModify(PartPath{}, replace(), accumulated);
}

// A write of a whole value laid out by `type` -- the bytes of an integral
// value, a structure, a union, a tuple, a fixed-size unpacked array -- through
// `lent`, told to whoever waits on what it names as any write through it is.
void WriteLaidOutThrough(
    const ErasedReference& lent, const value::ValueType& type,
    const void* value);

// The variable a reference to the whole of it names, as the cell of class `C`
// it is, which the reference's own type decides.
template <class C>
auto WholeVariable(const ErasedReference& reference) -> C& {
  if (!reference.whole ||
      !std::holds_alternative<VariableCell*>(reference.holder)) {
    throw InternalError(
        "WholeVariable: a reference names something other than the whole of a "
        "variable");
  }
  return static_cast<C&>(**std::get_if<VariableCell*>(&reference.holder));
}

// A reference to the whole of a variable, which names its value where every
// reference to a value of its type names one: the bytes a tuple, a union, an
// integral value wider than a word and a fixed-size unpacked array lay out,
// since no object of the runtime's stands for such a value where it lies in
// another, and the object for every other.
template <value::LyraValue T>
auto WholeReference(Var<T>& variable) -> ErasedReference {
  return {
      .holder = static_cast<VariableCell*>(&variable),
      .storage = HandleTo(variable.Storage()),
      .whole = true};
}
template <class HeldAs>
auto WholeReference(TailCell<HeldAs>& variable) -> ErasedReference {
  return {
      .holder = static_cast<VariableCell*>(&variable),
      .storage = variable.Bytes(),
      .whole = true};
}

// A reference, typed by the value it names. A reference to the whole of a
// variable and one to a part of it are the same thing, so a write through
// either is the variable's write, told to whoever waits on the variable at the
// moment it lands (LRM 4.3). Copyable, so a ref formal can be forwarded as a
// ref argument to a nested call.
template <value::LyraValue T>
class VariableRef {
 public:
  using ValueType = T;

  // A null view, default-constructed as a member and bound before first use:
  // a `ref` port's child-side member is declared with the child and filled by
  // the parent during elaboration (LRM 23.3.3.2), before simulation reads it.
  VariableRef() = default;
  explicit VariableRef(Var<T>& cell)
      : erased_{
            .holder = static_cast<VariableCell*>(&cell),
            .storage = &cell.Storage(),
            .whole = true} {
  }
  explicit VariableRef(T& storage)
      : erased_{.holder = {}, .storage = &storage} {
  }
  explicit VariableRef(const ErasedReference& erased) : erased_(erased) {
  }

  [[nodiscard]] auto Erased() const -> const ErasedReference& {
    return erased_;
  }

  // Where a wait on the storage this names enrols (LRM 13.5.2): what a write
  // through it is told to, never the reference itself.
  [[nodiscard]] auto ReportsTo() const -> WatchedPlace {
    return WatchedPlace::Through(erased_);
  }

  // The reference as a member something is bound into (LRM 23.3.3), which is
  // what binding rewrites.
  [[nodiscard]] auto AsMember() -> ErasedReference& {
    return erased_;
  }

  [[nodiscard]] auto Get() const -> const T& {
    return Storage();
  }

  // Const: a `VariableRef` is a view, so `Set` writes the referenced storage,
  // not the handle's own pointers -- as `*p = v` is allowed through a `T* const
  // p`.
  void Set(const T& new_val) const {
    const auto assign = [&] { StoreInto(Storage(), new_val); };
    SetThrough(
        erased_, [&] { ApplyOverriddenWrite(new_val); }, assign,
        [&] { return Storage().IsBitIdentical(new_val); },
        [&] { return BeforeModifyWhole(); },
        [&] { return ReplaceWhole(Storage(), assign); });
  }

  // Opens a write into the referenced storage, as an observable cell itself
  // does; only storage that belongs to a variable has anyone to tell what the
  // write did.
  [[nodiscard]] auto Mutate() const -> ScopedMutation<VariableRef<T>>;

  // A procedural continuous assignment on a name that owns no storage (LRM
  // 10.6.2), by the operations a variable itself answers: the reference is a
  // member of a scope standing for what its connection drives, and the
  // assignment gives it storage of its own to name until it ends.
  [[nodiscard]] auto BeginTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at, TakePositionsOf<T> take_positions = {}) const
      -> std::int64_t;
  [[nodiscard]] auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& new_val,
      base::SourceLocation at) const -> bool;
  void EndTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count,
      base::SourceLocation at) const;

  // A reference to an element or a component of what this one names (LRM
  // 13.5.2), which belongs to the same variable.
  template <typename Key>
  [[nodiscard]] auto ReferElement(
      const Key& key, base::SourceLocation at) const {
    const base::SourceLocationScope bound(at);
    erased_.AdmitStep();
    const bool accumulated =
        StepCanReshape(Storage(), key) && BeforeModifyWhole();
    value::Formation formed{};
    auto& element = value::ElementMut(Storage(), key, formed);
    return VariableRef<std::remove_reference_t<decltype(element)>>{
        erased_.Part(&element, formed, RetainedStepTo<T>(key), accumulated)};
  }

  // The same, bound by a call, with a lease on the element the array keeps,
  // which the array cannot then end before the call returns (LRM 13.5.2).
  template <typename Key>
  [[nodiscard]] auto ReferElementLeased(
      const Key& key, base::SourceLocation at) const -> LeasedReference {
    const base::SourceLocationScope bound(at);
    erased_.AdmitStep();
    const bool accumulated =
        StepCanReshape(Storage(), key) && BeforeModifyWhole();
    value::Formation formed{};
    auto& element = value::ElementMut(Storage(), key, formed);
    const ErasedReference part =
        erased_.Part(&element, formed, RetainedStepTo<T>(key), accumulated);
    if (formed == value::Formation::kNowhere) {
      return {StorageLease{}, part};
    }
    return {StorageLease(value::ElementLease(Storage(), element)), part};
  }

  template <std::size_t I>
  [[nodiscard]] auto ReferComponent() const {
    auto& component = Storage().template Component<I>();
    const bool accumulated = false;
    return VariableRef<std::remove_reference_t<decltype(component)>>{
        erased_.Part(
            &component, value::Formation::kExisting,
            PartPath::Written(
                support::PartStep::kComponent, static_cast<std::int64_t>(I)),
            accumulated)};
  }

  // The `MutationSink` surface. Storage that belongs to no variable has no
  // observation at all, so nothing reads what a write did to it; that is the
  // same answer a variable gives while nothing waits on it, reached by a
  // shorter route.
  [[nodiscard]] auto OverridesWrites() const -> bool {
    return erased_.OverridesWrites();
  }
  // Only a reference to the whole of a variable knows the variable's type,
  // which is what a value kept beneath an assignment is of.
  [[nodiscard]] auto OverriddenWriteStorage() const -> T* {
    return Whole() ? Cell().OverriddenWriteStorage() : nullptr;
  }
  // A write through a reference to part of a variable lands in the part where
  // it lies, which the variable then takes as any overridden write.
  void ApplyOverriddenWrite(const T& written) const {
    if (Whole()) {
      Cell().ApplyOverriddenWrite(written);
      return;
    }
    erased_.WritePart([&] { Storage() = written; });
  }
  [[nodiscard]] auto MutationStorage() const -> T& {
    return Storage();
  }
  [[nodiscard]] auto Watched() const -> bool {
    return erased_.Watched();
  }

  // A reference denotes the storage it binds (LRM 23.3.3.2), so the operations
  // on a cell answer through it. Storage nothing holds changes only where the
  // reference is, so its sampled value is its current one. A variable keeps
  // its sampled value whole, so a reference to a part of one has none to
  // answer with; nothing keeps a class property's Preponed value; and a net
  // keeps its own, which a reference naming no net type cannot ask for.
  void ArmSampling() const {
    if (Whole() && std::holds_alternative<VariableCell*>(erased_.holder)) {
      Cell().ArmSampling();
    }
  }
  [[nodiscard]] auto SampledGet(base::SourceLocation at) const -> const T& {
    const base::SourceLocationScope bound(at);
    return std::visit(
        Overloaded{
            [&](std::monostate) -> const T& { return Storage(); },
            [&](VariableCell*) -> const T& {
              if (!Whole()) {
                throw SimulationError(
                    "a sampled value of part of a variable lent by reference "
                    "is not yet supported");
              }
              return Cell().SampledGet(at);
            },
            [&](GcObject*) -> const T& {
              throw SimulationError(
                  "a sampled value of a class property lent by reference is "
                  "not yet supported");
            },
            [&](HeldByNet) -> const T& {
              throw SimulationError(
                  "a sampled value of a net lent by reference is not yet "
                  "supported");
            }},
        erased_.holder);
  }

  // A write through the reference changed the part at `within` of what it
  // names, which lies where the reference's own steps led.
  void AfterModify(
      const PartPath& within, const Change& change, bool accumulated) const {
    erased_.AfterModify(within, change, accumulated);
  }

  [[nodiscard]] auto WrittenPart() const -> runtime::WrittenPart {
    return WrittenPartThrough(erased_);
  }

 private:
  // Announces that what this names is about to be modified whole, by a write
  // through it or by a step that can make an element, and answers whether the
  // open interval accumulated that.
  [[nodiscard]] auto BeforeModifyWhole() const -> bool {
    return WrittenPart().BeforeModify(PartPath{}, [&](EffectStorage& into) {
      return NewOriginal<ValueOriginal<T>>(into, Storage());
    });
  }

  [[nodiscard]] auto Storage() const -> T& {
    return *static_cast<T*>(erased_.storage);
  }

  [[nodiscard]] auto Whole() const -> bool {
    return erased_.whole;
  }

  [[nodiscard]] auto Cell() const -> Var<T>& {
    return WholeVariable<Var<T>>(erased_);
  }

  ErasedReference erased_;
};

// The class declaring a member, and the member's type, as a pointer to the
// member states them.
template <class MemberPointer>
struct MemberOf;

template <class T, class C>
struct MemberOf<T C::*> {
  using Class = C;
  using Value = T;
};

// A reference to the property `Property` of an object (LRM 8.4), held by the
// object so a write through it tells the object as it lands (LRM 9.4.2,
// 13.5.2). The property is a step taken on the object, so the object is
// reached once.
template <auto Property>
auto ReferProperty(typename MemberOf<decltype(Property)>::Class* object)
    -> VariableRef<typename MemberOf<decltype(Property)>::Value> {
  return VariableRef<typename MemberOf<decltype(Property)>::Value>{
      ErasedReference{
          .holder = static_cast<GcObject*>(object),
          .storage = &(object->*Property)}};
}

// `reference`, which names storage an object holds, bound by a call with a
// share of that object, which then stays until the call returns though the
// program lets go of every handle to it (LRM 8.29, 13.5.2).
auto LeaseObject(const ErasedReference& reference) -> LeasedReference;
template <value::LyraValue T>
auto LeaseObject(const VariableRef<T>& reference) -> LeasedReference {
  return LeaseObject(reference.Erased());
}

// States that a continuous assignment drives the storage `reference` names
// (LRM 10.3), once, as the design is built.
template <value::LyraValue T>
void DrivesContinuously(const VariableRef<T>& reference) {
  reference.Erased().DriveContinuously();
}

// Defaulted here rather than where they are declared: a constructor or
// destructor defaulted on its first declaration is not user-provided, so a unit
// constructing a cell would define it itself with everything it reaches, and a
// family stated as already compiled would not withhold it.
template <class HeldAs>
Cell<HeldAs>::Cell()
  requires std::default_initializable<HeldAs>
= default;

template <class HeldAs>
Cell<HeldAs>::~Cell() = default;

template <value::LyraValue T>
Var<T>::Var() = default;

template <value::LyraValue T>
template <class... Args>
Var<T>::Var(const Args&... args) : Cell<ValueStorageCore<T>>(args...) {
}

template <value::LyraValue T>
Var<T>::~Var() = default;

template <class HeldAs>
template <class Told>
TailCell<HeldAs>::TailCell(const Told& told) : Cell<HeldAs>(told) {
}

template <class HeldAs>
TailCell<HeldAs>::~TailCell() = default;

template <class HeldAs>
void Cell<HeldAs>::Set(const Owned& new_val) {
  // A cell nobody has ever taken over holds no record at all, so the ordinary
  // write pays one null test.
  if (OverridesWrites()) {
    ApplyOverriddenWrite(new_val);
    return;
  }
  Store(new_val);
}

template <class HeldAs>
void Cell<HeldAs>::ApplyOverriddenWrite(const Owned& written) {
  Takeovers<Owned>& takeovers = *ExistingRareState()->ExistingTakeovers();
  if (Owned* beneath = takeovers.Beneath();
      beneath != nullptr && beneath != &written) {
    *beneath = written;
  }
  value::SmallBox<Owned> shown(written);
  takeovers.Overlay(*shown);
  Store(*shown);
}

// The write lands where the part lies, which leaves the whole value it made;
// the cell takes that as any overridden write, from what it held before, so
// what its waits are told is measured against that.
template <class HeldAs>
void CellRareState<HeldAs>::WritePart(OwnedCall write) {
  HeldAs& held = cell_->Held();
  const auto before =
      value::SmallBox<Owned>::BuiltBy([&] { return held.AsOwned(); });
  write();
  const auto written =
      value::SmallBox<Owned>::BuiltBy([&] { return held.AsOwned(); });
  held.Assign(*before);
  cell_->ApplyOverriddenWrite(*written);
}

template <class HeldAs>
auto Cell<HeldAs>::BeginTakeover(
    std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at, TakePositionsOf<Owned> take_positions)
    -> std::int64_t {
  const base::SourceLocationScope bound(at);
  return std::int64_t{
      RareState()
          .Takeovers(take_positions)
          .Begin(TakeoverLevelOf(level), PositionsNamed(first, count))};
}

template <class HeldAs>
auto Cell<HeldAs>::DriveTakeover(
    std::int64_t level, std::int64_t generation, const Owned& new_val,
    base::SourceLocation at) -> bool {
  const base::SourceLocationScope bound(at);
  return RecordTakeover(level, generation, new_val);
}

template <class HeldAs>
auto Cell<HeldAs>::DriveTakeoverBytes(
    std::int64_t level, std::int64_t generation, const void* bytes,
    base::SourceLocation at) -> bool
  requires requires(const HeldAs& held) {
    { held.ToOwned(bytes) } -> std::same_as<Owned>;
  }
{
  const base::SourceLocationScope bound(at);
  return RecordTakeover(level, generation, storage_.ToOwned(bytes));
}

template <class HeldAs>
auto Cell<HeldAs>::RecordTakeover(
    std::int64_t level, std::int64_t generation, const Owned& evaluated)
    -> bool {
  const CellRareState<HeldAs>* rare = ExistingRareState();
  Takeovers<Owned>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  if (takeovers == nullptr) {
    return false;
  }
  if (!takeovers->Drive(
          TakeoverLevelOf(level), TakeoverGenerationOf(generation),
          evaluated)) {
    return false;
  }
  // A cell something drives continuously, with nothing kept beneath yet, holds
  // what that driver last produced, which is what it shows again wherever
  // nothing covers it (LRM 10.6.2).
  if (takeovers->Beneath() == nullptr &&
      current_runtime().DrivenContinuously(*this)) {
    takeovers->KeepBeneath(storage_.AsOwned());
  }
  // A takeover that just recorded shows its value only where no higher level
  // covers it. Storing whatever shows needs no question asked: where a higher
  // level covers this one, what shows has not moved, and a store that changes
  // nothing publishes nothing.
  auto shown =
      value::SmallBox<Owned>::BuiltBy([&] { return storage_.AsOwned(); });
  takeovers->Overlay(*shown);
  Store(*shown);
  return true;
}

template <class HeldAs>
void Cell<HeldAs>::EndTakeover(
    std::int64_t level, std::int64_t first, std::int64_t count,
    base::SourceLocation at) {
  const base::SourceLocationScope bound(at);
  // Ending a level nothing occupies is what a `release` on an untaken variable
  // does, and the language gives it no effect (LRM 10.6.2).
  const CellRareState<HeldAs>* rare = ExistingRareState();
  Takeovers<Owned>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  if (takeovers == nullptr) {
    return;
  }
  takeovers->End(TakeoverLevelOf(level), PositionsNamed(first, count));
  // A variable nothing lies beneath keeps what it has, which is the value the
  // ended takeover last gave it. Anything else -- a lower takeover, or a
  // continuous driver -- is reestablished and shows once its reevaluation runs
  // in the Active region (LRM 10.6.2).
  if (takeovers->Beneath() == nullptr && !takeovers->Shows()) {
    return;
  }
  RuntimeEffects& runtime = current_runtime();
  runtime.Submit(
      runtime.Now(), support::Region::kActive, [this] { Reevaluate(); });
}

template <class HeldAs>
auto Cell<HeldAs>::TakenOver() const -> bool {
  const CellRareState<HeldAs>* rare = ExistingRareState();
  const Takeovers<Owned>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  return takeovers != nullptr && takeovers->Holds();
}

template <class HeldAs>
void Cell<HeldAs>::Reevaluate() {
  Takeovers<Owned>& takeovers = *ExistingRareState()->ExistingTakeovers();
  // Each position takes what the highest takeover holding it holds there, and
  // where none does, what the continuous driver produced, or what it has.
  const Owned* beneath = takeovers.Beneath();
  auto shown = value::SmallBox<Owned>::BuiltBy([&] {
    return beneath != nullptr ? Owned(*beneath) : storage_.AsOwned();
  });
  takeovers.Overlay(*shown);
  Store(*shown);
  if (!takeovers.Shows()) {
    takeovers.DropBeneath();
  }
}

// One write in progress into what a sink stands for, and what it has learned
// so far about whether the write changed it (LRM 4.3). The write reaches the
// sink's storage and lands where its steps lead; what it did there is learned
// in three ways, whichever comes first settling the answer:
//
//   - a step that forms the part it reaches can change the owner by forming it
//     -- an element made by being written (LRM 7.8.7, 7.10.1) -- or reach no
//     part of it at all, where an invalid index makes the write ignored
//     (LRM 7.4.5), and either answers the question whatever is written;
//   - a slice write compares each element before writing it, so it says
//     which of them it changed;
//   - otherwise the part landed on is compared with its value from before the
//     write, kept only while something reads the answer.
//
// The answer reaches the sink once. It is the owner's own storage the write
// lands in, so a write is visible to everything reading the owner from the
// moment it lands (LRM 13.5.2), and a second write open over the same owner
// cannot be overwritten by this one ending.
template <class Sink>
class WriteBracket {
 public:
  using ValueType = typename Sink::ValueType;

  // An overridden write (LRM 10.6) still has to reach a part to land
  // on. It lands where the sink keeps such a write, or in a copy of the sink's
  // storage that this write keeps, and the sink is handed what it left when
  // it is over.
  explicit WriteBracket(Sink sink) : sink_(sink) {
    if constexpr (OverridableSink<Sink>) {
      if (sink_.OverridesWrites()) {
        overridden_ = true;
        if (ValueType* kept = sink_.OverriddenWriteStorage()) {
          storage_ = kept;
        } else {
          storage_ = &*discarded_.emplace(sink_.MutationStorage());
        }
        return;
      }
    }
    storage_ = &sink_.MutationStorage();
    watched_ = sink_.Watched();
    watched_per_interval_ =
        watched_ && sink_.WrittenPart().WatchedPerInterval();
  }

  [[nodiscard]] auto Storage() const -> ValueType& {
    return *storage_;
  }

  // Announces that the part `further` leads to from the part the steps so far
  // reached, which `copy` copies, is about to be modified, so the open
  // interval keeps its original where a per-interval observer watches
  // the place (LRM 9.2.2.2). Only a write whose steps are still followed
  // announces anything: one a step settled has modified only what that step
  // announced, or lands where no read reaches (LRM 7.4.5); and an overridden
  // write modifies the place only once it is over, where the place announces
  // what it takes as a whole.
  template <class Copy>
  void BeforeModify(const PartPath& further, Copy copy) {
    if (!Announces()) {
      return;
    }
    NoteAccumulated(
        sink_.WrittenPart().BeforeModify(reached_.Join(further), copy));
  }
  // The same of a part of a type known here.
  template <value::LyraValue Part>
  void BeforeModifyPart(Part& part, const PartPath& further = PartPath{}) {
    BeforeModify(further, [&](EffectStorage& into) {
      return NewOriginal<ValueOriginal<Part>>(into, part);
    });
  }
  // The same of the bits `bits` of the integral value the steps so far
  // reached, whose words `live` names.
  void BeforeModifyBits(value::IntegralView live, value::BitPositions bits) {
    if (!Announces()) {
      return;
    }
    NoteAccumulated(sink_.WrittenPart().BeforeModifyBits(reached_, live, bits));
  }

  // Whether what the write does to the part it lands on is still to be told:
  // something reads the answer, and no step has given it already.
  [[nodiscard]] auto Tells() const -> bool {
    return watched_ && Open();
  }

  // Whether a modification this write is still to make has to be announced:
  // a per-interval observer watches the place, which is settled where the write
  // opens, and no step has settled the write.
  [[nodiscard]] auto Announces() const -> bool {
    return watched_per_interval_ && Open();
  }

  void Formed(value::Formation formed) {
    if (!Open()) {
      return;
    }
    switch (formed) {
      case value::Formation::kExisting:
        return;
      case value::Formation::kMade:
        outcome_ = Outcome::kChanged;
        return;
      case value::Formation::kNowhere:
        outcome_ = Outcome::kSettled;
        return;
    }
    throw InternalError("WriteBracket: unknown formation");
  }

  // A write of a run of elements of the part reached (LRM 7.6) changed those
  // from `begin` to one before `end`, which is all it does to the part; where
  // the two are the same it changed none, which settles the write unchanged.
  void Moved(std::int64_t begin, std::int64_t end) {
    if (!Open()) {
      return;
    }
    if (begin == end) {
      outcome_ = Outcome::kSettled;
      return;
    }
    outcome_ = Outcome::kChanged;
    moved_ = {
        .lsb = static_cast<std::uint64_t>(begin),
        .width = static_cast<std::uint64_t>(end - begin)};
  }

  // The write went on to a part of the one it had reached, by `step`.
  void Stepped(const PartPath& step) {
    reached_ = reached_.Join(step);
  }

  // The part landed on is not what it was, and `change` says which of its bits
  // moved where it is a value with bits to speak of.
  void Landed(const Change& change) {
    if (!Open()) {
      return;
    }
    outcome_ = Outcome::kSettled;
    sink_.AfterModify(reached_, change, Accumulated());
  }

  // The end of the write, with the full-expression that opened it.
  void End() {
    if constexpr (OverridableSink<Sink>) {
      if (overridden_) {
        sink_.ApplyOverriddenWrite(*storage_);
        return;
      }
    }
    switch (outcome_) {
      case Outcome::kUndecided:
      case Outcome::kSettled:
        return;
      case Outcome::kChanged:
        if (watched_) {
          sink_.AfterModify(reached_, Change::Elements(moved_), Accumulated());
        }
        return;
    }
    throw InternalError("WriteBracket: unknown outcome");
  }

 private:
  enum class Outcome : std::uint8_t { kUndecided, kChanged, kSettled };

  // Notes whether the open interval accumulated a modification this
  // write announced. Covered belongs to each modification, and every one a
  // write announces is the part it tells of or a part holding it, announced
  // in the interval open at that moment, so the answers cannot differ within
  // one write: either the interval holds an original holding the part told of,
  // or no original was kept at all.
  void NoteAccumulated(bool accumulated) {
    if (announced_ && accumulated_ != accumulated) {
      throw InternalError(
          "WriteBracket: modifications of one write answer differently "
          "whether an interval accumulated them -- please report this as "
          "a bug");
    }
    announced_ = true;
    accumulated_ = accumulated;
  }
  [[nodiscard]] auto Accumulated() const -> bool {
    return accumulated_;
  }

  // Whether nothing has yet settled what the write did.
  [[nodiscard]] auto Open() const -> bool {
    switch (outcome_) {
      case Outcome::kUndecided:
        return true;
      case Outcome::kChanged:
      case Outcome::kSettled:
        return false;
    }
    throw InternalError("WriteBracket: unknown outcome");
  }

  Sink sink_;
  ValueType* storage_ = nullptr;
  bool watched_ = false;
  bool overridden_ = false;
  // What the write knows of the open interval, read on every write
  // and held in the room of one flag: whether a per-interval observer watches
  // the place, settled where the write opens; whether the write has announced
  // a modification; and whether the interval accumulated what it announced.
  bool watched_per_interval_ : 1 = false;
  bool announced_ : 1 = false;
  bool accumulated_ : 1 = false;
  Outcome outcome_ = Outcome::kUndecided;
  // The part of the storage the steps taken so far reached.
  PartPath reached_;
  // The elements of that part a slice write moved, none in particular where
  // the write changed the part by forming one.
  value::BitPositions moved_{};
  // A copy of the storage for an overridden write to land in, where the sink
  // keeps none, held as a frame keeps a value aside, since the write lies in
  // the frame of whatever writes.
  std::optional<value::SmallBox<ValueType>> discarded_;
};

template <class Sink, class Bits>
class DesignatedBits;

// A place designated within a write: the whole of what the write was opened
// on, or a part of it reached by the steps below. It borrows the write. The
// steps reach the parts a write lands on -- an element, reported to the write
// as it is formed, a component, and a union's member (LRM 7.2, 7.3, 7.4) --
// each answering with the part designated within the same write; bits are
// reached by a reference to them and a run of elements is written in one call,
// each telling the write what moved. Dereferencing it is where the write lands,
// which is when the value from before the write is kept.
template <class Sink, class Part>
class Designation {
 public:
  Designation(WriteBracket<Sink>& write, Part& part)
      : write_(&write), part_(&part) {
  }

  Designation(const Designation&) = delete;
  auto operator=(const Designation&) -> Designation& = delete;
  Designation(Designation&&) = delete;
  auto operator=(Designation&&) -> Designation& = delete;

  ~Designation() {
    if (!kept_.has_value()) {
      return;
    }
    if (const std::optional<Change> change = kept_->ChangeTo(*part_)) {
      write_->Landed(*change);
    }
  }

  // An array whose size changes moves and ends its elements as it grows and
  // shrinks (LRM 7.5, 7.8, 7.10), so it is kept whole before a step into it.
  template <typename Key>
  auto DesignateElement(const Key& key) {
    if constexpr (ResizableArray<Part>) {
      write_->BeforeModifyPart(*part_);
    }
    value::Formation formed{};
    auto& element = value::ElementMut(*part_, key, formed);
    write_->Formed(formed);
    if (write_->Tells()) {
      write_->Stepped(StepTo<Part>(key));
    }
    return Designation<Sink, std::remove_reference_t<decltype(element)>>{
        *write_, element};
  }
  // The same of a queue, which a write of an element can grow past its bound.
  template <typename Key>
  auto DesignateElement(const Key& key, base::SourceLocation at) {
    const base::SourceLocationScope bound(at);
    return DesignateElement(key);
  }

  template <std::size_t I>
  auto DesignateComponent() {
    auto& component = part_->template Component<I>();
    if (write_->Tells()) {
      write_->Stepped(
          PartPath::Written(
              support::PartStep::kComponent, static_cast<std::int64_t>(I)));
    }
    return Designation<Sink, std::remove_reference_t<decltype(component)>>{
        *write_, component};
  }

  // A member of a union lies where every other member does (LRM 7.3), so the
  // path a write of it reports stops at the union, which every member reads,
  // and the union is what an interval keeps the original of, while the write
  // itself keeps only what it lands on below it.
  template <std::size_t I>
  auto DesignateMember() {
    auto& member = part_->template Component<I>();
    if (write_->Tells()) {
      write_->BeforeModifyPart(*part_);
      write_->Stepped(
          PartPath::Written(
              support::PartStep::kUnionMember, static_cast<std::int64_t>(I)));
    }
    return Designation<Sink, std::remove_reference_t<decltype(member)>>{
        *write_, member};
  }

  // A reference to the bits of a value of `Bits` at `position` within the
  // part (LRM 11.5.1), written within this write. They lie in the part's own
  // words, so a write through it lands on them where they are. An
  // interval keeps the original of the words the bits written lie in; a part
  // narrower than a word is that word, kept whole.
  template <typename Bits, typename Position>
  auto SliceMut(const Position& position) {
    auto bits = part_->template SliceMut<Bits>(position);
    const auto words = part_->MutableView();
    if constexpr (requires { words.AsDynamic(); }) {
      return DesignatedBits<Sink, decltype(bits)>{
          *write_, std::move(bits), words.AsDynamic()};
    } else {
      write_->BeforeModifyPart(*part_);
      return DesignatedBits<Sink, decltype(bits)>{
          *write_, std::move(bits), std::nullopt};
    }
  }

  // A run of the part's elements written over (LRM 7.6) by `kAssign`, the
  // part's own operation: a run of the run type's count from the position
  // `start`, and a queue's slice from `lo` through `hi` (LRM 7.10.1), each
  // reporting where it was written, `at`. The assignment compares each element
  // before writing it and answers the positions it changed, and that answer is
  // what the write is told and what this answers. What it may change is
  // announced first, so a run written and written back leaves the part
  // unchanged (LRM 9.2.2.2).
  template <auto kAssign, typename Start, typename Run>
  auto AssignSlice(
      const Start& start, const Run& run, base::SourceLocation at) {
    BeforeModifySlice(
        value::ReadPosition(start), static_cast<std::int64_t>(Run::Count()));
    return Tell(kAssign(*part_, start, run, at));
  }
  template <auto kAssign, typename Position, typename Run>
  auto AssignSlice(
      const Position& lo, const Position& hi, const Run& run,
      base::SourceLocation at) {
    BeforeModifySlice(value::ReadPosition(lo), 0);
    return Tell(kAssign(*part_, lo, hi, run, at));
  }

  auto operator*() -> Part& {
    write_->BeforeModifyPart(*part_);
    if (write_->Tells()) {
      kept_.emplace(*part_);
    }
    return *part_;
  }

  // A member reached through the designation is reached on what `*` lands on,
  // as `p->m` is `(*p).m`.
  auto operator->() -> Part* {
    return &**this;
  }

 private:
  // What a run write of `count` elements from `first` may change, announced
  // before it writes: the whole of a part whose size changes, since such a
  // write may move its elements and an announced element would lie in storage
  // the part gives back, and of a fixed-size array each element the run
  // reaches, where it lies, a bound naming no position writing nothing (LRM
  // 7.4.5).
  void BeforeModifySlice(
      std::optional<std::int64_t> first, std::int64_t count) {
    if constexpr (detail::ChangesSizeByKind<Part>) {
      write_->BeforeModifyPart(*part_);
    } else if (write_->Announces()) {
      for (const std::int64_t ordinal : PositionsASliceWrites(
               first, count, static_cast<std::int64_t>(part_->Count()))) {
        write_->BeforeModifyPart(
            part_->GetUnchecked(static_cast<std::size_t>(ordinal)),
            PartPath::Element(ordinal));
      }
    }
  }

  // The positions a run write answers it changed, told to the write.
  template <typename Changed>
  auto Tell(const Changed& changed) -> Changed {
    write_->Moved(
        *value::ReadPosition(changed.template Component<0>()),
        *value::ReadPosition(changed.template Component<1>()));
    return changed;
  }

  WriteBracket<Sink>* write_;
  Part* part_;
  std::optional<WriteOriginal<Part>> kept_;
};

// A reference to bits of a bit-addressed value within a write (LRM 11.5.1),
// named by `Bits`, the reference to them the value hands out. They lie in the
// value's own words, so the write lands on them where they are, and each write
// through them keeps what they held, writes them, and tells the write which
// bits it reached. `whole` names the words of the value they lie in, of which
// an interval keeps the original of those the bits written lie in;
// none for a value narrower than a word, which was announced whole already.
template <class Sink, class Bits>
class DesignatedBits {
 public:
  DesignatedBits(
      WriteBracket<Sink>& write, Bits bits,
      std::optional<value::IntegralView> whole)
      : write_(&write), bits_(std::move(bits)), whole_(whole) {
  }

  DesignatedBits(const DesignatedBits&) = delete;
  auto operator=(const DesignatedBits&) -> DesignatedBits& = delete;
  DesignatedBits(DesignatedBits&&) noexcept = default;
  auto operator=(DesignatedBits&&) -> DesignatedBits& = delete;
  ~DesignatedBits() = default;

  template <class Value>
  void Set(const Value& value) {
    if (whole_.has_value() && write_->Announces()) {
      if (const std::optional<value::BitPositions> reached = bits_.Reached()) {
        write_->BeforeModifyBits(*whole_, *reached);
      }
    }
    if (const std::optional<Change> change =
            WriteBits(bits_, value, write_->Tells())) {
      write_->Landed(*change);
    }
  }

  // A reference to the bits of a value of `Inner` within these, written
  // within the same write.
  template <class Inner, class Position>
  [[nodiscard]] auto SliceMut(const Position& position) const {
    auto inner = bits_.template SliceMut<Inner>(position);
    return DesignatedBits<Sink, decltype(inner)>{
        *write_, std::move(inner), whole_};
  }

 private:
  WriteBracket<Sink>* write_;
  Bits bits_;
  std::optional<value::IntegralView> whole_;
};

// A write opened in the full-expression that writes, and ended with it, which
// is when the sink is told once whether the write changed it. It names no
// place itself: the whole of what the sink stands for is designated within it,
// and the steps into parts start there. Non-copyable and non-movable:
// returning it by value from the entry that opens one relies on C++17
// mandatory copy elision.
template <class Sink>
class ScopedMutation {
 public:
  using ValueType = typename Sink::ValueType;

  explicit ScopedMutation(Sink sink) : write_(sink) {
  }

  ScopedMutation(const ScopedMutation&) = delete;
  auto operator=(const ScopedMutation&) -> ScopedMutation& = delete;
  ScopedMutation(ScopedMutation&&) = delete;
  auto operator=(ScopedMutation&&) -> ScopedMutation& = delete;

  ~ScopedMutation() {
    write_.End();
  }

  auto WholeRef() -> Designation<Sink, ValueType> {
    return Designation<Sink, ValueType>{write_, write_.Storage()};
  }

 private:
  WriteBracket<Sink> write_;
};

template <value::LyraValue T>
auto Var<T>::Mutate() -> ScopedMutation<VariableRef<T>> {
  return ScopedMutation<VariableRef<T>>{VariableRef<T>{*this}};
}

template <value::LyraValue T>
auto VariableRef<T>::Mutate() const -> ScopedMutation<VariableRef<T>> {
  return ScopedMutation<VariableRef<T>>{*this};
}

static_assert(OverridableSink<VariableRef<value::Logic>>);

}  // namespace lyra::runtime
