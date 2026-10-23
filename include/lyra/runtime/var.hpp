#pragma once

#include <algorithm>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/sampled_value.hpp"
#include "lyra/runtime/storage_for.hpp"
#include "lyra/runtime/tail.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/small_box.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::value {

template <typename T, std::int64_t kBound>
class Queue;
class RuntimeQueue;

}  // namespace lyra::value

namespace lyra::runtime {

namespace detail {

template <typename Container>
inline constexpr bool kIsQueue = false;
template <typename T, std::int64_t kBound>
inline constexpr bool kIsQueue<value::Queue<T, kBound>> = true;
template <>
inline constexpr bool kIsQueue<value::RuntimeQueue> = true;

}  // namespace detail

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
// owner is told once, when the write is over, whether it changed. Three things
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
// `PublishTransition` tells the owner the write changed it: which part of the
// owner's storage the write reached, and what can be said about which bits of
// that part moved.
//
// Each sink claims the contract with a `static_assert(MutationSink<...>)`
// beside its own definition, the way a value type claims its `lyra::value`
// concepts; the bracket itself takes the sink unconstrained, because a sink
// names the handle in its own partial-write entry's return type and checking
// the constraint there would depend on the sink being complete.
template <class S>
concept MutationSink =
    requires(S sink, const PartPath& reached, const Change& change) {
      typename S::ValueType;
      { sink.MutationStorage() } -> std::same_as<typename S::ValueType&>;
      { sink.Watched() } -> std::same_as<bool>;
      sink.PublishTransition(reached, change);
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

// The path of the one step to what `key` selects in a container. A position
// names an element by its ordinal (LRM 7.4.5), which is that step, whether it
// arrives as the position or as the ordinal read out of one; an index of any
// other type has none (LRM 7.8), so the path stops there, inexact.
template <typename Key>
[[nodiscard]] auto StepTo(const Key& key) -> PartPath {
  if constexpr (std::same_as<Key, value::Position>) {
    return PartPath::Element(value::ReadPosition(key));
  } else if constexpr (std::same_as<Key, std::optional<std::int64_t>>) {
    return PartPath::Element(key);
  } else {
    return PartPath::Inexact();
  }
}

// The same step as a reference keeps it (LRM 13.5.2). A reference to an
// element of a queue stays good while other elements are added and removed
// around it (LRM 7.10), so the ordinal it was taken at says nothing of where
// the element is later: its path stops at the queue, inexact.
template <typename Container, typename Key>
[[nodiscard]] auto RetainedStepTo(const Key& key) -> PartPath {
  if constexpr (detail::kIsQueue<Container>) {
    return PartPath::Inexact();
  } else {
    return StepTo(key);
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
        Trigger(cell_, Observation{}, {}, 0, 0));
  }
  [[nodiscard]] auto Sampled() const -> const SampledValueOf<TakenHeld>* {
    return sampled_.get();
  }

  [[nodiscard]] auto Takeovers() -> runtime::Takeovers<Owned>& {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<runtime::Takeovers<Owned>>();
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
  // the whole value it left.
  void ApplyOverriddenWrite(const Owned& written);

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
  // generation its evaluation carries (LRM 10.6).
  auto BeginTakeover(std::int64_t level, std::int64_t first, std::int64_t count)
      -> std::int64_t;

  // States what the takeover at `level` has evaluated to, as a value of the
  // cell's type of which the positions it holds are read. It reaches the cell
  // only where no higher level covers it, and is recorded either way so that
  // ending the level above it needs nobody to recompute. Answers whether the
  // evaluation offering the value still holds any position, which is how an
  // evaluation superseded by a later takeover, or ended by a `deassign` or
  // `release`, learns to stop.
  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const Owned& new_val)
      -> bool;

  // The same of a value handed as its bytes, laid out as what the cell holds
  // is.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes) -> bool
    requires requires(const HeldAs& held) {
      { held.ToOwned(bytes) } -> std::same_as<Owned>;
    };

  // Ends the procedural continuous assignment at `level` over the `count`
  // positions from `first`. Where a lower takeover or a continuous driver lies
  // beneath, it is reestablished, and each position released shows what it
  // holds once its reevaluation runs; any other variable is left exactly as it
  // stands, which is what a released variable keeps (LRM 10.6.2).
  void EndTakeover(std::int64_t level, std::int64_t first, std::int64_t count);

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
  [[nodiscard]] auto SampledGet() const -> const Owned& {
    const CellRareState<HeldAs>* rare = ExistingRareState();
    return SampledOf(rare == nullptr ? nullptr : rare->Sampled());
  }

  // Tells whoever waits on the part of the cell at `reached`, on what that
  // part lies in or on what lies in it that a write changed it (LRM 4.3),
  // passing over a wait on the part whose bits `change` shows the write left
  // alone.
  void PublishTransition(const PartPath& reached, const Change& change) {
    current_runtime().WakeParkedOn(*this, reached, change);
  }

 protected:
  Cell()
    requires std::default_initializable<HeldAs>;
  // A storage told what it holds where it is built.
  template <class Told>
  explicit Cell(const Told& told) : storage_(told) {
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
    if (!storage_.IsInstalled()) {
      throw InternalError(
          "Cell: a store reached a cell that was never initialized -- please "
          "report this as a bug");
    }
    if (!this->HasMembers()) {
      overwrite();
      return;
    }
    if (already_held()) {
      return;
    }
    PublishTransition(PartPath{}, ReplaceWhole(storage_.Get(), overwrite));
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
  Var();
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
  [[nodiscard]] auto IsInstalled() const noexcept -> bool {
    return this->Held().IsInstalled();
  }

  // The write a declaration makes, which may run more than once: a declaration
  // reached again begins a fresh variable in the one storage. A value whose
  // type states its representation is overwritten every time. A value
  // represented at run time takes its representation from the first such
  // write and is overwritten at it by each later one, so what is fixed is the
  // representation, not the number of times a declaration runs: a value that
  // does not match the installed one is the lowering defect and is what
  // refuses, and so is a store into such a cell before any declaration ran,
  // which the store path refuses.
  void Initialize(const T& prototype) {
    if (!this->Held().IsInstalled()) {
      this->Held().Install(prototype);
      return;
    }
    this->Held().Overwrite(prototype);
  }
  void Initialize(T&& prototype) {
    if (!this->Held().IsInstalled()) {
      this->Held().Install(std::move(prototype));
      return;
    }
    this->Held().Overwrite(prototype);
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
  [[nodiscard]] auto SampledBytes() const -> const void* {
    return this->SampledGet().Bytes();
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

// What a write keeps of the part it lands on, to tell afterwards what it did
// to it: a copy, of which nothing but whether it moved can be shown. The write
// lies in the frame of whatever writes, so the copy is kept as a frame keeps a
// value aside.
template <class Part>
class KeptPart {
 public:
  explicit KeptPart(const Part& part);
  KeptPart(const KeptPart&) = delete;
  auto operator=(const KeptPart&) -> KeptPart& = delete;
  KeptPart(KeptPart&&) = delete;
  auto operator=(KeptPart&&) -> KeptPart& = delete;
  ~KeptPart();

  // What the write did to `part`, none where it left it as it was.
  [[nodiscard]] auto ChangeTo(const Part& part) -> std::optional<Change>;

 private:
  value::SmallBox<Part> before_;
};

template <class Part>
KeptPart<Part>::KeptPart(const Part& part) : before_(part) {
}

template <class Part>
KeptPart<Part>::~KeptPart() = default;

template <class Part>
auto KeptPart<Part>::ChangeTo(const Part& part) -> std::optional<Change> {
  if (before_->IsBitIdentical(part)) {
    return std::nullopt;
  }
  return Change::Whole();
}

// A bit-addressed part keeps only the words it lies in, as a change reaching
// its bits, so a wait is compared on them by position. Some of a bit-addressed
// value's bits are a part a write lands on as well, kept as the words of the
// value they lie in.
template <BitAddressed Part>
class KeptPart<Part> {
 public:
  explicit KeptPart(const Part& part) : reached_(KeptBefore(part)) {
  }
  KeptPart(const Part& storage, value::BitPositions reached)
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
  KeptPart<std::remove_cvref_t<decltype(bits.Root())>> kept(
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
  // reference changed the part at `within` of what the reference names
  // (LRM 4.3), `change` saying what it did to that part. A wait on an object
  // reevaluates the expression that reached it, which decides whether the
  // write was an event, so it is told nothing about what moved.
  void Report(const PartPath& within, const Change& change) const;

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
  // storage -- an associative entry made by being bound (LRM 7.8.7). An index
  // naming no element names storage that belongs to nothing (LRM 7.4.6).
  [[nodiscard]] auto Part(
      void* storage, value::Formation formed, const PartPath& within) const
      -> ErasedReference;
};

// A write of a whole value through `lent` (LRM 4.3, 10.6): one the variable
// overrides is handed to it as `overridden` hands it, one nobody waits on is
// written as `assign` writes it, and one that leaves the bits as `identical`
// finds them is no change. Otherwise `replace` writes it and answers the
// change, which whoever waits through `lent` is told.
template <class Overridden, class Assign, class Identical, class Replace>
void SetThrough(
    const ErasedReference& lent, Overridden overridden, Assign assign,
    Identical identical, Replace replace) {
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
  lent.Report(PartPath{}, replace());
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
      std::int64_t level, std::int64_t first, std::int64_t count) const
      -> std::int64_t;
  [[nodiscard]] auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& new_val) const
      -> bool;
  void EndTakeover(
      std::int64_t level, std::int64_t first, std::int64_t count) const;

  // A reference to an element or a component of what this one names (LRM
  // 13.5.2), which belongs to the same variable.
  template <typename Key>
  [[nodiscard]] auto ReferElement(const Key& key) const {
    erased_.AdmitStep();
    value::Formation formed{};
    auto& element = Storage().ElementMut(key, formed);
    return VariableRef<std::remove_reference_t<decltype(element)>>{
        erased_.Part(&element, formed, RetainedStepTo<T>(key))};
  }

  template <std::size_t I>
  [[nodiscard]] auto ReferComponent() const {
    auto& component = Storage().template Component<I>();
    return VariableRef<std::remove_reference_t<decltype(component)>>{
        erased_.Part(
            &component, value::Formation::kExisting, PartPath::Child(I))};
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
  [[nodiscard]] auto SampledGet() const -> const T& {
    return std::visit(
        Overloaded{
            [&](std::monostate) -> const T& { return Storage(); },
            [&](VariableCell*) -> const T& {
              if (!Whole()) {
                throw SimulationError(
                    "a sampled value of part of a variable lent by reference "
                    "is not yet supported");
              }
              return Cell().SampledGet();
            },
            [](GcObject*) -> const T& {
              throw SimulationError(
                  "a sampled value of a class property lent by reference is "
                  "not yet supported");
            },
            [](HeldByNet) -> const T& {
              throw SimulationError(
                  "a sampled value of a net lent by reference is not yet "
                  "supported");
            }},
        erased_.holder);
  }

  // A write through the reference changed the part at `within` of what it
  // names, which lies where the reference's own steps led.
  void PublishTransition(const PartPath& within, const Change& change) const {
    erased_.Report(within, change);
  }

 private:
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
// 13.5.2), given whatever reaches the object: the running method's own object,
// or a handle. The property is a step taken on the object, so the object is
// reached once. Reaching a property through a handle naming no object is the
// design's own failure.
template <auto Property>
auto ReferProperty(typename MemberOf<decltype(Property)>::Class* object)
    -> VariableRef<typename MemberOf<decltype(Property)>::Value> {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return VariableRef<typename MemberOf<decltype(Property)>::Value>{
      ErasedReference{
          .holder = static_cast<GcObject*>(object),
          .storage = &(object->*Property)}};
}

template <auto Property>
auto ReferProperty(const value::ObjectRef& handle)
    -> VariableRef<typename MemberOf<decltype(Property)>::Value> {
  return ReferProperty<Property>(
      handle.View<typename MemberOf<decltype(Property)>::Class>());
}

// What a wait on the storage `reference` names enrols on (LRM 13.5.2): what
// a write through the reference is told to, never the reference itself.
template <value::LyraValue T>
auto ReportsTo(const VariableRef<T>& reference) -> WatchedPlace {
  return WatchedPlace::Through(reference.Erased());
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
    std::int64_t level, std::int64_t first, std::int64_t count)
    -> std::int64_t {
  return std::int64_t{RareState().Takeovers().Begin(
      TakeoverLevelOf(level), PositionsNamed(first, count))};
}

template <class HeldAs>
auto Cell<HeldAs>::DriveTakeover(
    std::int64_t level, std::int64_t generation, const Owned& new_val) -> bool {
  return RecordTakeover(level, generation, new_val);
}

template <class HeldAs>
auto Cell<HeldAs>::DriveTakeoverBytes(
    std::int64_t level, std::int64_t generation, const void* bytes) -> bool
  requires requires(const HeldAs& held) {
    { held.ToOwned(bytes) } -> std::same_as<Owned>;
  }
{
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
    std::int64_t level, std::int64_t first, std::int64_t count) {
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
  runtime.Submit(runtime.Now(), Region::kActive, [this] { Reevaluate(); });
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
//     (LRM 7.4.6), and either answers the question whatever is written;
//   - a slice write compares each element before writing it, so it says
//     whether it moved one;
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
  }

  [[nodiscard]] auto Storage() const -> ValueType& {
    return *storage_;
  }

  // Whether the part a write lands on is worth keeping from before the write:
  // something reads the answer, and no step has given it already.
  [[nodiscard]] auto Undecided() const -> bool {
    return watched_ && Open();
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

  // A write of the `count` elements from `start` of the part reached (LRM 7.6)
  // moved at least one of them. Those lying before the first element are no
  // elements of it.
  void Moved(std::int64_t start, std::int64_t count) {
    if (!Open()) {
      return;
    }
    outcome_ = Outcome::kChanged;
    const std::int64_t first = std::max<std::int64_t>(start, 0);
    moved_ = {
        .lsb = static_cast<std::uint64_t>(first),
        .width = static_cast<std::uint64_t>(start + count - first)};
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
    sink_.PublishTransition(reached_, change);
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
          sink_.PublishTransition(reached_, Change::Elements(moved_));
        }
        return;
    }
    throw InternalError("WriteBracket: unknown outcome");
  }

 private:
  enum class Outcome : std::uint8_t { kUndecided, kChanged, kSettled };

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

  template <typename Key>
  auto DesignateElement(const Key& key) {
    value::Formation formed{};
    auto& element = part_->ElementMut(key, formed);
    write_->Formed(formed);
    if (write_->Undecided()) {
      write_->Stepped(StepTo(key));
    }
    return Designation<Sink, std::remove_reference_t<decltype(element)>>{
        *write_, element};
  }

  template <std::size_t I>
  auto DesignateComponent() {
    auto& component = part_->template Component<I>();
    if (write_->Undecided()) {
      write_->Stepped(PartPath::Child(I));
    }
    return Designation<Sink, std::remove_reference_t<decltype(component)>>{
        *write_, component};
  }

  // A member of a union lies where every other member does (LRM 7.3), so no
  // step of a path stands for it: the path stops at the union, which every
  // member reads, while the write keeps only what it lands on below it.
  template <std::size_t I>
  auto DesignateMember() {
    auto& member = part_->template Component<I>();
    if (write_->Undecided()) {
      write_->Stepped(PartPath::Inexact());
    }
    return Designation<Sink, std::remove_reference_t<decltype(member)>>{
        *write_, member};
  }

  // A reference to the bits of a value of `Bits` at `position` within the
  // part (LRM 11.5.1), written within this write. They lie in the part's own
  // words, so a write through it lands on them where they are.
  template <typename Bits, typename Position>
  auto SliceMut(const Position& position) {
    auto bits = part_->template SliceMut<Bits>(position);
    return DesignatedBits<Sink, decltype(bits)>{*write_, std::move(bits)};
  }

  // `value` written into `count` elements of the part from `start` (LRM 7.6),
  // several elements rather than one place, so the write learns what moved
  // from the assignment, which compares each element before writing it. A
  // start that names no position assigns nothing.
  template <typename Position, typename Value>
  void AssignSlice(
      const Position& start, std::int64_t count, const Value& value) {
    if (part_->AssignSlice(start, count, value)) {
      write_->Moved(*value::ReadPosition(start), count);
    }
  }

  // `value` written into the elements of a queue's slice from `lo` through
  // `hi` (LRM 7.6, 7.10.1), the ones the queue's clamping names; the write
  // learns from the assignment which it wrote, where any moved.
  template <typename Position, typename Value>
  void AssignSlice(const Position& lo, const Position& hi, const Value& value) {
    if (const auto written = part_->AssignSlice(lo, hi, value)) {
      write_->Moved(
          static_cast<std::int64_t>(written->start),
          static_cast<std::int64_t>(written->count));
    }
  }

  auto operator*() -> Part& {
    if (write_->Undecided()) {
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
  WriteBracket<Sink>* write_;
  Part* part_;
  std::optional<KeptPart<Part>> kept_;
};

// A reference to bits of a bit-addressed value within a write (LRM 11.5.1),
// named by `Bits`, the reference to them the value hands out. They lie in the
// value's own words, so the write lands on them where they are, and each write
// through them keeps what they held, writes them, and tells the write which
// bits it reached.
template <class Sink, class Bits>
class DesignatedBits {
 public:
  DesignatedBits(WriteBracket<Sink>& write, Bits bits)
      : write_(&write), bits_(std::move(bits)) {
  }

  DesignatedBits(const DesignatedBits&) = delete;
  auto operator=(const DesignatedBits&) -> DesignatedBits& = delete;
  DesignatedBits(DesignatedBits&&) noexcept = default;
  auto operator=(DesignatedBits&&) -> DesignatedBits& = delete;
  ~DesignatedBits() = default;

  template <class Value>
  void Set(const Value& value) {
    if (const std::optional<Change> change =
            WriteBits(bits_, value, write_->Undecided())) {
      write_->Landed(*change);
    }
  }

  // A reference to the bits of a value of `Inner` within these, written
  // within the same write.
  template <class Inner, class Position>
  [[nodiscard]] auto SliceMut(const Position& position) const {
    auto inner = bits_.template SliceMut<Inner>(position);
    return DesignatedBits<Sink, decltype(inner)>{*write_, std::move(inner)};
  }

 private:
  WriteBracket<Sink>* write_;
  Bits bits_;
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
