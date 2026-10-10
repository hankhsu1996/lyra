#pragma once

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
#include "lyra/base/time.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/wide.hpp"

namespace lyra::runtime {

// A value a wait reads by bit position (LRM 9.4.2): an integral value, of a
// type known where it is compiled or one held as its words at a width it was
// told. A write into one says which of its bits it reached; a write into any
// other value says only that it changed it.
template <class T>
concept BitAddressed = value::IntegralValue<T> || value::HeldAsWords<T>;

// Answers `read` asked of the planes and the width of a bit-addressed value.
template <BitAddressed T, class Read>
auto ReadPlanes(const T& bits, Read read) -> decltype(auto) {
  if constexpr (value::IntegralValue<T>) {
    const typename T::Words words = bits.Load();
    return read(words.Read(), T::kWidth);
  } else {
    return read(bits.Read(), bits.Width());
  }
}

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
    const typename T::Words words = storage.Load();
    std::uint64_t unknown = 0;
    if constexpr (T::kFourState) {
      unknown = words.unknown[0];
    }
    return Change::ReachingInWord(
        words.value[0], unknown,
        reached.value_or(value::BitPositions{.lsb = 0, .width = T::kWidth}));
  } else {
    return ReadPlanes(
        storage, [&](value::ConstPlanes planes, std::uint64_t width) {
          return Change::Reaching(
              planes,
              reached.value_or(value::BitPositions{.lsb = 0, .width = width}));
        });
  }
}

// The same bits of `storage` once the write has landed.
template <BitAddressed T>
void KeepAfter(Change& change, const T& storage) {
  if constexpr (HeldInOneWord<T>) {
    const typename T::Words words = storage.Load();
    std::uint64_t unknown = 0;
    if constexpr (T::kFourState) {
      unknown = words.unknown[0];
    }
    change.SetAfterInWord(words.value[0], unknown);
  } else {
    ReadPlanes(storage, [&](value::ConstPlanes planes, std::uint64_t) {
      change.SetAfter(planes);
    });
  }
}

// What a write into part of an owner's storage writes through (LRM 11.5.1).
// The write reaches the owner's storage and lands its part there directly; the
// owner is told once, when the write is over, whether it changed. Four things
// that takes, named for the role rather than for either owner's vocabulary.
//
// `AdmitsWrite` is whether the write goes into the owner at all: one does not
// while a procedural continuous assignment holds a variable (LRM 10.6). Asking
// is the first write of the time slot at the latest, so a sampled variable
// keeps its value from before it here (LRM 16.5.1).
//
// `DisplacedStorage` is where a write turned away lands so that it is kept:
// what a variable driven continuously holds beneath the assignment covering it
// (LRM 10.6.2). Where there is none the write lands in a copy it keeps itself
// and nobody reads.
//
// `MutationStorage` is the storage itself, so a write reaches the part it
// names and disturbs nothing else. Two things follow: a write cannot lose one
// performed through another while it was open, and what writing one element
// costs does not scale with the size of the whole value.
//
// `Watched` is whether anything reads the answer. A variable cell no wait is
// enrolled on has no one to tell (LRM 4.3), so the write keeps nothing from
// before it; a net driver always has its net, which resolves again only where
// the driver's own contribution moved (LRM 6.5).
//
// `PublishTransition` tells the owner the write changed it, with what can be
// said about which of its bits moved.
//
// Each sink claims the contract with a `static_assert(MutationSink<...>)`
// beside its own definition, the way a value type claims its `lyra::value`
// concepts; the bracket itself takes the sink unconstrained, because a sink
// names the handle in its own partial-write entry's return type and checking
// the constraint there would depend on the sink being complete.
template <class S>
concept MutationSink = requires(S sink, const Change& change) {
  typename S::ValueType;
  { sink.AdmitsWrite() } -> std::same_as<bool>;
  { sink.DisplacedStorage() } -> std::same_as<typename S::ValueType*>;
  { sink.MutationStorage() } -> std::same_as<typename S::ValueType&>;
  { sink.Watched() } -> std::same_as<bool>;
  sink.PublishTransition(change);
};

template <class Sink>
class ScopedMutation;

template <value::LyraValue T>
class Ref;

// What only a variable something samples or takes over has to do before a
// write lands in it: keep its value from before the time slot's first change
// (LRM 16.5.1), and turn the write away while a procedural continuous
// assignment holds it (LRM 10.6). Almost no variable is either, so this exists
// only once one is, behind the variable, and the variable asks it for nothing
// otherwise.
class RareWriteState {
 public:
  RareWriteState();
  RareWriteState(const RareWriteState&) = delete;
  auto operator=(const RareWriteState&) -> RareWriteState& = delete;
  RareWriteState(RareWriteState&&) = delete;
  auto operator=(RareWriteState&&) -> RareWriteState& = delete;
  virtual ~RareWriteState();

  // Before a write lands: keeps the value the variable held when the slot
  // began, the first time in the slot, and answers whether the write lands.
  [[nodiscard]] virtual auto Admit() -> bool = 0;
  [[nodiscard]] virtual auto TakenOver() const -> bool = 0;
};

// What every variable is whatever it holds: the waits enrolled on it, and
// whatever a sampled or taken-over variable has to do before a write lands. A
// write that knows the variable only as this -- one through a reference to a
// part of it, which knows the part's type and not the variable's -- asks it
// everything it needs.
class VariableCell : public Observable {
 public:
  VariableCell(const VariableCell&) = delete;
  auto operator=(const VariableCell&) -> VariableCell& = delete;
  VariableCell(VariableCell&&) = delete;
  auto operator=(VariableCell&&) -> VariableCell& = delete;

  // Written here rather than in the library's own source because every write
  // asks them, and for a variable nothing samples or takes over each is a null
  // test.
  [[nodiscard]] auto AdmitsWrite() -> bool {
    return rare_ == nullptr || rare_->Admit();
  }
  // Whether a write into the variable has anyone to tell what it did: a wait
  // enrolled here (LRM 4.3). Under a procedural continuous assignment a write
  // lands where nothing reads it (LRM 10.6), so it has nothing to tell either.
  [[nodiscard]] auto Watched() const -> bool {
    return HasMembers() && (rare_ == nullptr || !rare_->TakenOver());
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

// What a sampled or taken-over variable of type `T` keeps: the value it held
// when the current slot began, and the slot that value belongs to (LRM
// 16.5.1); and the procedural continuous assignments it has been put under
// (LRM 10.6). Each part is empty until its feature is used.
template <value::LyraValue T>
class CellRareState final : public RareWriteState {
 public:
  explicit CellRareState(const T& value) : value_(&value) {
  }
  CellRareState(const CellRareState&) = delete;
  auto operator=(const CellRareState&) -> CellRareState& = delete;
  CellRareState(CellRareState&&) = delete;
  auto operator=(CellRareState&&) -> CellRareState& = delete;
  ~CellRareState() override;

  auto Admit() -> bool override {
    KeepPreponed();
    return !TakenOver();
  }
  [[nodiscard]] auto TakenOver() const -> bool override {
    return takeovers_ != nullptr && takeovers_->Highest() != nullptr;
  }

  // Arms the variable to answer for its sampled value, taking what it holds
  // now, stamped with time zero.
  void ArmSampling() {
    if (retained_.has_value()) {
      return;
    }
    retained_ = *value_;
    retained_slot_ = SimTime{};
  }

  [[nodiscard]] auto Retained() const -> const std::optional<T>& {
    return retained_;
  }
  [[nodiscard]] auto RetainedSlot() const -> SimTime {
    return retained_slot_;
  }

  // Keeps what the variable held in the Preponed region of the current slot,
  // the first time a write reaches it in that slot (LRM 4.4.2.1, 16.5.1).
  // Nothing in the slot has changed it before its first write, so what it
  // holds then is that value, whether or not the write goes on to change it.
  void KeepPreponed() {
    if (!retained_.has_value()) {
      return;
    }
    const SimTime now = current_runtime().Now();
    if (retained_slot_ == now) {
      return;
    }
    retained_ = *value_;
    retained_slot_ = now;
  }

  [[nodiscard]] auto Takeovers() -> runtime::Takeovers<T>& {
    if (takeovers_ == nullptr) {
      takeovers_ = std::make_unique<runtime::Takeovers<T>>();
    }
    return *takeovers_;
  }
  [[nodiscard]] auto ExistingTakeovers() const -> runtime::Takeovers<T>* {
    return takeovers_.get();
  }

 private:
  const T* value_;
  std::optional<T> retained_;
  SimTime retained_slot_ = SimTime{};
  std::unique_ptr<runtime::Takeovers<T>> takeovers_;
};

template <value::LyraValue T>
CellRareState<T>::~CellRareState() = default;

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

template <value::LyraValue T>
class Var : public VariableCell, public ValueStorageCore<T> {
 public:
  Var();
  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var();

  // The write a declaration makes, which may run more than once: a declaration
  // reached again begins a fresh variable in the one storage. A value whose
  // type states its representation is overwritten every time. A value
  // represented at run time takes its representation from the first such
  // write and is overwritten at it by each later one, so what is fixed is the
  // representation, not the number of times a declaration runs: a value that
  // does not match the installed one is the lowering defect and is what
  // refuses, and so is a store into such a cell before any declaration ran,
  // which the store path refuses.
  void Initialize(T prototype) {
    if (!this->IsInstalled()) {
      this->Install(std::move(prototype));
      return;
    }
    this->Overwrite(prototype);
  }

  // Commits a whole-variable write and, on a real change (LRM 4.3 update
  // event), wakes whoever waits through the engine. The engine is the ambient
  // one: it has the standing of a stack pointer, so a store does not carry it.
  //
  // A procedural continuous assignment overrides the procedural writes a cell
  // takes on its own (LRM 10.6), so while one is in effect the value arriving
  // here is discarded outright rather than held anywhere: after the takeover
  // ends the cell keeps what the takeover last gave it, and never the write
  // that was overridden.
  void Set(const T& new_val);

  // The same write of a value handed as its bytes, as wide as what the cell
  // holds, which the cell compares with and takes into the words it has.
  void SetBytes(const void* bytes)
    requires value::TakesBytesInPlace<T>
  {
    if (TakenOver()) {
      if (T* beneath = DisplacedStorage()) {
        beneath->TakeBytes(bytes);
      }
      return;
    }
    StoreBy(
        [&] { return this->Get().HoldsBytes(bytes); },
        [&] { this->Storage().TakeBytes(bytes); });
  }

  // Starts a procedural continuous assignment at `level`, superseding whatever
  // was driving that level, and answers with the generation its evaluation
  // carries (LRM 10.6).
  auto BeginTakeover(std::int64_t level) -> std::int64_t;

  // States what the takeover at `level` has evaluated to. It reaches the cell
  // only when no higher level covers it, and is recorded either way so that
  // ending the level above it needs nobody to recompute. Answers whether the
  // evaluation offering the value is still the one driving that level, which
  // is how an evaluation superseded by a later takeover, or ended by a
  // `deassign` or `release`, learns to stop.
  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& new_val) -> bool;

  // The same of a value handed as its bytes, as wide as what the cell holds,
  // which the level takes into the words it has once it holds a value.
  auto DriveTakeoverBytes(
      std::int64_t level, std::int64_t generation, const void* bytes) -> bool
    requires value::TakesBytesInPlace<T>;

  // Ends the procedural continuous assignment at `level`, handing the cell to
  // the highest level still in effect. Where none is left a cell something
  // drives continuously takes what its driver last produced, and any other is
  // left exactly as it stands, which is what a released variable keeps (LRM
  // 10.6.2).
  void EndTakeover(std::int64_t level);

  // Where a write a procedural continuous assignment turned away lands so that
  // it is kept: what the cell's continuous driver last produced, held beneath
  // the assignment. None where nothing drives the cell continuously, or
  // nothing covers it.
  [[nodiscard]] auto DisplacedStorage() const -> T* {
    const CellRareState<T>* rare = ExistingRareState();
    Takeovers<T>* takeovers =
        rare == nullptr ? nullptr : rare->ExistingTakeovers();
    return takeovers == nullptr ? nullptr : takeovers->Beneath();
  }

  // Arms the cell to answer for its sampled value (LRM 16.5.1) and installs the
  // one every read answers with until the first change of some later slot.
  //
  // A static variable's default sampled value is the value its declaration
  // assigns (LRM 16.5.1), which is in the cell once time-zero initialization
  // has run (LRM 10.5), so arming takes what it finds there. Stamping it at
  // time zero is what makes the whole of time zero answer with it: a write then
  // finds the slot already current and leaves the retained value alone.
  //
  // More than one read may name one cell, and they all want the same answer, so
  // arming an armed cell is not an error and changes nothing.
  void ArmSampling() {
    RareState().ArmSampling();
  }

  // What the cell held in the Preponed region of the current time slot -- its
  // value before anything in that slot ran (LRM 4.4.2.1, 16.5.1). Once the slot
  // has changed the cell that value is the retained one; until then the cell
  // still holds it.
  [[nodiscard]] auto SampledGet() const -> const T& {
    const CellRareState<T>* rare = ExistingRareState();
    if (rare == nullptr || !rare->Retained().has_value()) {
      throw InternalError(
          "Var::SampledGet: a sampled value was read from a cell nothing armed "
          "to answer for one");
    }
    if (rare->RetainedSlot() == current_runtime().Now()) {
      return *rare->Retained();
    }
    return this->Get();
  }

  // Tells whoever waits here that a write changed the cell (LRM 4.3), passing
  // over a wait whose bits `change` shows the write left alone.
  void PublishTransition(const Change& change) {
    current_runtime().WakeParkedOn(this->Members(), change);
  }

  // Opens a write into the cell for the full-expression that writes. The cell's
  // own storage is designated within it, the steps into parts are taken from
  // there, and the write lands where they are dereferenced, so a part is
  // written in the cell as it lies and the cell is told once whether it
  // changed. The write is non-copyable and non-movable, so keeping it past the
  // statement is rejected at compile time.
  auto Mutate() -> ScopedMutation<Ref<T>>;

 private:
  // The one path a whole value reaches the cell's storage by, whoever sent it.
  // What it adds over a partial write is the representation match, which only
  // a whole value can be checked for -- a partial write lands a part and never
  // restates the whole. The value arriving is compared with the one it would
  // replace before anything is written, so telling whoever waits keeps no copy
  // of the old value, except the old words a packed value's waits are passed
  // over by.
  void Store(const T& new_val) {
    StoreBy(
        [&] { return this->Get().IsBitIdentical(new_val); },
        [&] { this->Overwrite(new_val); });
  }

  // That path over however the arriving value is held: `already_held` answers
  // whether the cell holds the same bits, and `overwrite` replaces what it
  // holds.
  template <class AlreadyHeld, class Overwrite>
  void StoreBy(AlreadyHeld already_held, Overwrite overwrite) {
    if (!this->IsInstalled()) {
      throw InternalError("Var: store into a cell that was never initialized");
    }
    if (CellRareState<T>* rare = ExistingRareState()) {
      rare->KeepPreponed();
    }
    if (!this->HasMembers()) {
      overwrite();
      return;
    }
    if (already_held()) {
      return;
    }
    PublishTransition(ReplaceWhole(this->Get(), overwrite));
  }

  // Whether a procedural continuous assignment shows through the cell (LRM
  // 10.6).
  [[nodiscard]] auto TakenOver() const -> bool {
    const CellRareState<T>* rare = ExistingRareState();
    return rare != nullptr && rare->TakenOver();
  }

  // Has the takeover at `level` record what it evaluated to, through `record`,
  // and shows whatever the highest level in effect then holds.
  template <class Record>
  auto RecordTakeover(
      std::int64_t level, std::int64_t generation, Record record) -> bool;

  // What only a sampled or taken-over cell keeps, made the first time either
  // is asked for. This cell is the only thing that makes it, so it is always
  // of the cell's own type.
  [[nodiscard]] auto RareState() -> CellRareState<T>& {
    if (Rare() == nullptr) {
      InstallRare(std::make_unique<CellRareState<T>>(this->Get()));
    }
    return static_cast<CellRareState<T>&>(*Rare());
  }
  [[nodiscard]] auto ExistingRareState() const -> CellRareState<T>* {
    return static_cast<CellRareState<T>*>(Rare());
  }
};

// What a write keeps of the part it lands on, to tell afterwards what it did
// to it: a copy, of which nothing but whether it moved can be shown.
template <class Part>
class KeptPart {
 public:
  explicit KeptPart(const Part& part) : before_(part) {
  }

  // What the write did to `part`, none where it left it as it was.
  [[nodiscard]] auto ChangeTo(const Part& part) -> std::optional<Change> {
    if (before_.IsBitIdentical(part)) {
      return std::nullopt;
    }
    return Change::Whole();
  }

 private:
  Part before_;
};

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
    bits = value;
    return std::nullopt;
  }
  KeptPart<std::remove_cvref_t<decltype(bits.Root())>> kept(
      bits.Root(), *reached);
  bits = value;
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

// What holds the storage a reference names, which is what a write through the
// reference is a write of (LRM 13.5.2): a variable something may wait on, the
// object a class property belongs to (LRM 9.4.2), or nothing anyone is told
// about -- a variable of a function, which nothing can wait on (LRM 13.4.4), a
// closure's own copy of a variable, which no other process reaches, and an
// element an index named none of.
using StorageHolder = std::variant<std::monostate, VariableCell*, GcObject*>;

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
  // to a child's port, one a wait was built from -- still says which member it
  // is a copy of. None for a reference bound into no member.
  ErasedReference* member = nullptr;

  // Written here rather than in the library's own source because every write
  // through a reference asks them, and for storage whose variable nothing
  // samples or takes over each is a test or two.

  // Whether a write through the reference now lands where it names. Only a
  // variable can be put under a procedural continuous assignment (LRM 10.6).
  [[nodiscard]] auto Admits() const -> bool {
    VariableCell* const* variable = std::get_if<VariableCell*>(&holder);
    return variable == nullptr || (*variable)->AdmitsWrite();
  }

  [[nodiscard]] auto Watched() const -> bool {
    if (VariableCell* const* variable = std::get_if<VariableCell*>(&holder)) {
      return (*variable)->Watched();
    }
    GcObject* const* object = std::get_if<GcObject*>(&holder);
    return object != nullptr && (*object)->Watched();
  }

  // The rest are defined in the library's own source: none depends on the type
  // of the value the reference names, and none is on every write's path.

  // Tells whoever waits on what holds the storage that a write through the
  // reference changed it (LRM 4.3). A wait on an object reevaluates the
  // expression that reached it, which decides whether the write was an event,
  // so it is told nothing about which bits moved.
  void Report(const Change& change) const;

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

  // The reference to a part of what this one names, at `part`, which forming
  // did `formed` to. The part has the same holder, which is told at once where
  // forming it changed the storage -- an associative entry made by being bound
  // (LRM 7.8.7). An index naming no element names storage that belongs to
  // nothing (LRM 7.4.6).
  [[nodiscard]] auto Part(void* part, value::Formation formed) const
      -> ErasedReference;
};

// The variable a reference to the whole of it names, whose value is then of
// the reference's own type `T`.
template <value::LyraValue T>
auto WholeVariable(const ErasedReference& reference) -> Var<T>& {
  return static_cast<Var<T>&>(**std::get_if<VariableCell*>(&reference.holder));
}

// A reference, typed by the value it names. A reference to the whole of a
// variable and one to a part of it are the same thing, so a write through
// either is the variable's write, told to whoever waits on the variable at the
// moment it lands (LRM 4.3). Copyable, so a ref formal can be forwarded as a
// ref argument to a nested call.
template <value::LyraValue T>
class Ref {
 public:
  using ValueType = T;

  // A null view, default-constructed as a member and bound before first use:
  // a `ref` port's child-side member is declared with the child and filled by
  // the parent during elaboration (LRM 23.3.3.2), before simulation reads it.
  Ref() = default;
  explicit Ref(Var<T>& cell)
      : erased_{
            .holder = static_cast<VariableCell*>(&cell),
            .storage = &cell.Storage(),
            .whole = true} {
  }
  explicit Ref(T& storage) : erased_{.holder = {}, .storage = &storage} {
  }
  explicit Ref(const ErasedReference& erased) : erased_(erased) {
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

  // Const: a `Ref` is a view, so `Set` writes the referenced storage, not the
  // handle's own pointers -- as `*p = v` is allowed through a `T* const p`.
  void Set(const T& new_val) const {
    if (!erased_.Admits()) {
      if (T* beneath = DisplacedStorage()) {
        *beneath = new_val;
      }
      return;
    }
    if (!erased_.Watched()) {
      StoreInto(Storage(), new_val);
      return;
    }
    if (Storage().IsBitIdentical(new_val)) {
      return;
    }
    erased_.Report(
        ReplaceWhole(Storage(), [&] { StoreInto(Storage(), new_val); }));
  }

  // Opens a write into the referenced storage, as an observable cell itself
  // does; only storage that belongs to a variable has anyone to tell what the
  // write did.
  [[nodiscard]] auto Mutate() const -> ScopedMutation<Ref<T>>;

  // A procedural continuous assignment on a name that owns no storage (LRM
  // 10.6.2), by the operations a variable itself answers: the reference is a
  // member of a scope standing for what its connection drives, and the
  // assignment gives it storage of its own to name until it ends.
  auto BeginTakeover(std::int64_t level) const -> std::int64_t;
  auto DriveTakeover(
      std::int64_t level, std::int64_t generation, const T& new_val) const
      -> bool;
  void EndTakeover(std::int64_t level) const;

  // A reference to an element or a component of what this one names (LRM
  // 13.5.2), which belongs to the same variable.
  template <typename Key>
  [[nodiscard]] auto ReferElement(const Key& key) const {
    erased_.AdmitStep();
    value::Formation formed{};
    auto& element = Storage().ElementRef(key, formed);
    return Ref<std::remove_reference_t<decltype(element)>>{
        erased_.Part(&element, formed)};
  }

  template <std::size_t I>
  [[nodiscard]] auto ReferComponent() const {
    auto& component = Storage().template ComponentRef<I>();
    return Ref<std::remove_reference_t<decltype(component)>>{
        erased_.Part(&component, value::Formation::kExisting)};
  }

  // The `MutationSink` surface. Storage that belongs to no variable has no
  // observation at all, so nothing reads what a write did to it; that is the
  // same answer a variable gives while nothing waits on it, reached by a
  // shorter route.
  [[nodiscard]] auto AdmitsWrite() const -> bool {
    return erased_.Admits();
  }
  // Only a reference to the whole of a variable knows the variable's type,
  // which is what a value kept beneath an assignment is of.
  [[nodiscard]] auto DisplacedStorage() const -> T* {
    return Whole() ? Cell().DisplacedStorage() : nullptr;
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
  // the value a time slot moved away from, and keeps it whole, so a reference
  // to a part of one has none to answer with; and nothing keeps a class
  // property's Preponed value.
  void ArmSampling() const {
    if (Whole()) {
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
            }},
        erased_.holder);
  }

  void PublishTransition(const Change& change) const {
    erased_.Report(change);
  }

 private:
  [[nodiscard]] auto Storage() const -> T& {
    return *static_cast<T*>(erased_.storage);
  }

  [[nodiscard]] auto Whole() const -> bool {
    return erased_.whole;
  }

  [[nodiscard]] auto Cell() const -> Var<T>& {
    return WholeVariable<T>(erased_);
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
    -> Ref<typename MemberOf<decltype(Property)>::Value> {
  if (object == nullptr) {
    value::RaiseNullObjectHandleAccess();
  }
  return Ref<typename MemberOf<decltype(Property)>::Value>{ErasedReference{
      .holder = static_cast<GcObject*>(object),
      .storage = &(object->*Property)}};
}

template <auto Property>
auto ReferProperty(const value::ObjectRef& handle)
    -> Ref<typename MemberOf<decltype(Property)>::Value> {
  return ReferProperty<Property>(
      handle.View<typename MemberOf<decltype(Property)>::Class>());
}

// What a wait on the storage `reference` names enrols on (LRM 13.5.2): what
// a write through the reference is told to, never the reference itself.
template <value::LyraValue T>
auto ReportsTo(const Ref<T>& reference) -> WatchedPlace {
  return WatchedPlace::Through(reference.Erased());
}

// States that a continuous assignment drives the storage `reference` names
// (LRM 10.3), once, as the design is built.
template <value::LyraValue T>
void DrivesContinuously(const Ref<T>& reference) {
  reference.Erased().DriveContinuously();
}

// Defaulted here rather than where they are declared: a constructor or
// destructor defaulted on its first declaration is not user-provided, so a unit
// constructing a cell would define it itself with everything it reaches, and a
// family stated as already compiled would not withhold it.
template <value::LyraValue T>
Var<T>::Var() = default;

template <value::LyraValue T>
Var<T>::~Var() = default;

template <value::LyraValue T>
void Var<T>::Set(const T& new_val) {
  // A procedural write is discarded outright while any takeover shows through
  // this cell (LRM 10.6). A cell nobody has ever taken over holds no record at
  // all, so the ordinary write pays one null test.
  if (TakenOver()) {
    if (T* beneath = DisplacedStorage()) {
      *beneath = new_val;
    }
    return;
  }
  Store(new_val);
}

template <value::LyraValue T>
auto Var<T>::BeginTakeover(std::int64_t level) -> std::int64_t {
  return std::int64_t{RareState().Takeovers().Begin(TakeoverLevelOf(level))};
}

template <value::LyraValue T>
auto Var<T>::DriveTakeover(
    std::int64_t level, std::int64_t generation, const T& new_val) -> bool {
  return RecordTakeover(
      level, generation, [&](std::optional<T>& held) { held = new_val; });
}

template <value::LyraValue T>
auto Var<T>::DriveTakeoverBytes(
    std::int64_t level, std::int64_t generation, const void* bytes) -> bool
  requires value::TakesBytesInPlace<T>
{
  return RecordTakeover(level, generation, [&](std::optional<T>& held) {
    if (held.has_value()) {
      held->TakeBytes(bytes);
    } else {
      held.emplace(this->Get().Holding(bytes));
    }
  });
}

template <value::LyraValue T>
template <class Record>
auto Var<T>::RecordTakeover(
    std::int64_t level, std::int64_t generation, Record record) -> bool {
  const CellRareState<T>* rare = ExistingRareState();
  Takeovers<T>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  if (takeovers == nullptr) {
    return false;
  }
  const bool was_covered = takeovers->Highest() != nullptr;
  if (!takeovers->Drive(
          TakeoverLevelOf(level), TakeoverGenerationOf(generation), record)) {
    return false;
  }
  // The first level to cover a cell something drives continuously finds in it
  // what that driver last produced, which is what the cell shows again once
  // nothing covers it (LRM 10.6.2).
  if (!was_covered && current_runtime().DrivenContinuously(*this)) {
    takeovers->KeepBeneath(this->Get());
  }
  // A level that just recorded always leaves something showing, and it is this
  // value only where no higher level covers it. Storing whatever shows needs
  // no question asked: where a higher level covers this one, what shows has
  // not moved, and a store that changes nothing publishes nothing.
  Store(*takeovers->Highest());
  return true;
}

template <value::LyraValue T>
void Var<T>::EndTakeover(std::int64_t level) {
  // Ending a level nothing occupies is what a `release` on an untaken variable
  // does, and the language gives it no effect (LRM 10.6.2).
  const CellRareState<T>* rare = ExistingRareState();
  Takeovers<T>* takeovers =
      rare == nullptr ? nullptr : rare->ExistingTakeovers();
  if (takeovers == nullptr) {
    return;
  }
  takeovers->End(TakeoverLevelOf(level));
  // Where a level is still in effect underneath, the cell takes what that
  // level already holds. Where none is, the cell keeps what it has, which is
  // the value the ended takeover last gave it.
  if (const T* showing = takeovers->Highest(); showing != nullptr) {
    Store(*showing);
    return;
  }
  if (const std::optional<T> beneath = takeovers->TakeBeneath()) {
    Store(*beneath);
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

  // A write the sink turns away (LRM 10.6) still has to reach a part to land
  // on, and it has no one to tell. It lands where the sink keeps such a write,
  // or in a copy of the sink's storage that this write keeps and nobody reads.
  explicit WriteBracket(Sink sink) : sink_(sink) {
    if (sink_.AdmitsWrite()) {
      storage_ = &sink_.MutationStorage();
      watched_ = sink_.Watched();
    } else if (ValueType* kept = sink_.DisplacedStorage()) {
      storage_ = kept;
    } else {
      storage_ = &discarded_.emplace(sink_.MutationStorage());
    }
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

  // A slice write moved at least one element.
  void Moved() {
    if (Open()) {
      outcome_ = Outcome::kChanged;
    }
  }

  // The part landed on is not what it was, and `change` says which of its bits
  // moved. A write into a bit-addressed value lands on the whole or on some of
  // its bits, and either way `change` names bits of the whole by their
  // position; the waits on any other value are not bit-addressed.
  void Landed(const Change& change) {
    if (!Open()) {
      return;
    }
    outcome_ = Outcome::kSettled;
    if constexpr (BitAddressed<ValueType>) {
      sink_.PublishTransition(change);
    } else {
      sink_.PublishTransition(Change::Whole());
    }
  }

  // The end of the write, with the full-expression that opened it.
  void End() {
    switch (outcome_) {
      case Outcome::kUndecided:
      case Outcome::kSettled:
        return;
      case Outcome::kChanged:
        if (watched_) {
          sink_.PublishTransition(Change::Whole());
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
  Outcome outcome_ = Outcome::kUndecided;
  std::optional<ValueType> discarded_;
};

template <class Sink, class Slice>
class DesignatedSlice;

template <class Sink, class Bits>
class DesignatedBits;

// A place designated within a write: the whole of what the write was opened
// on, or a part of it reached by the steps below. It borrows the write. The
// steps reach the parts that are storage of their own (LRM 13.5.2) -- an
// element, reported to the write as it is formed, a component, and a slice of
// elements -- each answering with the part designated within the same write.
// Dereferencing it is where the write lands, which is when the value from
// before the write is kept.
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
  auto ElementRef(const Key& key) {
    value::Formation formed{};
    auto& element = part_->ElementRef(key, formed);
    write_->Formed(formed);
    return Designation<Sink, std::remove_reference_t<decltype(element)>>{
        *write_, element};
  }

  template <std::size_t I>
  auto ComponentRef() {
    auto& component = part_->template ComponentRef<I>();
    return Designation<Sink, std::remove_reference_t<decltype(component)>>{
        *write_, component};
  }

  // The bits of a value of `Bits` at `position` within the part (LRM 11.5.1).
  // They lie in the part's own words, so they are a place the write lands on.
  template <typename Bits, typename Position>
  auto SliceRef(const Position& position) {
    auto bits = part_->template SliceRef<Bits>(position);
    return DesignatedBits<Sink, decltype(bits)>{*write_, std::move(bits)};
  }

  // `count` elements of the part from `start` (LRM 7.4.6), which are several
  // places rather than one.
  template <typename Position>
  auto SliceRef(const Position& start, std::int64_t count) {
    auto slice = part_->SliceRef(start, count);
    return DesignatedSlice<Sink, decltype(slice)>{*write_, std::move(slice)};
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

// A slice of elements designated within a write (LRM 7.6). It is several
// elements rather than one place, so what the write learns comes from the
// assignment into it, which compares each element before writing it.
template <class Sink, class Slice>
class DesignatedSlice {
 public:
  DesignatedSlice(WriteBracket<Sink>& write, Slice slice)
      : write_(&write), slice_(std::move(slice)) {
  }

  DesignatedSlice(const DesignatedSlice&) = delete;
  auto operator=(const DesignatedSlice&) -> DesignatedSlice& = delete;
  DesignatedSlice(DesignatedSlice&&) = delete;
  auto operator=(DesignatedSlice&&) -> DesignatedSlice& = delete;
  ~DesignatedSlice() = default;

  auto operator*() -> DesignatedSlice& {
    return *this;
  }

  template <typename Value>
  auto operator=(const Value& value) -> DesignatedSlice& {
    if (slice_.Assign(value)) {
      write_->Moved();
    }
    return *this;
  }

 private:
  WriteBracket<Sink>* write_;
  Slice slice_;
};

// Bits of a bit-addressed value designated within a write (LRM 11.5.1), named
// by `Bits`, the designation of them the value hands out. They lie in the
// value's own words, so the write lands on them where they are, and each
// assignment through them keeps what they held, writes them, and tells the
// write which bits it reached.
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

  auto operator*() -> DesignatedBits& {
    return *this;
  }
  auto operator->() -> DesignatedBits* {
    return this;
  }

  template <class Value>
  auto operator=(const Value& value) -> DesignatedBits& {
    if (const std::optional<Change> change =
            WriteBits(bits_, value, write_->Undecided())) {
      write_->Landed(*change);
    }
    return *this;
  }

  // The bits of a value of `Inner` within these, written within the same
  // write.
  template <class Inner, class Position>
  [[nodiscard]] auto SliceRef(const Position& position) const {
    auto inner = bits_.template SliceRef<Inner>(position);
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
auto Var<T>::Mutate() -> ScopedMutation<Ref<T>> {
  return ScopedMutation<Ref<T>>{Ref<T>{*this}};
}

template <value::LyraValue T>
auto Ref<T>::Mutate() const -> ScopedMutation<Ref<T>> {
  return ScopedMutation<Ref<T>>{*this};
}

static_assert(MutationSink<Ref<value::Logic>>);

}  // namespace lyra::runtime
