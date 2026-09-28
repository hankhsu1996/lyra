#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

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
// `Watched` is whether anything reads the answer. A variable cell with nothing
// waiting on it has no one to tell (LRM 4.3), so the write keeps nothing from
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
concept MutationSink = requires(S sink, const ProjectionUnchanged& unchanged) {
  typename S::ValueType;
  { sink.MutationStorage() } -> std::same_as<typename S::ValueType&>;
  { sink.Watched() } -> std::same_as<bool>;
  sink.PublishTransition(unchanged);
};

template <class Sink>
class ScopedMutation;

template <value::LyraValue T>
class Ref;

// Reads one leaf's bits out of the values a change moved between, so a wait
// that reads only bits this change left alone is passed over. Defined in the
// library: a unit writing a packed cell reaches it, and no design shapes it.
[[nodiscard]] auto MakePackedProjectionTest(
    const value::PackedArray& old_val, const value::PackedArray& new_val)
    -> ProjectionUnchanged;

template <value::LyraValue T>
class Var : public Observable, public ValueStorageCore<T> {
 public:
  Var();
  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var();

  // The write a declaration makes. The first one installs the cell's
  // representation and contents from `prototype`, a value of the declared
  // type; a later one -- a declaration reached again, which begins a fresh
  // variable in the one storage -- overwrites at that representation. What is
  // fixed is the representation, not the number of times a declaration runs,
  // so a prototype that does not match the installed one is the lowering
  // defect and is what refuses. A store before any of this is one too, and the
  // store path is what refuses it.
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

  // Starts a procedural continuous assignment at `level`, superseding whatever
  // was driving that level, and answers with the generation its evaluation
  // carries (LRM 10.6).
  auto BeginTakeover(const value::PackedArray& level) -> value::PackedArray;

  // States what the takeover at `level` has evaluated to. It reaches the cell
  // only when no higher level covers it, and is recorded either way so that
  // ending the level above it needs nobody to recompute. Answers whether the
  // evaluation offering the value is still the one driving that level, which
  // is how an evaluation superseded by a later takeover, or ended by a
  // `deassign` or `release`, learns to stop.
  auto DriveTakeover(
      const value::PackedArray& level, const value::PackedArray& generation,
      const T& new_val) -> bool;

  // Ends the procedural continuous assignment at `level`, handing the cell to
  // the highest level still in effect. Where none is left the cell is left
  // exactly as it stands, which is what a released variable keeps (LRM
  // 10.6.2).
  void EndTakeover(const value::PackedArray& level);

  // Whether a write into the cell has anyone to tell what it did: a wait parked
  // here (LRM 4.3). Under a procedural continuous assignment a write lands
  // where nothing reads it (LRM 10.6), so it has nothing to tell either.
  [[nodiscard]] auto Watched() const -> bool {
    return this->HasWaiter() && !TakenOver();
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
    if (retained_.has_value()) {
      return;
    }
    retained_ = this->Get();
    retained_slot_ = SimTime{};
  }

  // What the cell held in the Preponed region of the current time slot -- its
  // value before anything in that slot ran (LRM 4.4.2.1, 16.5.1). Once the slot
  // has changed the cell that value is the retained one; until then the cell
  // still holds it.
  [[nodiscard]] auto SampledGet() const -> const T& {
    if (!retained_.has_value()) {
      throw InternalError(
          "Var::SampledGet: a sampled value was read from a cell nothing armed "
          "to answer for one");
    }
    if (retained_slot_ == current_runtime().Now()) {
      return *retained_;
    }
    return this->Get();
  }

  // Tells whoever waits here that a write changed the cell (LRM 4.3), passing
  // over a wait whose bits `unchanged` shows the write left alone.
  void PublishTransition(const ProjectionUnchanged& unchanged) {
    current_runtime().WakeWaitersOf(*this, unchanged);
  }

  // Where a write reaching part of the cell lands: the cell's own storage, or,
  // while a procedural continuous assignment is in effect (LRM 10.6), storage
  // nothing reads -- the partial write is overridden exactly as a whole-value
  // write through `Set` is. Opening one is the first write of the slot at the
  // latest, so the slot's sampled value is kept here.
  [[nodiscard]] auto PartialWriteTarget() -> T& {
    KeepPreponed();
    if (TakenOver()) {
      return takeovers_->Discarded(this->Get());
    }
    return this->Storage();
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
  // of the old value, except the old bits a packed value's waits are passed
  // over by.
  void Store(const T& new_val) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!this->IsInstalled()) {
        throw InternalError(
            "Var<PackedArray>: store into a cell that was never initialized");
      }
    }
    KeepPreponed();
    if (!this->HasWaiter()) {
      this->Overwrite(new_val);
      return;
    }
    if (this->Get().IsBitIdentical(new_val)) {
      return;
    }
    if constexpr (std::same_as<T, value::PackedArray>) {
      const T before = this->Get();
      this->Overwrite(new_val);
      PublishTransition(MakePackedProjectionTest(before, this->Get()));
    } else {
      this->Overwrite(new_val);
      PublishTransition(MakeWholeValueProjectionTest());
    }
  }

  // Keeps what the cell held in the Preponed region of the current slot, the
  // first time a write reaches the cell in that slot (LRM 4.4.2.1, 16.5.1).
  // Nothing in the slot has changed the cell before its first write, so what
  // it holds then is that value, whether or not the write goes on to change
  // it.
  void KeepPreponed() {
    if (!retained_.has_value()) {
      return;
    }
    const SimTime now = current_runtime().Now();
    if (retained_slot_ == now) {
      return;
    }
    retained_ = this->Get();
    retained_slot_ = now;
  }

  // Whether a procedural continuous assignment shows through the cell (LRM
  // 10.6).
  [[nodiscard]] auto TakenOver() const -> bool {
    return takeovers_ != nullptr && takeovers_->Highest() != nullptr;
  }

  // The sampled value (LRM 16.5.1) and the time slot it belongs to. Engaged
  // exactly while the cell is armed to answer for one, so a cell nothing
  // samples carries neither the storage nor the work of maintaining it.
  std::optional<T> retained_;
  SimTime retained_slot_ = SimTime{};

  // The procedural continuous assignments this cell has been put under (LRM
  // 10.6), which almost no cell in a design ever is. It appears the first time
  // one starts, so a cell nobody takes over carries one pointer, answers every
  // read from its own storage, and pays one null test on a write.
  std::unique_ptr<Takeovers<T>> takeovers_;
};

// A reference to a variable cell. Transparently views one of two backings: an
// observable `Var<T>`, where a write goes through the cell so the update event
// fires and subscribers wake, or a plain `T` cell, where it is a raw write and
// nothing observes it. Copyable, so a ref formal can be forwarded as a ref
// argument to a nested call.
template <value::LyraValue T>
class Ref {
 public:
  using ValueType = T;

  // A null view, default-constructed as a member and bound before first use:
  // a `ref` port's child-side member is declared with the child and filled by
  // the parent during elaboration (LRM 23.3.3.2), before simulation reads it.
  Ref() = default;
  explicit Ref(Var<T>& cell) : signal_(&cell) {
  }
  explicit Ref(T& cell) : plain_(&cell) {
  }

  [[nodiscard]] auto Get() const -> const T& {
    if (signal_ != nullptr) {
      return signal_->Get();
    }
    return *plain_;
  }

  // Const: a `Ref` is a view, so `Set` writes the referenced cell, not the
  // handle's own pointers -- as `*p = v` is allowed through a `T* const p`.
  void Set(const T& new_val) const {
    if (signal_ != nullptr) {
      signal_->Set(new_val);
    } else {
      *plain_ = new_val;
    }
  }

  // Opens a write into the referenced storage, as an observable cell itself
  // does; only an observable backing has anyone to tell what the write did.
  [[nodiscard]] auto Mutate() const -> ScopedMutation<Ref<T>>;

  // The `MutationSink` surface. A plain backing has no observation at all, so
  // nothing reads what a write did to it; that is the same answer an
  // observable backing gives while nothing waits on it, reached by a shorter
  // route.
  [[nodiscard]] auto MutationStorage() const -> T& {
    if (signal_ != nullptr) {
      return signal_->PartialWriteTarget();
    }
    return *plain_;
  }
  [[nodiscard]] auto Watched() const -> bool {
    return signal_ != nullptr && signal_->Watched();
  }

  // A reference denotes the storage it binds (LRM 23.3.3.2), so the operations
  // on a cell answer through it. Only an observable cell keeps the value a time
  // slot moved away from, so a plain backing has none to answer with -- and
  // answering with its current value would be a different value whenever the
  // slot has already written it.
  void ArmSampling() const {
    if (signal_ != nullptr) {
      signal_->ArmSampling();
    }
  }
  [[nodiscard]] auto SampledGet() const -> const T& {
    if (signal_ == nullptr) {
      throw SimulationError(
          "a sampled value of storage lent by reference is only available "
          "where that storage is an observable cell");
    }
    return signal_->SampledGet();
  }

  // Opening the reference: the cell it binds (LRM 23.3.3.2). What a wait
  // registers on, and what an operation on the cell acts through, is that cell
  // and never the reference standing for it. A plain backing is storage no cell
  // stands for, so there is nothing to open.
  [[nodiscard]] auto operator*() const -> Var<T>& {
    if (signal_ == nullptr) {
      throw SimulationError(
          "storage lent by reference can only be reached as a cell where that "
          "storage is an observable cell");
    }
    return *signal_;
  }

  void PublishTransition(const ProjectionUnchanged& unchanged) const {
    if (signal_ != nullptr) {
      signal_->PublishTransition(unchanged);
    }
  }

 private:
  Var<T>* signal_ = nullptr;
  T* plain_ = nullptr;
};

// Makes `frame` runnable again when what happens at one of `triggers` is an
// event for the wait (LRM 9.4.2 / 9.4.2.2 / 9.4.3 / 15.5.2). Each subscription
// registers on the frame's own wait-registration set, so waking or destroying
// the frame revokes every leaf and the one that wakes it drops the siblings;
// the engine has no idea what kind of wait this is. Each leaf is copied into
// the target's waiter record, so `triggers` is only read for the duration of
// this call.
//
// An empty leaf set is legal and means "never wake up" -- an `always_comb`
// whose body reads nothing (`always_comb c = 7;`) runs once, then suspends
// forever.
void SubscribeToLeaves(
    CoroutineHandle frame, std::span<const Trigger> triggers);

// Which frame is waiting is the runtime's own to know, so nothing here is
// handed one: both backends reach this the same way and answer their caller
// with the same bool. The wait keeps its own copy of each leaf, so the leaves
// are only read here -- laid out in one array, or each where its builder left
// it and named by address.
auto WaitAny(RuntimeEffects& services, std::span<const Trigger> triggers)
    -> bool;
auto WaitAny(RuntimeEffects& services, std::span<const Trigger* const> triggers)
    -> bool;

auto WaitUntil(RuntimeEffects& services, std::span<const Trigger> triggers)
    -> bool;
auto WaitUntil(
    RuntimeEffects& services, std::span<const Trigger* const> triggers) -> bool;

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
    return;
  }
  Store(new_val);
}

template <value::LyraValue T>
auto Var<T>::BeginTakeover(const value::PackedArray& level)
    -> value::PackedArray {
  if (takeovers_ == nullptr) {
    takeovers_ = std::make_unique<Takeovers<T>>();
  }
  return TakeoverGenerationValue(takeovers_->Begin(TakeoverLevelOf(level)));
}

template <value::LyraValue T>
auto Var<T>::DriveTakeover(
    const value::PackedArray& level, const value::PackedArray& generation,
    const T& new_val) -> bool {
  if (takeovers_ == nullptr ||
      !takeovers_->Drive(
          TakeoverLevelOf(level), TakeoverGenerationOf(generation), new_val)) {
    return false;
  }
  // A level that just recorded always leaves something showing, and it is this
  // value only where no higher level covers it. Storing whatever shows needs
  // no question asked: where a higher level covers this one, what shows has
  // not moved, and a store that changes nothing publishes nothing.
  Store(*takeovers_->Highest());
  return true;
}

template <value::LyraValue T>
void Var<T>::EndTakeover(const value::PackedArray& level) {
  // Ending a level nothing occupies is what a `release` on an untaken variable
  // does, and the language gives it no effect (LRM 10.6.2).
  if (takeovers_ == nullptr) {
    return;
  }
  takeovers_->End(TakeoverLevelOf(level));
  // Where a level is still in effect underneath, the cell takes what that
  // level already holds. Where none is, the cell keeps what it has, which is
  // the value the ended takeover last gave it.
  if (const T* showing = takeovers_->Highest(); showing != nullptr) {
    Store(*showing);
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

  explicit WriteBracket(Sink sink)
      : sink_(sink),
        storage_(&sink_.MutationStorage()),
        watched_(sink_.Watched()) {
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

  // The part landed on is not what it was, and `unchanged` says which of its
  // bits moved. A packed value has no parts that are storage of their own, so
  // a write into one lands on the whole and the bits it moved are the whole's;
  // the waits on any other value are not bit-addressed.
  void Landed(const ProjectionUnchanged& unchanged) {
    if (!Open()) {
      return;
    }
    outcome_ = Outcome::kSettled;
    if constexpr (std::same_as<ValueType, value::PackedArray>) {
      sink_.PublishTransition(unchanged);
    } else {
      sink_.PublishTransition(MakeWholeValueProjectionTest());
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
          sink_.PublishTransition(MakeWholeValueProjectionTest());
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
  ValueType* storage_;
  bool watched_;
  Outcome outcome_ = Outcome::kUndecided;
};

// Which bits of a part landed on a change between the two values left alone:
// a packed value's by position, and nothing that can be shown of any other.
template <class Part>
auto LandedChange(const Part& before, const Part& after)
    -> ProjectionUnchanged {
  if constexpr (std::same_as<Part, value::PackedArray>) {
    return MakePackedProjectionTest(before, after);
  } else {
    return MakeWholeValueProjectionTest();
  }
}

// A packed part is what no design shapes, so it is compiled once, in the
// library.
extern template auto LandedChange<value::PackedArray>(
    const value::PackedArray& before, const value::PackedArray& after)
    -> ProjectionUnchanged;

template <class Sink, class Slice>
class DesignatedSlice;

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
    if (before_.has_value() && !before_->IsBitIdentical(*part_)) {
      write_->Landed(LandedChange(*before_, *part_));
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

  template <typename... Bounds>
  auto SliceRef(const Bounds&... bounds) {
    auto slice = part_->SliceRef(bounds...);
    return DesignatedSlice<Sink, decltype(slice)>{*write_, std::move(slice)};
  }

  auto operator*() -> Part& {
    if (write_->Undecided()) {
      before_.emplace(*part_);
    }
    return *part_;
  }

 private:
  WriteBracket<Sink>* write_;
  Part* part_;
  std::optional<Part> before_;
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

static_assert(MutationSink<Ref<value::PackedArray>>);

}  // namespace lyra::runtime
