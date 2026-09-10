#pragma once

#include <concepts>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/observable.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/takeover.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// What a partial-write chain writes through (LRM 11.5.1). The chain reaches
// the owner's storage and lands its part there directly; the owner is told
// once, when the chain is over, and works out then what to publish. Two things
// that takes, named for the role rather than for either owner's vocabulary.
//
// `MutationStorage` is the storage itself, so a write reaches the part it
// names and disturbs nothing else. Two things follow: a chain cannot lose a
// write performed through another chain while it was open, and what writing
// one element costs does not scale with the size of the whole value.
//
// `TransitionBase` is whatever the owner must hold from before the write to
// decide afterwards what the write meant, captured at the start and handed
// back at the end. What that is differs by owner. A variable cell holds a
// before-image of its contents, and holds one only while something is armed to
// read the answer. A net driver holds its own contribution, because the
// transition that matters there is the resolved value's and a chain leaving
// the contribution bit-identical leaves the resolution over it unchanged
// (LRM 6.5).
//
// Each sink claims the contract with a `static_assert(MutationSink<...>)`
// beside its own definition, the way a value type claims its `lyra::value`
// concepts; `ScopedMutation` itself takes the sink unconstrained, because a
// sink names the handle in its own partial-write entry's return type and
// checking the constraint there would depend on the sink being complete.
template <class S>
concept MutationSink =
    requires(S sink, const typename S::TransitionBase& base) {
      typename S::ValueType;
      { sink.MutationStorage() } -> std::same_as<typename S::ValueType&>;
      {
        sink.CaptureTransitionBase()
      } -> std::same_as<typename S::TransitionBase>;
      sink.PublishTransition(base);
    };

template <class Sink>
class ScopedMutation;

template <value::LyraValue T>
class Ref;

template <value::LyraValue T>
class Var : public Observable, public ValueStorageCore<T> {
 public:
  Var() = default;
  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var() = default;

  // Installs the cell's declared representation (and default contents) exactly
  // once, at construction; `prototype` is a value of the cell's declared type,
  // only its representation is used. Installing twice, or a store before
  // installation, is a lowering defect and throws. After installation, every
  // store requires the right-hand side to already be at this representation --
  // so the cell's type is fixed by construction, not adopted from whichever
  // store runs first.
  void Initialize(T prototype) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (this->IsInstalled()) {
        throw InternalError(
            "Var<PackedArray>::Initialize: cell is already initialized");
      }
    }
    this->Install(std::move(prototype));
  }

  // Commits a whole-variable write and, on a real change (LRM 4.3 update
  // event), wakes whoever waits through the engine. The engine is the ambient
  // one: it has the standing of a stack pointer, so a store does not carry it.
  // Defined out of line below, where the per-value-family test it reports
  // through is in scope.
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

  // The before-image a transition is computed against, held only where
  // something will read the answer: a wait parked here, or a retained sampled
  // value the current time slot has not moved away from yet (LRM 16.5.1). With
  // neither there is no question to answer, which is what lets an unobserved
  // cell take a plain store.
  [[nodiscard]] auto CaptureTransitionBase() const -> std::optional<T> {
    if (!this->HasWaiter() && !retained_.has_value()) {
      return std::nullopt;
    }
    return this->Get();
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

  // Reports what the write between the capture and here did to the cell, waking
  // whoever the change is an event for. Defined out of line below, where the
  // per-value-family test it reports through is in scope.
  void PublishTransition(const std::optional<T>& before);

  // RAII entry to partial-write context. Construct via `var.Mutate()` at the
  // start of a chain; the returned handle names the cell's own storage, so a
  // partial write expressed as a single selector chain (e.g. ending in a
  // `SliceRef = v`) lands in the cell as it is written, and reports the
  // transition once in its destructor. Lifetime is C++ standard
  // full-expression temporary lifetime -- the handle is non-copyable and
  // non-movable, so storing it past the statement is rejected at compile time.
  auto Mutate() -> ScopedMutation<Ref<T>>;

 private:
  // The one path a value reaches the cell's storage by, whoever sent it: state
  // what the transition is computed against, write, report. It is the
  // whole-value case of the same bracket a partial write uses; what it adds is
  // the representation match, which only a whole value can be checked for --
  // a chain writes a part and never restates the whole.
  void Store(const T& new_val) {
    if constexpr (std::same_as<T, value::PackedArray>) {
      if (!this->IsInstalled()) {
        throw InternalError(
            "Var<PackedArray>: store into a cell that was never initialized");
      }
    }
    const std::optional<T> before = this->CaptureTransitionBase();
    this->Overwrite(new_val);
    this->PublishTransition(before);
  }

  // Keeps the value a slot is about to move away from, once per slot. The first
  // change in a slot is the one whose before-image is that slot's Preponed
  // value (LRM 4.4.2.1); every later change moves away from a value the slot
  // itself produced.
  void RetainPreponed(const T& before) {
    if (!retained_.has_value()) {
      return;
    }
    const SimTime now = current_runtime().Now();
    if (retained_slot_ == now) {
      return;
    }
    retained_ = before;
    retained_slot_ = now;
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
  using TransitionBase = std::optional<T>;

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

  // Opens a partial-write bracket, as an observable cell itself does: the
  // returned handle names the referenced cell's own storage, so a selector
  // chain lands in that cell as it is written, and reports the transition in
  // its destructor (waking observations when the backing is observable).
  [[nodiscard]] auto Mutate() const -> ScopedMutation<Ref<T>>;

  // The `MutationSink` surface. A plain backing has no observation at all, so
  // it states no before-image and reports nothing; that is the same answer an
  // observable backing gives while nothing is armed on it, reached by a
  // shorter route.
  [[nodiscard]] auto MutationStorage() const -> T& {
    if (signal_ != nullptr) {
      return signal_->Storage();
    }
    return *plain_;
  }
  [[nodiscard]] auto CaptureTransitionBase() const -> std::optional<T> {
    if (signal_ == nullptr) {
      return std::nullopt;
    }
    return signal_->CaptureTransitionBase();
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

  void PublishTransition(const std::optional<T>& before) const {
    if (signal_ != nullptr) {
      signal_->PublishTransition(before);
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
inline void SubscribeToLeaves(
    CoroutineHandle frame, std::span<const Trigger> triggers) {
  for (const Trigger& trigger : triggers) {
    if (trigger.observable == nullptr) {
      throw InternalError("SubscribeToLeaves: a leaf names nothing to wait on");
    }
    trigger.observable->Subscribe(
        frame, trigger.observation, trigger.lsb_bit_offset, trigger.bit_width);
  }
}

// Suspends the calling frame on an event control's wait. The registration
// happens in `await_suspend`, where the frame that must be resumed is in hand:
// a wait inside an enabled task has to resume the task's frame, not the
// enabling process's, and only the language knows which frame is awaiting.
class EventControlAwaitable : public PendingWait {
 public:
  explicit EventControlAwaitable(std::span<const Trigger> triggers)
      : triggers_(triggers.begin(), triggers.end()) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  template <class P>
  void await_suspend(std::coroutine_handle<P> handle) {
    CoroutineHandle token = &handle.promise();
    SubscribeToLeaves(token, triggers_);
    BlockOn(token);
  }

  void await_resume() const {
    CheckAbortOnResume();
  }

  // None of what a leaf reports is a level: what happens while the procedure is
  // not waiting here is missed, so resuming waits for the next one and compares
  // against what it finds now rather than against what it left. Re-establishing
  // needs no runtime access, but the capability signature carries them
  // uniformly.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeEffects&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    for (const Trigger& trigger : triggers_) {
      if (ArmedObservation* observation = trigger.observation.Get()) {
        observation->Arm();
      }
    }
    SubscribeToLeaves(activation, triggers_);
    return PendingWaitOutcome::kReblocked;
  }

  // The construct behind this wait is an event control, a `wait` condition, or
  // an always_comb / always_latch sensitivity list -- each of which
  // LRM 12.4.2.1 names as a violation report flush point when it resumes the
  // process.
  [[nodiscard]] auto IsReportFlushPoint() const -> bool override {
    return true;
  }

 private:
  std::vector<Trigger> triggers_;
};

// A wait's registration names the process to wake. A C++ coroutine is handed
// its own frame at the suspension, so this realization reads the frame from the
// language and never consults the engine handle the call carries; an execution
// backend, whose generated frame the engine never sees, needs that handle to
// ask the runtime which process is running.
inline auto WaitAny(
    RuntimeEffects&,  // NOLINT(readability-named-parameter)
    std::span<const Trigger> triggers) -> EventControlAwaitable {
  return EventControlAwaitable{triggers};
}

// Reads one leaf's bits out of the values a change moved between, so a wait
// that reads only bits this change left alone is passed over.
inline auto MakePackedProjectionTest(
    const value::PackedArray& old_val, const value::PackedArray& new_val)
    -> ProjectionUnchanged {
  return [&old_val, &new_val](std::uint64_t lsb, std::uint64_t width) -> bool {
    const auto lsb_arg = value::PackedArray::FromInt(
        static_cast<std::int64_t>(lsb), 64U, false, false);
    const auto old_slice =
        old_val.ExtractBits(lsb_arg, static_cast<std::uint32_t>(width));
    const auto new_slice =
        new_val.ExtractBits(lsb_arg, static_cast<std::uint32_t>(width));
    return old_slice.IsBitIdentical(new_slice);
  };
}

template <value::LyraValue T>
void Var<T>::PublishTransition(const std::optional<T>& before) {
  if (!before || before->IsBitIdentical(this->Get())) {
    return;
  }
  this->RetainPreponed(*before);
  if constexpr (std::same_as<T, value::PackedArray>) {
    current_runtime().WakeWaitersOf(
        *this, MakePackedProjectionTest(*before, this->Get()));
  } else {
    current_runtime().WakeWaitersOf(*this, MakeWholeValueProjectionTest());
  }
}

template <value::LyraValue T>
void Var<T>::Set(const T& new_val) {
  // A procedural write is discarded outright while any takeover shows through
  // this cell (LRM 10.6). A cell nobody has ever taken over holds no record at
  // all, so the ordinary write pays one null test.
  if (takeovers_ != nullptr && takeovers_->Highest() != nullptr) {
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

// RAII handle bracketing one partial-write expression: it names the sink's
// storage for the duration, and on the way out hands back what the sink stated
// beforehand so the sink can report what the write did. Non-copyable and
// non-movable: the contract is that it lives only until the end of the
// constructing full expression. Returning it by value from the entry that
// opens one relies on C++17 mandatory copy elision (prvalues are materialized
// in the caller's storage with no copy/move).
//
// `operator*` is the single access point -- all chain methods, operators, and
// selectors are reached through the deref'd storage directly. That storage is
// the owner's own, so a write is visible to everything reading the owner from
// the moment it lands (LRM 13.5.2), and a second chain open over the same
// owner cannot be overwritten by this one closing.
template <class Sink>
class ScopedMutation {
 public:
  using ValueType = typename Sink::ValueType;

  explicit ScopedMutation(Sink sink)
      : sink_(sink),
        storage_(sink_.MutationStorage()),
        before_(sink_.CaptureTransitionBase()) {
  }

  ScopedMutation(const ScopedMutation&) = delete;
  auto operator=(const ScopedMutation&) -> ScopedMutation& = delete;
  ScopedMutation(ScopedMutation&&) = delete;
  auto operator=(ScopedMutation&&) -> ScopedMutation& = delete;

  ~ScopedMutation() {
    sink_.PublishTransition(before_);
  }

  auto operator*() -> ValueType& {
    return storage_;
  }

 private:
  Sink sink_;
  ValueType& storage_;
  typename Sink::TransitionBase before_;
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
