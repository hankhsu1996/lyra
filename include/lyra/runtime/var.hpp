#pragma once

#include <algorithm>
#include <concepts>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/value_storage_core.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// Classification of a single value change by its LSB transition per LRM
// 9.4.2 Table 9-2:
//   - posedge: 0 -> {1, x, z}; {x, z} -> 1
//   - negedge: 1 -> {0, x, z}; {x, z} -> 0
//   - kChangeOnly: any other LSB-different change (x <-> z), or the full
//     value moved but the LSB stayed the same (upper bits changing).
enum class EdgeTransition : std::uint8_t {
  kChangeOnly,
  kPosedge,
  kNegedge,
};

inline auto ClassifyEdge(
    value::FourStateBit old_lsb, value::FourStateBit new_lsb)
    -> EdgeTransition {
  if (old_lsb == new_lsb) {
    return EdgeTransition::kChangeOnly;
  }
  // Leaving 0 is posedge regardless of destination (1, x, or z).
  if (old_lsb == value::FourStateBit::kZero) {
    return EdgeTransition::kPosedge;
  }
  // Leaving 1 is negedge regardless of destination (0, x, or z).
  if (old_lsb == value::FourStateBit::kOne) {
    return EdgeTransition::kNegedge;
  }
  // Leaving x or z: only arrival at 0 or 1 counts; x <-> z is kChangeOnly.
  if (new_lsb == value::FourStateBit::kOne) {
    return EdgeTransition::kPosedge;
  }
  if (new_lsb == value::FourStateBit::kZero) {
    return EdgeTransition::kNegedge;
  }
  return EdgeTransition::kChangeOnly;
}

// Whether the transition names a direction, as opposed to a change that names
// none (LRM 9.4.2 Table 9-2).
[[nodiscard]] constexpr auto IsDirectedEdge(EdgeTransition transition) -> bool {
  switch (transition) {
    case EdgeTransition::kPosedge:
    case EdgeTransition::kNegedge:
      return true;
    case EdgeTransition::kChangeOnly:
      return false;
  }
  throw InternalError("runtime::IsDirectedEdge: unknown EdgeTransition");
}

inline auto EdgeMatches(
    support::EventEdge subscribed, EdgeTransition transition) -> bool {
  switch (subscribed) {
    case support::EventEdge::kAnyChange:
      return true;
    case support::EventEdge::kPosedge:
      return transition == EdgeTransition::kPosedge;
    case support::EventEdge::kNegedge:
      return transition == EdgeTransition::kNegedge;
    case support::EventEdge::kBothEdges:
      return IsDirectedEdge(transition);
  }
  throw InternalError("runtime::EdgeMatches: unknown EventEdge");
}

class Observable {
 public:
  Observable() = default;
  Observable(const Observable&) = delete;
  auto operator=(const Observable&) -> Observable& = delete;
  Observable(Observable&&) = delete;
  auto operator=(Observable&&) -> Observable& = delete;
  ~Observable() = default;

  // Whether anything is currently armed to observe a change here. LRM 4.3
  // makes an update event matter to what is "considered for evaluation", and
  // the armed observations are the whole of that: nothing else reads a change.
  // So a write to a cell with none has nothing to report, and may skip the
  // work of describing itself.
  [[nodiscard]] auto HasArmedObservation() const noexcept -> bool {
    return !waiters_.Empty();
  }

  void Subscribe(
      CoroutineHandle handle, support::EventEdge edge,
      std::uint64_t lsb_bit_offset, std::uint64_t bit_width) {
    Registration& reg = handle->Park(waiters_);
    reg.edge = edge;
    reg.lsb_bit_offset = lsb_bit_offset;
    reg.bit_width = bit_width;
  }

  // Claims and returns the activations whose fire condition this change
  // satisfies; the rest stay parked. The classifier reads each membership's
  // projection and edge, and decides from the old / new value the caller
  // captured.
  [[nodiscard]] auto TakeMatchingWaiters(const EdgeClassifier& classify)
      -> std::vector<CoroutineHandle> {
    std::vector<CoroutineHandle> woken;
    waiters_.ForEach([&](Registration& reg) {
      if (classify(reg.lsb_bit_offset, reg.bit_width, reg.edge)) {
        reg.Unlink();
        woken.push_back(reg.activation);
      }
    });
    return woken;
  }

 private:
  RegistrationList waiters_;
};

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
  // event), wakes subscribers through the engine. The engine is the ambient
  // one: it has the standing of a stack pointer, so a store does not carry it.
  // Defined out of line below so it can reach the PackedArray edge classifier.
  void Set(const T& new_val);

  // The before-image a transition is computed against, held only where an
  // armed observation will read the answer. With nothing armed there is no
  // question to answer, which is what lets an unobserved cell take a plain
  // store.
  [[nodiscard]] auto CaptureTransitionBase() const -> std::optional<T> {
    if (!this->HasArmedObservation()) {
      return std::nullopt;
    }
    return this->Get();
  }

  // Reports what the write between the capture and here did to the cell,
  // waking whichever armed observations the transition satisfies. Defined out
  // of line below so it can reach the PackedArray edge classifier.
  void PublishTransition(const std::optional<T>& before);

  // RAII entry to partial-write context. Construct via `var.Mutate()` at the
  // start of a chain; the returned handle names the cell's own storage, so a
  // partial write expressed as a single selector chain (e.g. ending in a
  // `SliceRef = v`) lands in the cell as it is written, and reports the
  // transition once in its destructor. Lifetime is C++ standard
  // full-expression temporary lifetime -- the handle is non-copyable and
  // non-movable, so storing it past the statement is rejected at compile time.
  auto Mutate() -> ScopedMutation<Ref<T>>;
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
  void PublishTransition(const std::optional<T>& before) const {
    if (signal_ != nullptr) {
      signal_->PublishTransition(before);
    }
  }

 private:
  Var<T>* signal_ = nullptr;
  T* plain_ = nullptr;
};

// Makes `frame` runnable again when any leaf of `triggers` changes as its edge
// demands (LRM 9.4.2 / 9.4.2.2 / 9.4.3). Each subscription registers on the
// frame's own wait-registration set, so waking or destroying the frame revokes
// every leaf and the one that wakes it drops the siblings; the engine has no
// idea what kind of wait this is. Each leaf's projection is copied into the
// cell's subscriber record, so `triggers` is only read for the duration of this
// call.
//
// An empty trigger set is legal and means "never wake up" -- an `always_comb`
// whose body reads nothing (`always_comb c = 7;`) runs once, then suspends
// forever.
inline void SubscribeValueChange(
    CoroutineHandle frame, std::span<const Trigger> triggers) {
  for (const Trigger& trigger : triggers) {
    if (trigger.observable == nullptr) {
      throw InternalError(
          "SubscribeValueChange: a trigger names no observable cell");
    }
    trigger.observable->Subscribe(
        frame, trigger.edge, trigger.lsb_bit_offset, trigger.bit_width);
  }
}

// Suspends the calling frame on a value-change wait. The registration happens
// in `await_suspend`, where the frame that must be resumed is in hand: a wait
// inside an enabled task has to resume the task's frame, not the enabling
// process's, and only the language knows which frame is awaiting.
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
    SubscribeValueChange(token, triggers_);
    BlockOn(token);
  }

  void await_resume() const {
    CheckAbortOnResume();
  }

  // An edge / value-change is not a level: a change during suspension is missed
  // (LRM 9.7 resensitize), so resume re-subscribes and waits for the next one.
  // Re-subscribing needs no runtime access, but the capability signature
  // carries them uniformly.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeEffects&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    SubscribeValueChange(activation, triggers_);
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

// Builds the per-leaf classifier that the Observable invokes per waiter.
// For any-change waiters: compares the projected slice in `old` and `new` (or
// fires unconditionally when the waiter is whole-var, `bit_width == 0`).
// For edge waiters: classifies the transition at `lsb_bit_offset` (LRM Table
// 9-2 via `ClassifyEdge`), then matches against the subscribed edge.
inline auto MakePackedArrayEdgeClassifier(
    const value::PackedArray& old_val, const value::PackedArray& new_val)
    -> EdgeClassifier {
  return [&old_val, &new_val](
             std::uint64_t lsb, std::uint64_t width,
             support::EventEdge edge) -> bool {
    if (edge == support::EventEdge::kAnyChange) {
      if (width == 0U) {
        return true;
      }
      const auto lsb_arg = value::PackedArray::FromInt(
          static_cast<std::int64_t>(lsb), 64U, false, false);
      const auto old_slice =
          old_val.ExtractBits(lsb_arg, static_cast<std::uint32_t>(width));
      const auto new_slice =
          new_val.ExtractBits(lsb_arg, static_cast<std::uint32_t>(width));
      return !old_slice.IsBitIdentical(new_slice);
    }
    const value::FourStateBit old_bit =
        (width == 0U) ? old_val.Lsb() : old_val.GetBit(lsb);
    const value::FourStateBit new_bit =
        (width == 0U) ? new_val.Lsb() : new_val.GetBit(lsb);
    return EdgeMatches(edge, ClassifyEdge(old_bit, new_bit));
  };
}

// The classifier for a value with no bit projection to speak of: it changed,
// so every any-change waiter fires and no edge waiter can, there being no LSB
// for LRM 9.4.2 Table 9-2 to read.
inline auto MakeAnyChangeClassifier() -> EdgeClassifier {
  return [](std::uint64_t, std::uint64_t, support::EventEdge edge) -> bool {
    return edge == support::EventEdge::kAnyChange;
  };
}

template <value::LyraValue T>
void Var<T>::PublishTransition(const std::optional<T>& before) {
  if (!before || before->IsBitIdentical(this->Get())) {
    return;
  }
  if constexpr (std::same_as<T, value::PackedArray>) {
    current_runtime().TriggerValueChange(
        *this, MakePackedArrayEdgeClassifier(*before, this->Get()));
  } else {
    current_runtime().TriggerValueChange(*this, MakeAnyChangeClassifier());
  }
}

template <value::LyraValue T>
void Var<T>::Set(const T& new_val) {
  // The whole-value case of the same bracket a partial write uses: state what
  // the transition is computed against, write, report. What it adds over a
  // partial write is the representation match, which only a whole value can be
  // checked for -- a chain writes a part and never restates the whole.
  if constexpr (std::same_as<T, value::PackedArray>) {
    if (!this->IsInstalled()) {
      throw InternalError(
          "Var<PackedArray>::Set: store into a cell that was never "
          "initialized");
    }
  }
  const std::optional<T> before = this->CaptureTransitionBase();
  this->Overwrite(new_val);
  this->PublishTransition(before);
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
