#pragma once

#include <algorithm>
#include <concepts>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/pending_wait.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_services.hpp"
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
      return transition == EdgeTransition::kPosedge ||
             transition == EdgeTransition::kNegedge;
  }
  return false;
}

class Observable {
 public:
  Observable() = default;
  Observable(const Observable&) = delete;
  auto operator=(const Observable&) -> Observable& = delete;
  Observable(Observable&&) = delete;
  auto operator=(Observable&&) -> Observable& = delete;
  ~Observable() = default;

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

template <value::LyraValue T>
class ScopedMutation;

template <value::LyraValue T>
class Var : public Observable, public ValueStorageCore<T> {
 public:
  Var() = default;
  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var() = default;

  // `Get` is inherited from `ValueStorageCore`.

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
  // event), wakes subscribers through the engine. Defined out of line below so
  // it can reach the PackedArray edge classifier.
  void Set(RuntimeServices& services, const T& new_val);

  // RAII entry to partial-write context. Construct via `var.Mutate(svc)`
  // at the start of a chain; the returned handle snapshots the current
  // value, forwards chain methods so a partial write expressed as a single
  // selector chain (e.g. ending in a `SliceRef = v`) lands in the snapshot,
  // and commits the mutated snapshot through `Var::Set` in its destructor
  // (fires subscribers on change). Lifetime is C++ standard full-expression
  // temporary lifetime -- the handle is non-copyable and non-movable, so
  // storing it past the statement is rejected at compile time.
  auto Mutate(RuntimeServices& services) -> ScopedMutation<T>;
};

// A reference to a variable cell. Transparently views one of two backings:
// an observable `Var<T>` (writes route through `Var::Set` so the update
// event fires and subscribers wake), or a plain `T` cell (raw read / write,
// no observers). Copyable, so a ref formal can be forwarded as a ref
// argument to a nested call.
template <value::LyraValue T>
class Ref {
 public:
  // A null view, default-constructed as a member and bound before first use:
  // a `ref` port's child-side member is declared with the child and filled by
  // the parent during elaboration (LRM 23.3.3.2), before simulation reads it.
  Ref() = default;
  explicit Ref(Var<T>& cell) : signal_(&cell) {
  }
  explicit Ref(T& cell) : plain_(&cell) {
  }

  [[nodiscard]] auto Get() const -> T {
    if (signal_ != nullptr) {
      return signal_->Get();
    }
    return *plain_;
  }

  // Const: a `Ref` is a view, so `Set` writes the referenced cell, not the
  // handle's own pointers -- as `*p = v` is allowed through a `T* const p`.
  void Set(RuntimeServices& services, const T& new_val) const {
    if (signal_ != nullptr) {
      signal_->Set(services, new_val);
    } else {
      *plain_ = new_val;
    }
  }

  // RAII entry to partial-write context, mirroring `Var<T>::Mutate`: the
  // returned handle snapshots the referenced cell, forwards chain methods so a
  // selector chain lands in the snapshot, and writes the snapshot back through
  // `Ref::Set` in its destructor (firing subscribers when the backing is
  // observable).
  [[nodiscard]] auto Mutate(RuntimeServices& services) const
      -> ScopedMutation<T>;

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
    token->process->BlockLeaf(token, this);
  }

  static void await_resume() noexcept {
  }

  // An edge / value-change is not a level: a change during suspension is missed
  // (LRM 9.7 resensitize), so resume re-subscribes and waits for the next one.
  // Re-subscribing needs no engine services, but the capability signature
  // carries them uniformly.
  // NOLINTNEXTLINE(readability-named-parameter)
  auto Reestablish(RuntimeServices&, CoroutineHandle activation)
      -> PendingWaitOutcome override {
    SubscribeValueChange(activation, triggers_);
    return PendingWaitOutcome::kReblocked;
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
    RuntimeServices&,  // NOLINT(readability-named-parameter)
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

template <value::LyraValue T>
void Var<T>::Set(RuntimeServices& services, const T& new_val) {
  // A PackedArray write classifies the LSB transition per waiter (LRM 9.4.2
  // Table 9-2); every other cell type only has any-change waiters. The
  // representation match and change detection live in `ValueStorageCore`; the
  // signal cell adds only the subscriber wakeup on a real change.
  if constexpr (std::same_as<T, value::PackedArray>) {
    if (!this->IsInstalled()) {
      throw InternalError(
          "Var<PackedArray>::Set: store into a cell that was never "
          "initialized");
    }
    const value::PackedArray old_val = this->Get();
    if (this->Overwrite(new_val)) {
      services.TriggerValueChange(
          *this, MakePackedArrayEdgeClassifier(old_val, new_val));
    }
  } else {
    if (this->Overwrite(new_val)) {
      services.TriggerValueChange(
          *this,
          [](std::uint64_t, std::uint64_t, support::EventEdge edge) -> bool {
            return edge == support::EventEdge::kAnyChange;
          });
    }
  }
}

// RAII handle that owns a snapshot of the referenced cell for the lifetime of
// one partial-write expression. The destructor writes the (possibly mutated)
// snapshot back through `Ref<T>::Set` -- subscribers fire on change when the
// backing is observable, a raw store when it is plain. Non-copyable and
// non-movable: the contract is that it lives only until the end of the
// constructing full expression. Returning it by value from `Var<T>::Mutate` /
// `Ref<T>::Mutate` relies on C++17 mandatory copy elision (prvalues are
// materialized in the caller's storage with no copy/move).
//
// `operator*` is the single access point -- all chain methods, operators,
// and selectors are reached through the deref'd T directly.
template <value::LyraValue T>
class ScopedMutation {
 public:
  ScopedMutation(RuntimeServices& services, Ref<T> ref)
      : services_(&services), ref_(ref), snapshot_(ref.Get()) {
  }

  ScopedMutation(const ScopedMutation&) = delete;
  auto operator=(const ScopedMutation&) -> ScopedMutation& = delete;
  ScopedMutation(ScopedMutation&&) = delete;
  auto operator=(ScopedMutation&&) -> ScopedMutation& = delete;

  ~ScopedMutation() {
    ref_.Set(*services_, std::move(snapshot_));
  }

  auto operator*() -> T& {
    return snapshot_;
  }

 private:
  RuntimeServices* services_;
  Ref<T> ref_;
  T snapshot_;
};

template <value::LyraValue T>
auto Var<T>::Mutate(RuntimeServices& services) -> ScopedMutation<T> {
  return ScopedMutation<T>{services, Ref<T>{*this}};
}

template <value::LyraValue T>
auto Ref<T>::Mutate(RuntimeServices& services) const -> ScopedMutation<T> {
  return ScopedMutation<T>{services, *this};
}

}  // namespace lyra::runtime
