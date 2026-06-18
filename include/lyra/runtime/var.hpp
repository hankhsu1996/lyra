#pragma once

#include <algorithm>
#include <concepts>
#include <cstdint>
#include <initializer_list>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/value/packed.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/value_concept.hpp"

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

inline auto EdgeMatches(Edge subscribed, EdgeTransition transition) -> bool {
  switch (subscribed) {
    case Edge::kAnyChange:
      return true;
    case Edge::kPosedge:
      return transition == EdgeTransition::kPosedge;
    case Edge::kNegedge:
      return transition == EdgeTransition::kNegedge;
    case Edge::kBothEdges:
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
      CoroutineHandle handle, Edge edge, std::uint64_t lsb_bit_offset,
      std::uint64_t bit_width) {
    waiters_.push_back(
        Waiter{
            .handle = handle,
            .edge = edge,
            .lsb_bit_offset = lsb_bit_offset,
            .bit_width = bit_width});
  }

  // Idempotent: a no-op if `handle` is not currently subscribed. Used to clean
  // up sibling subscriptions when a multi-trigger wait resumes on one
  // Observable.
  void Unsubscribe(CoroutineHandle handle) {
    std::erase_if(
        waiters_, [&](const Waiter& w) { return w.handle == handle; });
  }

  // Removes and returns the coroutine frames whose per-waiter classifier
  // returns true. Non-matching waiters stay subscribed. The classifier receives
  // each waiter's projection and edge and decides based on context the caller
  // (Var::Set) captured (old/new value).
  [[nodiscard]] auto TakeMatchingWaiters(const EdgeClassifier& classify)
      -> std::vector<CoroutineHandle> {
    std::vector<CoroutineHandle> out;
    auto keep_begin = std::ranges::partition(waiters_, [&](const Waiter& w) {
                        return !classify(w.lsb_bit_offset, w.bit_width, w.edge);
                      }).begin();
    out.reserve(static_cast<std::size_t>(waiters_.end() - keep_begin));
    for (auto it = keep_begin; it != waiters_.end(); ++it) {
      out.push_back(it->handle);
    }
    waiters_.erase(keep_begin, waiters_.end());
    return out;
  }

 private:
  struct Waiter {
    CoroutineHandle handle;
    Edge edge;
    std::uint64_t lsb_bit_offset;
    std::uint64_t bit_width;
  };
  std::vector<Waiter> waiters_;
};

template <value::LyraValueType T>
class ScopedMutation;

template <value::LyraValueType T>
class Var : public Observable {
 public:
  Var() = default;

  template <typename... Args>
  explicit Var(Args&&... args) : value_(std::forward<Args>(args)...) {
  }

  Var(const Var&) = delete;
  auto operator=(const Var&) -> Var& = delete;
  Var(Var&&) = delete;
  auto operator=(Var&&) -> Var& = delete;
  ~Var() = default;

  // Records the new value and reports whether it differs from the old (LRM
  // 9.4.2 `===` semantics for @() detection -- the engine fires subscribers
  // only on a real change). The store always happens so a freshly-defaulted
  // cell adopts shape and implementation-side seeds (e.g. an
  // `AssociativeArray`'s `oob_slot_`) from the source on the first write,
  // even when the SV-visible content compares identical (both empty).
  auto AssignIfChanged(const T& v) -> bool {
    const bool changed = !value_.IsBitIdentical(v);
    value_ = v;
    return changed;
  }

  auto AssignIfChanged(T&& v) -> bool {
    const bool changed = !value_.IsBitIdentical(v);
    value_ = std::move(v);
    return changed;
  }

  [[nodiscard]] auto Get() const noexcept -> const T& {
    return value_;
  }

  // Commits a whole-variable write and, on a real change (LRM 4.3 update
  // event), wakes subscribers through the engine. Defined out of line below so
  // it can reach the PackedArray edge classifier.
  void Set(RuntimeServices& services, const T& new_val);

  // RAII entry to partial-write context. Construct via `var.Mutate(services)`
  // at the start of a chain; the returned handle snapshots the current value,
  // forwards chain methods so `var.Mutate(svc)[idx].Slice(...) = v` works as a
  // single expression, and commits the mutated snapshot through `Var::Set` in
  // its destructor (fires subscribers on change). Lifetime is C++ standard
  // full-expression temporary lifetime -- the handle is non-copyable and
  // non-movable, so storing it past the statement is rejected at compile time.
  auto Mutate(RuntimeServices& services) -> ScopedMutation<T>;

 private:
  T value_{};
};

// A reference to a variable cell. Transparently views one of two backings:
// an observable `Var<T>` (writes route through `Var::Set` so the update
// event fires and subscribers wake), or a plain `T` cell (raw read / write,
// no observers). Copyable, so a ref formal can be forwarded as a ref
// argument to a nested call.
template <value::LyraValueType T>
class Ref {
 public:
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

  void Set(RuntimeServices& services, const T& new_val) {
    if (signal_ != nullptr) {
      signal_->Set(services, new_val);
    } else {
      *plain_ = new_val;
    }
  }

 private:
  Var<T>* signal_ = nullptr;
  T* plain_ = nullptr;
};

// Suspends the calling frame until one of the supplied Triggers fires
// (matching its edge). Subscribes the frame to every Observable in
// `await_suspend` and records the set on the frame's promise; the engine has no
// idea what kind of wait this is. When one Observable wakes the frame, the
// engine sweeps the recorded set to drop the dangling subscriptions.
//
// An empty trigger list is legal -- the frame suspends forever (used by
// `always_comb` / `always_latch` with no inferred reads,
// e.g. `always_comb c = 7;`).
class EventControlAwaitable {
 public:
  explicit EventControlAwaitable(std::vector<Trigger> triggers)
      : triggers_(std::move(triggers)) {
  }

  [[nodiscard]] static auto await_ready() noexcept -> bool {
    return false;
  }

  void await_suspend(CoroutineHandle handle) {
    std::vector<Observable*> subs;
    subs.reserve(triggers_.size());
    for (const auto& trigger : triggers_) {
      if (trigger.observable == nullptr) {
        throw InternalError(
            "EventControlAwaitable::await_suspend: observable pointer is "
            "null");
      }
      trigger.observable->Subscribe(
          handle, trigger.edge, trigger.lsb_bit_offset, trigger.bit_width);
      subs.push_back(trigger.observable);
    }
    handle.promise().pending_value_change_subscriptions = std::move(subs);
  }

  static void await_resume() noexcept {
  }

 private:
  std::vector<Trigger> triggers_;
};

inline auto WaitAny(std::initializer_list<Trigger> triggers)
    -> EventControlAwaitable {
  return EventControlAwaitable{std::vector<Trigger>(triggers)};
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
             std::uint64_t lsb, std::uint64_t width, Edge edge) -> bool {
    if (edge == Edge::kAnyChange) {
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

template <value::LyraValueType T>
void Var<T>::Set(RuntimeServices& services, const T& new_val) {
  // A PackedArray write classifies the LSB transition per waiter (LRM 9.4.2
  // Table 9-2); every other cell type only has any-change waiters.
  if constexpr (std::same_as<T, value::PackedArray>) {
    const value::PackedArray old_val = Get();
    if (AssignIfChanged(new_val)) {
      services.TriggerValueChange(
          *this, MakePackedArrayEdgeClassifier(old_val, new_val));
    }
  } else {
    if (AssignIfChanged(new_val)) {
      services.TriggerValueChange(
          *this, [](std::uint64_t, std::uint64_t, Edge edge) -> bool {
            return edge == Edge::kAnyChange;
          });
    }
  }
}

// RAII handle that owns a snapshot of `var.Get()` for the lifetime of one
// partial-write expression. Forwards chain methods to the snapshot so emitted
// code reads like a normal `PackedArrayRef` chain. The destructor calls
// `Var::Set` with the (possibly mutated) snapshot -- subscribers fire on
// change. Non-copyable and non-movable: the contract is that it lives only
// until the end of the constructing full expression. Returning it by value
// from `Var<T>::Mutate` relies on C++17 mandatory copy elision (prvalues are
// materialized in the caller's storage with no copy/move).
template <value::LyraValueType T>
class ScopedMutation {
 public:
  ScopedMutation(RuntimeServices& services, Var<T>& var)
      : services_(&services), var_(&var), snapshot_(var.Get()) {
  }

  ScopedMutation(const ScopedMutation&) = delete;
  auto operator=(const ScopedMutation&) -> ScopedMutation& = delete;
  ScopedMutation(ScopedMutation&&) = delete;
  auto operator=(ScopedMutation&&) -> ScopedMutation& = delete;

  ~ScopedMutation() {
    var_->Set(*services_, std::move(snapshot_));
  }

  // Chain forwards. For a packed-storage snapshot the returned
  // `PackedArrayRef` holds a pointer into this ScopedMutation's snapshot_; for
  // an unpacked-storage snapshot (`DynamicArray<T>`, `Queue<T>`, etc.) the
  // forwarder must preserve the reference so the assignment lands in the
  // snapshot (not in a returned-by-value copy).
  auto ElementAt(const value::PackedArray& idx) -> decltype(auto) {
    return snapshot_.ElementAt(idx);
  }
  auto Slice(const value::PackedArray& lsb, std::uint32_t count)
      -> decltype(auto) {
    return snapshot_.Slice(lsb, count);
  }
  // LRM 7.8.7 write-side index access on associative arrays: distinct from
  // `ElementAt` because the AA splits read from write (`Read` for missing
  // keys, `ElementRef` for allocation on write). Member-template so SFINAE
  // suppresses the declaration on `T` types without `ElementRef`.
  template <typename Key, typename U = T>
    requires requires(U& u, const Key& k) { u.ElementRef(k); }
  auto ElementRef(const Key& key) -> decltype(auto) {
    return snapshot_.ElementRef(key);
  }

  // LRM 11.4 whole-var compound. Mirrors `x op= rhs` directly: snapshot is
  // mutated through the underlying type's own `op=`, and the destructor
  // commits via Var::Set. Keeping the emit shape `x.Mutate(svc) op= rhs`
  // (instead of expanding to `x.Set(svc, x.Get() op rhs)`) preserves
  // the compound intent end-to-end and mirrors the selector form
  // `x.Mutate(svc).Chain(...) op= rhs`.
  auto operator+=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ += rhs;
    return *this;
  }
  auto operator-=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ -= rhs;
    return *this;
  }
  auto operator*=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ *= rhs;
    return *this;
  }
  auto operator/=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ /= rhs;
    return *this;
  }
  auto operator%=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ %= rhs;
    return *this;
  }
  auto operator&=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ &= rhs;
    return *this;
  }
  auto operator|=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ |= rhs;
    return *this;
  }
  auto operator^=(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_ ^= rhs;
    return *this;
  }
  auto ShiftLeftAssign(const value::PackedArray& rhs) -> ScopedMutation& {
    snapshot_.ShiftLeftAssign(rhs);
    return *this;
  }
  auto LogicalShiftRightAssign(const value::PackedArray& rhs)
      -> ScopedMutation& {
    snapshot_.LogicalShiftRightAssign(rhs);
    return *this;
  }
  auto ArithmeticShiftRightAssign(const value::PackedArray& rhs)
      -> ScopedMutation& {
    snapshot_.ArithmeticShiftRightAssign(rhs);
    return *this;
  }

  // LRM 11.4.2 inc/dec on an observable whole-var. The snapshot is mutated
  // in place; the destructor commits via Var::Set so subscribers fire exactly
  // once. Prefix returns the new value; postfix returns the old. Both return
  // by value (not `ScopedMutation&`) so outer rvalue use such as
  // `b = ++observable_var` routes a materialized PackedArray through the
  // outer Var::Set instead of a transient proxy.
  auto operator++() -> value::PackedArray {
    ++snapshot_;
    return snapshot_;
  }
  auto operator++(int) -> value::PackedArray {
    value::PackedArray prior = snapshot_;
    ++snapshot_;
    return prior;
  }
  auto operator--() -> value::PackedArray {
    --snapshot_;
    return snapshot_;
  }
  auto operator--(int) -> value::PackedArray {
    value::PackedArray prior = snapshot_;
    --snapshot_;
    return prior;
  }

  // Drilldown to the underlying snapshot for instance methods on `T` that
  // mutate the value in place (e.g. `String::Itoa`). The chain becomes
  // `var.Mutate(svc)->Method(...)` and the destructor commits the mutated
  // snapshot exactly once.
  auto operator->() -> T* {
    return &snapshot_;
  }
  // The dereference form for free-function callees that take a mutable
  // reference (e.g. `LyraFRead(svc, var.Mutate(svc), ...)` -- spelled
  // `*var.Mutate(svc)` at the call site).
  auto operator*() -> T& {
    return snapshot_;
  }

 private:
  RuntimeServices* services_;
  Var<T>* var_;
  T snapshot_;
};

template <value::LyraValueType T>
auto Var<T>::Mutate(RuntimeServices& services) -> ScopedMutation<T> {
  return ScopedMutation<T>{services, *this};
}

}  // namespace lyra::runtime
