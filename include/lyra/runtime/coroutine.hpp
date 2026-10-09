#pragma once

#include <coroutine>
#include <exception>
#include <utility>
#include <variant>

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/intrusive_list.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

class RuntimeProcess;
struct Activation;

// An activation's place in a queue the scheduler drains, as a node of that
// queue's list.
struct QueuePlace : IntrusiveListNode<QueuePlace> {
  // Null on the sentinel a list embeds to close its ring.
  Activation* activation = nullptr;
};

// One suspendable execution -- a process body, a task body, a fork branch --
// as the scheduler sees it: the wait it is parked on, the queue it sits in,
// whom its completion goes to. It is the part of every frame's promise that
// does not depend on the value the frame completes with, so the engine and
// everything an execution waits on hold an `Activation*` and never name the
// frame's result type.
struct Activation {
  // The process whose thread this frame runs in: its own for a process body or
  // a fork branch, its caller's for a called task (LRM 9.5).
  RuntimeProcess* process = nullptr;
  // This frame, type-erased, so the engine resumes it and asks whether it is
  // done without knowing its result type. Stored where the frame is made,
  // because a promise of one type cannot be turned back into its frame through
  // a base of it.
  std::coroutine_handle<> coroutine;

  // The frame awaiting this one's completion, resumed in its place (a task
  // returning to its caller); none for a process body, which ends its process.
  std::coroutine_handle<> continuation;

  // The awaiter this activation is stopped at, while it is blocked and only
  // then. The frame owns it; this only names it. Stopping the process from
  // outside keeps it, and starting it again waits for the same thing afresh
  // through it (LRM 9.7).
  Awaiter* awaiter = nullptr;

  // This activation's place in the scheduler queue, delay slot or end-of-run
  // list it sits in. It is in at most one at a time and is put in one at
  // nearly every stop, so the place is part of the activation.
  QueuePlace queued;

  // Defined in this struct's own source file, as is every member of it that is
  // not parameterized: what a member of the shipped surface compiles to is the
  // library's to decide once, and a definition written in a header is decided
  // again in every translation unit that reaches it -- a constructor or
  // destructor defaulted here included, since every frame a unit states
  // constructs and destroys one.
  Activation();
  Activation(const Activation&) = delete;
  auto operator=(const Activation&) -> Activation& = delete;
  Activation(Activation&&) = delete;
  auto operator=(Activation&&) -> Activation& = delete;
  ~Activation();

  // Puts this activation in `queue`, to run when the queue does.
  void Queue(IntrusiveList<QueuePlace>& queue) noexcept;

  // Parks this activation on `stopped_at`: it names that awaiter, and the
  // awaiter names it as what an occurrence there wakes.
  void ParkOn(Awaiter& stopped_at) noexcept;

  // Takes this activation out of whatever would resume it: off the queue it
  // sits in, and no longer parked on its awaiter, which it keeps. It is
  // runnable again, or stopped from outside, so nothing it was waiting on may
  // resume it.
  void Withdraw() noexcept;

  static auto initial_suspend() noexcept -> std::suspend_always;

  // What runs once this frame completes: the frame awaiting it (a task
  // returning to its caller), or nothing, so a process body suspends at its end
  // and the engine sees `done()`.
  [[nodiscard]] auto ContinuationAfterCompletion() const noexcept
      -> std::coroutine_handle<>;

  [[nodiscard]] auto Process() const -> RuntimeProcess&;
};

// A nested activation -- a called task -- takes over its process's thread and
// gives it back when it completes (LRM 9.5). The pair is what makes "which
// frame is running" a fact the runtime holds, so a stop made from anywhere
// parks the frame that actually made it and nothing has to be handed one.
void EnterActivation(Activation& leaf);
void LeaveActivation(Activation& leaf);

// Enters `nested` as an activation of the process running now, to return to
// `continuation` when it completes.
void EnterNestedActivation(
    Activation& nested, std::coroutine_handle<> continuation);

// The activation was left by a control effect no region claimed, which its
// landing reports as a forced termination (LRM 9.6.2, 9.7).
//
// It and the form below are held wherever an outcome is, which is in every unit
// whose bodies complete with a value, so their special members are the
// library's.
struct Cancelled {
  explicit Cancelled(std::exception_ptr effect);
  Cancelled(const Cancelled&);
  auto operator=(const Cancelled&) -> Cancelled&;
  Cancelled(Cancelled&&) noexcept;
  auto operator=(Cancelled&&) noexcept -> Cancelled&;
  ~Cancelled();

  std::exception_ptr effect;
};

// The activation was left by a run-time error -- the design's (LRM 20.10) or
// the tool's own -- which its landing reports and ends the run on.
struct Raised {
  explicit Raised(std::exception_ptr error);
  Raised(const Raised&);
  auto operator=(const Raised&) -> Raised&;
  Raised(Raised&&) noexcept;
  auto operator=(Raised&&) noexcept -> Raised&;
  ~Raised();

  std::exception_ptr error;
};

// The single typed terminal outcome an activation settles: the value it
// produced, or the departure that reached its landing in one of the two forms
// above. Kept off the activation so the scheduler never sees `T` or the
// outcome.
// The slot stores the outcome and hands it to the activation's one consumer;
// whether it is re-raised in place or extracted first and re-raised after the
// frame is torn down is the consumer's decision, not the slot's. The
// alternatives are mutually exclusive -- a frame runs `return_value` /
// `return_void` xor `unhandled_exception` -- and the initial monostate is the
// not-yet-settled state, which is also where an activation cancelled while
// parked ends: it is released without resuming, so nothing ever settles here.
//
// Both departures arrive by unwinding, which is how a body this slot is the
// promise of is left. Which of the two it was is settled where the effect is
// raised, while its type can still be read, and reaches this slot already
// answered -- rather than being re-derived later by raising it again to ask, or
// carried beside the slot in a flag nothing checks against it. Both hold the
// same `exception_ptr`; the one it sits in is what says which it is.
template <class T>
class CompletionSlot {
 public:
  void return_value(T value) {
    outcome_.template emplace<Value>(std::move(value));
  }
  void unhandled_exception() noexcept {
    Unwound unwound = ClassifyUnwind();
    if (unwound.control_effect) {
      outcome_.template emplace<Cancelled>(std::move(unwound.raised));
      return;
    }
    outcome_.template emplace<Raised>(std::move(unwound.raised));
  }
  // An awaiting frame is not the activation's landing: both departures carry on
  // past it, which is why both are raised here.
  auto Take() -> T {
    if (const auto* cancelled = std::get_if<Cancelled>(&outcome_)) {
      std::rethrow_exception(cancelled->effect);
    }
    if (const auto* raised = std::get_if<Raised>(&outcome_)) {
      std::rethrow_exception(raised->error);
    }
    return std::move(std::get<Value>(outcome_).held);
  }

 private:
  struct Value {
    T held;
  };
  std::variant<std::monostate, Value, Cancelled, Raised> outcome_;
};

// The one such slot no body parameterizes -- a process body and every task
// returning nothing settle through it -- so what it compiles to is decided once
// in the library rather than in every unit that states a body.
template <>
class CompletionSlot<void> {
 public:
  CompletionSlot();
  CompletionSlot(const CompletionSlot&) = delete;
  auto operator=(const CompletionSlot&) -> CompletionSlot& = delete;
  CompletionSlot(CompletionSlot&&) = delete;
  auto operator=(CompletionSlot&&) -> CompletionSlot& = delete;
  ~CompletionSlot();

  void return_void();
  void unhandled_exception() noexcept;
  // Whether the body was left by a control effect no region claimed, which its
  // landing reports as a forced termination rather than as an end of body.
  [[nodiscard]] auto WasCancelled() const -> bool;
  // Hands a raised error out without raising it (null unless one left the
  // body), so a consumer that must run its own teardown first can settle and
  // report afterward.
  auto TakeRaisedError() -> std::exception_ptr;
  // An awaiting frame is not the activation's landing: both departures carry on
  // past it, which is why both are raised here.
  void Take();

 private:
  struct Succeeded {};
  std::variant<std::monostate, Succeeded, Cancelled, Raised> outcome_;
};

// A suspendable activation that completes with a value of `T` -- a task body,
// or a process body with `T = void`. The completion value travels through the
// promise: the body's `co_return v` stores it, and the awaiting frame moves it
// out in `await_resume` before this `Coroutine` (which owns the frame) is
// destroyed. All scheduling lives on the non-templated `Activation` the engine
// sees; nothing per-`T` reaches the scheduler.
//
// A `Coroutine` is lazy (suspends before its first statement) and is its own
// awaiter, so a task is enabled with `co_await task(args)`. The awaiting
// frame's receiver is a single consumer of the completion value.
template <class T>
class Coroutine {
 public:
  struct promise_type : Activation, CompletionSlot<T> {
    // Defaulted after the class rather than here: left implicit, the frame's
    // promise would be constructed and destroyed by code every unit stating a
    // body defines itself, which no statement that the family is already
    // compiled reaches.
    promise_type();
    promise_type(const promise_type&) = delete;
    auto operator=(const promise_type&) -> promise_type& = delete;
    promise_type(promise_type&&) = delete;
    auto operator=(promise_type&&) -> promise_type& = delete;
    ~promise_type();

    auto get_return_object() -> Coroutine {
      auto handle = std::coroutine_handle<promise_type>::from_promise(*this);
      coroutine = handle;
      return Coroutine{handle};
    }

    // On completion, control transfers to what continues this frame. The frame
    // the language hands over is the one completing, so its promise is where
    // that is read.
    struct FinalAwaiter {
      [[nodiscard]] static auto await_ready() noexcept -> bool;
      [[nodiscard]] static auto await_suspend(
          std::coroutine_handle<promise_type> completing) noexcept
          -> std::coroutine_handle<>;
      static void await_resume() noexcept;
    };
    static auto final_suspend() noexcept -> FinalAwaiter;
  };

  Coroutine() = default;
  Coroutine(const Coroutine&) = delete;
  auto operator=(const Coroutine&) -> Coroutine& = delete;

  Coroutine(Coroutine&& other) noexcept
      : handle_(std::exchange(other.handle_, {})) {
  }
  auto operator=(Coroutine&& other) noexcept -> Coroutine& {
    if (handle_) {
      handle_.destroy();
    }
    handle_ = std::exchange(other.handle_, {});
    return *this;
  }
  ~Coroutine() {
    if (handle_) {
      handle_.destroy();
    }
  }

  // Awaiter surface: `co_await task(args)` enables the task. Starting it is a
  // symmetric transfer to its handle; the task carries this enabler as its
  // continuation and runs as part of the process running now, which is the
  // enabler's. Nothing about the enabler's own frame is read, so what it
  // completes with does not shape this.
  [[nodiscard]] auto await_ready() const noexcept -> bool {
    return !handle_ || handle_.done();
  }
  auto await_suspend(std::coroutine_handle<> caller)
      -> std::coroutine_handle<promise_type> {
    EnterNestedActivation(handle_.promise(), caller);
    return handle_;
  }
  auto await_resume() -> T {
    LeaveActivation(handle_.promise());
    return handle_.promise().Take();
  }

  [[nodiscard]] auto Handle() const -> std::coroutine_handle<promise_type> {
    return handle_;
  }

  // The scheduling token for this frame -- what the engine queues and resumes.
  [[nodiscard]] auto Token() const -> Activation* {
    return handle_ ? &handle_.promise() : nullptr;
  }

  [[nodiscard]] auto Done() const -> bool {
    return !handle_ || handle_.done();
  }

  // Wires the promise's back-pointer to the owning RuntimeProcess. Called from
  // RuntimeProcess construction for the top-level coroutine.
  void BindProcess(RuntimeProcess& process) {
    if (handle_) {
      handle_.promise().process = &process;
    }
  }

 private:
  explicit Coroutine(std::coroutine_handle<promise_type> handle)
      : handle_(handle) {
  }

  std::coroutine_handle<promise_type> handle_;
};

template <class T>
Coroutine<T>::promise_type::promise_type() = default;

template <class T>
Coroutine<T>::promise_type::~promise_type() = default;

template <class T>
auto Coroutine<T>::promise_type::FinalAwaiter::await_ready() noexcept -> bool {
  return false;
}

template <class T>
auto Coroutine<T>::promise_type::FinalAwaiter::await_suspend(
    std::coroutine_handle<promise_type> completing) noexcept
    -> std::coroutine_handle<> {
  return completing.promise().ContinuationAfterCompletion();
}

template <class T>
void Coroutine<T>::promise_type::FinalAwaiter::await_resume() noexcept {
}

template <class T>
auto Coroutine<T>::promise_type::final_suspend() noexcept -> FinalAwaiter {
  return {};
}

}  // namespace lyra::runtime
