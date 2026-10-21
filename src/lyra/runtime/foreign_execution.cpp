#include "lyra/runtime/foreign_execution.hpp"

#include <boost/context/fiber.hpp>
#include <boost/context/protected_fixedsize_stack.hpp>
#include <boost/context/stack_context.hpp>
#include <cstddef>
#include <iterator>
#include <memory>
#include <sanitizer/asan_interface.h>
#include <sanitizer/common_interface_defs.h>
#include <utility>

#include "lyra/base/simulation_error.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/owned_call.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

ForeignExecution::~ForeignExecution() = default;

namespace {

namespace ctx = boost::context;

class Fiber;

// The fiber whose entry is executing on this thread right now, so a yield from
// deep inside the foreign call finds the fiber to switch out of without the
// call site naming it. A stackful switch never migrates threads, so this stays
// correct on the single simulation thread.
auto RunningFiberSlot() -> Fiber*& {
  static thread_local Fiber* running = nullptr;
  return running;
}

// The process whose foreign call is running on this thread right now. The
// exported-task driver reaches it here because foreign C hands it no engine
// handle. Guarded so a nested foreign call restores its caller's process.
auto ForeignProcessSlot() -> RuntimeProcess*& {
  static thread_local RuntimeProcess* process = nullptr;
  return process;
}

// A program built to check its memory links the address sanitizer, which keeps
// its own account of the stack running, and has to be told of every switch to
// another one: a throw on a stack it was not told of is unwound against the
// wrong bounds, and it warns that false reports may follow. A program built
// without it links none of this, so the declarations are weak and a switch
// tells nobody.
#pragma weak __sanitizer_start_switch_fiber
#pragma weak __sanitizer_finish_switch_fiber
#pragma weak __asan_unpoison_memory_region

struct StackBounds {
  const void* bottom = nullptr;
  std::size_t size = 0;
};

// Called on the stack being left, just before the switch. Without
// `fake_stack` the sanitizer frees the frames it keeps for the stack being left
// to catch a use after return.
void StartSwitch(void** fake_stack, StackBounds to) {
  if (__sanitizer_start_switch_fiber != nullptr) {
    __sanitizer_start_switch_fiber(fake_stack, to.bottom, to.size);
  }
}

// Called on the stack switched to, first thing; `from`, where given, receives
// the bounds of the stack that was left.
void FinishSwitch(void* fake_stack, StackBounds* from) {
  if (__sanitizer_finish_switch_fiber != nullptr) {
    __sanitizer_finish_switch_fiber(
        fake_stack, from == nullptr ? nullptr : &from->bottom,
        from == nullptr ? nullptr : &from->size);
  }
}

// The guard-page protected stack a fiber runs on, recording where it lies.
class BoundedStack {
 public:
  explicit BoundedStack(StackBounds& bounds) : bounds_(&bounds) {
  }

  auto allocate() -> ctx::stack_context {
    ctx::stack_context stack = inner_.allocate();
    // The library hands over the top, where a stack growing down starts.
    *bounds_ = {
        .bottom = std::prev(
            static_cast<const std::byte*>(stack.sp),
            static_cast<std::ptrdiff_t>(stack.size)),
        .size = stack.size};
    return stack;
  }

  // A sanitizer runtime need not clear what it marked of the stack's frames
  // when the stack is unmapped, and the marks would then fall on whatever
  // memory takes its place.
  void deallocate(ctx::stack_context& stack) noexcept {
    if (__asan_unpoison_memory_region != nullptr) {
      __asan_unpoison_memory_region(bounds_->bottom, bounds_->size);
    }
    inner_.deallocate(stack);
  }

 private:
  ctx::protected_fixedsize_stack inner_;
  StackBounds* bounds_;
};

// A foreign call running on its own stack (guard-page protected). The entry
// runs to its first suspension or its return on `Resume`; a suspension switches
// back out via `Yield`, and the next `Resume` continues just past it. The two
// continuations are duals: `fiber_` names the suspended foreign stack while
// control is outside it, `home_` names the resumer while control is inside.
//
// Every switch that runs code on the fiber's stack once the entry has begun is
// told to the sanitizer from both sides, including the two the fiber library
// makes on its own: leaving the entry for good, and unwinding a suspended entry
// when the fiber is destroyed, which throws on the fiber's stack out of the
// switch in `Yield`. Before that, the library's round trip into the entry as
// the fiber is made runs nothing, and unwinding a fiber destroyed before it was
// first resumed follows only an error on the way into the call; neither is
// told.
class Fiber final : public ForeignExecution {
 public:
  explicit Fiber(OwnedCall entry)
      : entry_(std::move(entry)),
        fiber_(
            std::allocator_arg, BoundedStack{fiber_stack_},
            [this](ctx::fiber&& home) {
              FinishSwitch(nullptr, &home_stack_);
              entered_ = true;
              home_ = std::move(home);
              const ExitSwitchGuard leaving(*this);
              entry_();
              done_ = true;
              return std::move(home_);
            }) {
  }

  Fiber(const Fiber&) = delete;
  Fiber(Fiber&&) = delete;
  auto operator=(const Fiber&) -> Fiber& = delete;
  auto operator=(Fiber&&) -> Fiber& = delete;

  ~Fiber() override {
    if (entered_ && !done_) {
      void* fake_stack = nullptr;
      StartSwitch(&fake_stack, fiber_stack_);
      fiber_ = ctx::fiber{};
      FinishSwitch(fake_stack, nullptr);
    }
    FreeFakeStack();
  }

  void Resume() override {
    Fiber* previous = RunningFiberSlot();
    RunningFiberSlot() = this;
    void* fake_stack = nullptr;
    StartSwitch(&fake_stack, fiber_stack_);
    fiber_ = std::move(fiber_).resume();
    FinishSwitch(fake_stack, nullptr);
    RunningFiberSlot() = previous;
  }

  [[nodiscard]] auto IsDone() const -> bool override {
    return done_;
  }

  void Yield() {
    void* fake_stack = nullptr;
    StartSwitch(&fake_stack, home_stack_);
    const SwitchBackGuard back(*this, fake_stack);
    home_ = std::move(home_).resume();
  }

 private:
  // Back on the fiber's stack after a `Yield`, whether the switch returned or
  // threw because the fiber is being destroyed.
  class SwitchBackGuard {
   public:
    SwitchBackGuard(Fiber& fiber, void* fake_stack)
        : fiber_(&fiber), fake_stack_(fake_stack) {
    }
    SwitchBackGuard(const SwitchBackGuard&) = delete;
    SwitchBackGuard(SwitchBackGuard&&) = delete;
    auto operator=(const SwitchBackGuard&) -> SwitchBackGuard& = delete;
    auto operator=(SwitchBackGuard&&) -> SwitchBackGuard& = delete;
    ~SwitchBackGuard() {
      FinishSwitch(fake_stack_, &fiber_->home_stack_);
    }

   private:
    Fiber* fiber_;
    void* fake_stack_;
  };

  // The entry is ending, by returning or by being unwound, and the fiber
  // library switches to its resumer for good once this has run. The sanitizer
  // is not told the stack is left for good, which would free at once the
  // frames it keeps for this stack to catch a use after return, while the
  // library's own frames still run on them; they are kept until the fiber is
  // destroyed.
  class ExitSwitchGuard {
   public:
    explicit ExitSwitchGuard(Fiber& fiber) : fiber_(&fiber) {
    }
    ExitSwitchGuard(const ExitSwitchGuard&) = delete;
    ExitSwitchGuard(ExitSwitchGuard&&) = delete;
    auto operator=(const ExitSwitchGuard&) -> ExitSwitchGuard& = delete;
    auto operator=(ExitSwitchGuard&&) -> ExitSwitchGuard& = delete;
    ~ExitSwitchGuard() {
      StartSwitch(&fiber_->fake_stack_, fiber_->home_stack_);
    }

   private:
    Fiber* fiber_;
  };

  // Frees the frames the entry's exit kept, once the fiber's stack runs
  // nothing. The sanitizer offers no call that frees them outright, so they are
  // taken over as this stack's, left for good, which frees them, and this
  // stack's own are then taken back.
  void FreeFakeStack() {
    if (fake_stack_ == nullptr) {
      return;
    }
    void* own = nullptr;
    StackBounds here;
    StartSwitch(&own, {});
    FinishSwitch(fake_stack_, &here);
    StartSwitch(nullptr, here);
    FinishSwitch(own, nullptr);
  }

  StackBounds fiber_stack_;
  StackBounds home_stack_;
  void* fake_stack_ = nullptr;
  OwnedCall entry_;
  ctx::fiber fiber_;
  ctx::fiber home_;
  bool entered_ = false;
  bool done_ = false;
};

}  // namespace

auto MakeForeignExecution(OwnedCall entry)
    -> std::unique_ptr<ForeignExecution> {
  return std::make_unique<Fiber>(std::move(entry));
}

void YieldForeignExecution() {
  if (RunningFiberSlot() == nullptr) {
    // LRM 35.5.3.
    throw SimulationError(
        "an exported task suspended outside a foreign call, and a task that "
        "consumes simulation time is callable only from a foreign call the "
        "simulation made");
  }
  RunningFiberSlot()->Yield();
}

ForeignExecutionGuard::ForeignExecutionGuard(
    RuntimeProcess& process, ForeignExecution& fe)
    : process_(&process),
      previous_execution_(
          std::exchange(process.current_foreign_execution_, &fe)),
      previous_process_(std::exchange(ForeignProcessSlot(), &process)) {
}

ForeignExecutionGuard::~ForeignExecutionGuard() {
  process_->current_foreign_execution_ = previous_execution_;
  ForeignProcessSlot() = previous_process_;
}

void DriveOnForeignStack(Activation* frame) {
  frame->coroutine.resume();
  while (!frame->coroutine.done()) {
    YieldForeignExecution();
    frame->coroutine.resume();
  }
}

auto EnterForeignTask(
    RuntimeEffects& effects, Activation* continuation,
    std::unique_ptr<ForeignExecution> fiber) -> bool {
  return effects.CurrentProcess().EnterForeignExecution(
      continuation, std::move(fiber));
}

auto CurrentForeignProcess() -> RuntimeProcess& {
  if (ForeignProcessSlot() == nullptr) {
    // LRM 35.5.3.
    throw SimulationError(
        "a DPI export was reached from outside a foreign call, and an exported "
        "subroutine runs as the process that entered the foreign call, so "
        "there must be one");
  }
  return *ForeignProcessSlot();
}

auto RunForeignTaskOnFiber(
    RuntimeEffects& effects,
    OwnedClosure<support::EffectSignature> foreign_call) -> bool {
  return !EnterForeignTask(
      effects, effects.CurrentProcess().CurrentLeaf(),
      MakeForeignExecution(std::move(foreign_call)));
}

}  // namespace lyra::runtime
