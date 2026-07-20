#include "lyra/runtime/foreign_execution.hpp"

#include <boost/context/fiber.hpp>
#include <boost/context/protected_fixedsize_stack.hpp>
#include <functional>
#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

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

// A foreign call running on its own stack (guard-page protected). The entry
// runs to its first suspension or its return on `Resume`; a suspension switches
// back out via `Yield`, and the next `Resume` continues just past it. The two
// continuations are duals: `fiber_` names the suspended foreign stack while
// control is outside it, `home_` names the resumer while control is inside.
class Fiber final : public ForeignExecution {
 public:
  explicit Fiber(std::function<void()> entry)
      : entry_(std::move(entry)),
        fiber_(
            std::allocator_arg, ctx::protected_fixedsize_stack{},
            [this](ctx::fiber&& home) {
              home_ = std::move(home);
              entry_();
              done_ = true;
              return std::move(home_);
            }) {
  }

  void Resume() override {
    Fiber* previous = RunningFiberSlot();
    RunningFiberSlot() = this;
    fiber_ = std::move(fiber_).resume();
    RunningFiberSlot() = previous;
  }

  [[nodiscard]] auto IsDone() const -> bool override {
    return done_;
  }

  void Yield() {
    home_ = std::move(home_).resume();
  }

 private:
  std::function<void()> entry_;
  ctx::fiber fiber_;
  ctx::fiber home_;
  bool done_ = false;
};

}  // namespace

auto MakeForeignExecution(std::function<void()> entry)
    -> std::unique_ptr<ForeignExecution> {
  return std::make_unique<Fiber>(std::move(entry));
}

void YieldForeignExecution() {
  if (RunningFiberSlot() == nullptr) {
    throw InternalError(
        "YieldForeignExecution: no foreign execution running on this thread");
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

auto CurrentForeignProcess() -> RuntimeProcess& {
  if (ForeignProcessSlot() == nullptr) {
    throw InternalError(
        "CurrentForeignProcess: no foreign call running on this thread");
  }
  return *ForeignProcessSlot();
}

}  // namespace lyra::runtime
