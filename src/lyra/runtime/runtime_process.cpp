#include "lyra/runtime/runtime_process.hpp"

#include <algorithm>
#include <coroutine>
#include <cstddef>
#include <exception>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/foreign_execution.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(
    Scope* owning_scope, Coroutine<void> coroutine, RandomSeed seed,
    const char* written_at)
    : owning_scope_(owning_scope),
      written_at_(written_at),
      coroutine_(std::move(coroutine)),
      running_(RunningState{.rng = DrawRng{seed}, .import_calls = {}}),
      // Before the body runs, the top frame is the active leaf (what the engine
      // schedules to start the process); a wait moves the leaf inward.
      current_leaf_(coroutine_.Token()) {
  // Wire the promise's back-pointer so a frame can recover the RuntimeProcess
  // identity from itself, which is what a wait reads to find the process the
  // frame it parks belongs to.
  coroutine_.BindProcess(*this);
}

RuntimeProcess::~RuntimeProcess() = default;

auto RuntimeProcess::TopActivation() const -> Activation* {
  return coroutine_.Token();
}

auto RuntimeProcess::Parent() const -> RuntimeProcess* {
  return parent_;
}

auto RuntimeProcess::ParkAt(RuntimeEffects& services, Awaiter& awaiter)
    -> bool {
  if (!Arrange(services, current_leaf_, awaiter, awaiter.Begin())) {
    return false;
  }
  // The vehicle carrying this thread when it blocks is the one the scheduler
  // must drive to resume it, kept past the block because a resume runs from
  // another process's context, where the ambient vehicle is that caller's.
  resume_target_ = current_foreign_execution_;
  return true;
}

auto RuntimeProcess::Arrange(
    RuntimeEffects& services, Activation* leaf, Awaiter& awaiter,
    const Resumption& resumption) -> bool {
  return std::visit(
      Overloaded{
          [](const WithoutStopping&) { return false; },
          [&](const OnAnOccurrence&) {
            leaf->ParkOn(awaiter);
            return true;
          },
          [&](const LaterInThisTimeStep& later) {
            leaf->ParkOn(awaiter);
            services.Schedule(services.Now(), later.region, leaf);
            return true;
          },
          [&](const AtTime& at) {
            // A time already reached leaves nothing to wait for.
            if (at.when <= services.Now()) {
              return false;
            }
            leaf->ParkOn(awaiter);
            services.Schedule(at.when, Region::kActive, leaf);
            return true;
          }},
      resumption);
}

auto RuntimeProcess::BlockedLeaf() const -> Activation* {
  if (execution_state_ != ProcessExecutionState::kWaiting ||
      current_leaf_->awaiter == nullptr) {
    return nullptr;
  }
  return current_leaf_;
}

auto RuntimeProcess::PushActivation(Coroutine<void> nested) -> Activation* {
  nested_activations_.push_back(std::move(nested));
  Activation* const leaf = nested_activations_.back().Token();
  // A called task runs in its caller's thread (LRM 9.5), so it reaches the same
  // identity, lineage and disable membership as the body that called it.
  leaf->process = this;
  EnterLeaf(leaf);
  return leaf;
}

void RuntimeProcess::PopActivation() {
  LeaveLeaf();
  nested_activations_.pop_back();
}

void RuntimeProcess::EnterLeaf(Activation* leaf) {
  outer_leaves_.push_back(current_leaf_);
  current_leaf_ = leaf;
}

void RuntimeProcess::LeaveLeaf() {
  current_leaf_ = outer_leaves_.back();
  outer_leaves_.pop_back();
}

void EnterActivation(Activation& leaf) {
  leaf.Process().EnterLeaf(&leaf);
}

void LeaveActivation(Activation& leaf) {
  leaf.Process().LeaveLeaf();
}

void EnterNestedActivation(
    Activation& nested, std::coroutine_handle<> continuation) {
  nested.continuation = continuation;
  nested.process = current_runtime().TryCurrentProcess();
  EnterActivation(nested);
}

auto RuntimeProcess::TakeInnermostRaisedError() -> std::exception_ptr {
  return nested_activations_.back().Handle().promise().TakeRaisedError();
}

void RuntimeProcess::Suspend() {
  // A process on its way to terminating is handed control only so that it can
  // finish leaving; stopping it there would leave it never terminated.
  if (execution_state_ == ProcessExecutionState::kSuspended ||
      execution_state_ == ProcessExecutionState::kTerminated ||
      termination_requested_) {
    return;
  }
  // Take the leaf off whatever could resume it -- the awaiter it is parked on
  // or the queue it sits in. The awaiter itself stays, because starting the
  // process again waits for that same thing.
  current_leaf_->Withdraw();
  execution_state_ = ProcessExecutionState::kSuspended;
}

void RuntimeProcess::Resume(RuntimeEffects& effects) {
  if (execution_state_ != ProcessExecutionState::kSuspended) {
    return;
  }
  execution_state_ = ProcessExecutionState::kWaiting;
  Activation* const leaf = current_leaf_;
  // An activation names an awaiter exactly while it is blocked, so naming none
  // is how a process stopped while already runnable -- woken, but not yet run
  // -- says that it has nothing left to wait for. One owed a departure has
  // nothing left to wait for either: it resumes only to take it.
  Awaiter* const awaiter = leaf->awaiter;
  const bool waits_again = awaiter != nullptr && !DepartureIsDue() &&
                           Arrange(effects, leaf, *awaiter, awaiter->Again());
  if (!waits_again) {
    effects.Wake(leaf);
  }
}

auto RuntimeProcess::HasNoLiveChild() const -> bool {
  return std::ranges::all_of(children_, [](const auto& child) {
    return child->execution_state_ == ProcessExecutionState::kTerminated;
  });
}

void RuntimeProcess::DisableDescendants(RuntimeEffects& services) {
  for (const std::shared_ptr<RuntimeProcess>& child : children_) {
    // Sever the upward link before the recursion severs the downward ones, so a
    // handle-held child left behind by the clear below is a parent-less orphan
    // rather than a node pointing into freed storage.
    child->parent_ = nullptr;
    child->TerminateSubtreeKilled(services);
  }
  children_.clear();
}

void RuntimeProcess::SettleOrRequestKilled(RuntimeEffects& services) {
  if (execution_state_ == ProcessExecutionState::kTerminated) {
    return;
  }
  // An execution with a foreign call still to return cannot be torn down where
  // it stands: the frames between it and its own belong to another language,
  // and such a frame ends only by running. So it is asked to stop and handed
  // control once more instead -- it leaves at the gate its own body passes, its
  // foreign frames return of their own accord (LRM 35.9), and the terminal
  // state is published when the body finally settles.
  if (HasLiveForeignCall()) {
    RequestTermination(ProcessTerminationCause::kKilled);
    services.Wake(current_leaf_);
    return;
  }
  SettleTerminated(ProcessTerminationCause::kKilled, services);
}

void RuntimeProcess::TerminateSubtreeKilled(RuntimeEffects& services) {
  DisableDescendants(services);
  SettleOrRequestKilled(services);
}

void RuntimeProcess::TerminateSubtreeDeferringRunning(
    RuntimeProcess& running, RuntimeEffects& services) {
  // Off-path children are killed and severed synchronously; the one child on
  // the path down to `running` is kept linked and recursed into, so the chain
  // that owns `running` survives until `running` settles at its safe boundary.
  std::erase_if(children_, [&](const std::shared_ptr<RuntimeProcess>& child) {
    if (child->IsSelfOrAncestorOf(running)) {
      child->TerminateSubtreeDeferringRunning(running, services);
      return false;
    }
    child->parent_ = nullptr;
    child->TerminateSubtreeKilled(services);
    return true;
  });
  if (this == &running) {
    RequestTermination(ProcessTerminationCause::kKilled);
    return;
  }
  SettleOrRequestKilled(services);
}

auto RuntimeProcess::IsSelfOrAncestorOf(const RuntimeProcess& other) const
    -> bool {
  for (const RuntimeProcess* node = &other; node != nullptr;
       node = node->parent_) {
    if (node == this) {
      return true;
    }
  }
  return false;
}

void RuntimeProcess::DetachFromParent() {
  if (parent_ == nullptr) {
    return;
  }
  RuntimeProcess* parent = parent_;
  parent_ = nullptr;
  parent->EraseChild(*this);
}

auto RuntimeProcess::IsReleasable() const -> bool {
  return execution_state_ == ProcessExecutionState::kTerminated &&
         children_.empty();
}

void RuntimeProcess::AdoptChild(std::shared_ptr<RuntimeProcess> child) {
  child->parent_ = this;
  children_.push_back(std::move(child));
}

void RuntimeProcess::EraseChild(RuntimeProcess& child) {
  const std::size_t erased = std::erase_if(
      children_, [&](const std::shared_ptr<RuntimeProcess>& node) {
        return node.get() == &child;
      });
  if (erased != 1) {
    throw InternalError("RuntimeProcess::EraseChild: child is not ours");
  }
}

void RuntimeProcess::ReleaseTerminatedLineage(RuntimeProcess& process) {
  RuntimeProcess* node = &process;
  while (node->parent_ != nullptr && node->IsReleasable()) {
    RuntimeProcess* parent = node->parent_;
    parent->EraseChild(*node);
    node = parent;
  }
}

auto RuntimeProcess::DriveForeignVehicle(ForeignExecution& fe) -> bool {
  const ForeignExecutionGuard guard(*this, fe);
  // Foreign code reached from here may call back into generated code (LRM
  // 35.7) that keeps values across a park, so each stretch of the call names
  // the call's own store. The scope is one stretch of the call rather than the
  // whole of it, because the stack parks between two stretches and a scope open
  // across that would still be the innermost one while some other execution
  // ran. No generated body completes into it -- the call ends by the foreign
  // code returning -- so nothing settles a departure here.
  const GeneratedCallScope stretch(fe.Values(), nullptr);
  fe.Resume();
  return fe.IsDone();
}

auto RuntimeProcess::EnterForeignExecution(
    Activation* continuation, std::unique_ptr<ForeignExecution> fe) -> bool {
  ForeignExecution& entered =
      *foreign_calls_
           .emplace_back(
               ForeignCall{
                   .vehicle = std::move(fe), .continuation = continuation})
           .vehicle;
  // The call returned without suspending: nothing snapshotted the vehicle, and
  // the caller continues inline. If instead it suspended, an inner frame parked
  // with this vehicle as its resume target, and the scheduler will drive it.
  if (DriveForeignVehicle(entered)) {
    foreign_calls_.pop_back();
    return true;
  }
  return false;
}

void RuntimeProcess::RequestTermination(ProcessTerminationCause cause) {
  if (execution_state_ == ProcessExecutionState::kTerminated ||
      termination_requested_) {
    return;
  }
  termination_requested_ = true;
  termination_cause_ = cause;
  // Take the leaf off whatever could resume it -- its awaiter or a run queue --
  // explicitly. The frame is not destroyed here (it is still going to unwind),
  // so nothing would do it implicitly, and doing it now is what keeps a
  // settled-later frame un-nameable in between.
  current_leaf_->Withdraw();
}

void RuntimeProcess::SettleTerminated(
    ProcessTerminationCause cause, RuntimeEffects& services) {
  execution_state_ = ProcessExecutionState::kTerminated;
  termination_cause_ = cause;
  {
    // The frame is parked at its final suspend point (normal completion) or at
    // some blocking point (a kill), and holds the only copies of this
    // activation's automatic storage, so it is released with the terminal
    // state rather than pinned for as long as the node lives. Releasing it
    // destroys the frame, and with it every wait the frame held and its place
    // in any queue -- so a killed process, parked anywhere, is left unable to
    // resume. A branch this body spawned may still be running, which is why
    // the node itself stays (LRM 9.6.3).
    //
    // A thread that had handed itself to a called activation holds that frame
    // too, and it is the innermost one -- so it, not the body below it, is
    // what a wait target can still name.
    //
    // Releasing a frame runs the cleanups it holds open, leaving the disable
    // targets it is inside among them, and those are this process's to leave
    // whoever is running when it is killed.
    const ProcessExecutionGuard releasing(services, *this);
    nested_activations_.clear();
    coroutine_ = Coroutine<void>{};
    current_leaf_ = nullptr;
  }
  // Settling and waking what awaits it are one step, so no terminal path can
  // leave one of them parked forever (LRM 9.3.2, 9.7).
  services.WakeParkedOn(termination_, Change::Whole());
}

auto RuntimeProcess::ResumeWith(RuntimeEffects& effects, Activation* activation)
    -> bool {
  if (execution_state_ == ProcessExecutionState::kTerminated) {
    throw InternalError(
        "RuntimeProcess::ResumeWith: cannot resume terminated process");
  }
  if (execution_state_ == ProcessExecutionState::kRunning) {
    throw InternalError("RuntimeProcess::ResumeWith: reentrant resume");
  }
  // An activity spawned inside a target that was disabled before this activity
  // ever ran has no statement to leave from: it is terminated where it stands,
  // without running any of its body (LRM 9.6.2 -- everything enabled within the
  // target is terminated). Its frame has not begun, so releasing it here is the
  // whole termination.
  if (execution_state_ == ProcessExecutionState::kCreated &&
      OutermostInvalidatedTarget() != nullptr) {
    SettleTerminated(ProcessTerminationCause::kKilled, effects);
    return true;
  }
  execution_state_ = ProcessExecutionState::kRunning;
  {
    // Install the process and its owning scope as the ambient execution
    // identity for this resume. `ProcessExecutionGuard` publishes both
    // atomically -- LRM 9.5 process identity + LRM 21.2.1.5 `%m` scope
    // attribution -- and stacks via save-and-restore so a nested foreign
    // call that re-enters generated code cannot lose either identity on
    // the way back.
    const ProcessExecutionGuard resume_guard(effects, *this);
    // A leaf that blocked under a foreign call is resumed by driving its
    // vehicle, which re-enters the native stack and continues the coroutine
    // from within; a leaf blocked on the runtime's own stack is resumed
    // directly by a symmetric transfer into its frame.
    if (resume_target_ != nullptr) {
      // The foreign call returned: its exported task completed internally to
      // the vehicle, so what continues the process is the import frame that
      // entered the call, resumed directly now that no vehicle is in the way.
      // Otherwise an inner frame re-blocked and re-snapshotted the vehicle, so
      // the process stays waiting on that new wait.
      if (DriveForeignVehicle(*resume_target_)) {
        current_leaf_ = foreign_calls_.back().continuation;
        resume_target_ = nullptr;
        foreign_calls_.pop_back();
        current_leaf_->coroutine.resume();
      }
    } else {
      activation->coroutine.resume();
    }
  }
  if (!coroutine_.Done()) {
    execution_state_ = ProcessExecutionState::kWaiting;
    return false;
  }
  // The body has settled and its completion slot says how. This frame is the
  // activation's landing, so a departure that reached it was claimed by no
  // region and ends the process here, reported KILLED (LRM 9.6.2, 9.7);
  // anything else ran out its body, which LRM 9.7 reports FINISHED.
  //
  // The outcome is read before the frame is released, so a process reaches its
  // terminal state and frees its frame on the same path a successful one does.
  // A raised error is reported only afterwards: acting on it first would skip
  // the rest of this resumption -- the activations this termination wakes, the
  // enclosing `wait fork` condition -- so the simulation would hang rather than
  // end. A control effect needs no such report; it has arrived where it was
  // going.
  auto& promise = coroutine_.Handle().promise();
  const bool cancelled = promise.WasCancelled();
  std::exception_ptr raised = promise.TakeRaisedError();
  SettleTerminated(
      (cancelled || raised) ? ProcessTerminationCause::kKilled
                            : ProcessTerminationCause::kCompleted,
      effects);
  if (raised) {
    // The report is about this process, so it is made under this process's
    // identity even though its body has stopped: what a report says about where
    // in the design it was made is the scope the raising body belonged to.
    const ProcessExecutionGuard reporting(effects, *this);
    ReportRaisedError(effects, raised);
  }
  return true;
}

}  // namespace lyra::runtime
