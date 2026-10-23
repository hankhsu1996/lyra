#include "lyra/runtime/runtime_process.hpp"

#include <coroutine>
#include <exception>
#include <memory>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/foreign_execution.hpp"
#include "lyra/runtime/generated_call_scope.hpp"
#include "lyra/runtime/region.hpp"
#include "lyra/runtime/runtime_effects.hpp"
#include "lyra/runtime/wait.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(
    ExecutionContext context, Coroutine<void> coroutine, DrawRng rng)
    : context_(context),
      coroutine_(std::move(coroutine)),
      running_(rng),
      // Before the body runs, the top frame is the active leaf (what the engine
      // schedules to start the process); a wait moves the leaf inward.
      current_leaf_(coroutine_.Token()) {
  // Wire the promise's back-pointer so a frame can recover the RuntimeProcess
  // identity from itself, which is what a wait reads to find the process the
  // frame it parks belongs to.
  coroutine_.BindProcess(*this);
}

RuntimeProcess::~RuntimeProcess() {
  while (!foreign_calls_.empty()) {
    foreign_calls_.pop_back();
  }
}

auto RuntimeProcess::TopActivation() const -> Activation* {
  return coroutine_.Token();
}

auto RuntimeProcess::Parent() const -> RuntimeProcess* {
  return owned_by_ != nullptr ? owned_by_->Parent() : nullptr;
}

auto RuntimeProcess::ParkAt(RuntimeEffects& services, Awaiter& awaiter)
    -> bool {
  return Arrange(services, current_leaf_, awaiter, awaiter.Begin());
}

auto RuntimeProcess::Arrange(
    RuntimeEffects& services, Activation* leaf, Awaiter& awaiter,
    const Resumption& resumption) -> bool {
  const RegionSet& regions = leaf->Process().Context().regions;
  return std::visit(
      Overloaded{
          [](const WithoutStopping&) { return false; },
          [&](const OnAnOccurrence&) {
            leaf->ParkOn(awaiter);
            return true;
          },
          [&](const LaterInThisTimeStep& later) {
            leaf->ParkOn(awaiter);
            services.Schedule(services.Now(), regions[later.region], leaf);
            return true;
          },
          [&](const AtTime& at) {
            // A time already reached leaves nothing to wait for.
            if (at.when <= services.Now()) {
              return false;
            }
            leaf->ParkOn(awaiter);
            services.Schedule(at.when, regions.active, leaf);
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

void RuntimeProcess::DisableDescendants(RuntimeEffects& services) {
  children_.EndAll(services, nullptr);
}

void RuntimeProcess::Dismantle(RuntimeEffects& services) {
  TerminateSubtreeKilled(services);
  if (IsReleasable()) {
    Release();
  }
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
  // Off-path children are killed and dismantled synchronously; the one child on
  // the path down to `running` is kept and recursed into, so the chain that
  // owns `running` survives until `running` settles at its safe boundary.
  children_.EndAll(services, &running);
  if (this == &running) {
    RequestTermination(ProcessTerminationCause::kKilled);
    return;
  }
  SettleOrRequestKilled(services);
}

auto RuntimeProcess::IsSelfOrAncestorOf(const RuntimeProcess& other) const
    -> bool {
  for (const RuntimeProcess* node = &other; node != nullptr;
       node = node->Parent()) {
    if (node == this) {
      return true;
    }
  }
  return false;
}

auto RuntimeProcess::IsReleasable() const -> bool {
  return execution_state_ == ProcessExecutionState::kTerminated &&
         children_.Empty();
}

void RuntimeProcess::Release() {
  owned_place_.Unlink();
  owned_by_ = nullptr;
  const std::shared_ptr<RuntimeProcess> last = std::move(owned_place_.held);
}

void RuntimeProcess::ReleaseTerminatedLineage(RuntimeProcess& process) {
  RuntimeProcess* node = &process;
  while (node != nullptr && node->owned_by_ != nullptr &&
         node->IsReleasable()) {
    RuntimeProcess* parent = node->Parent();
    node->Release();
    node = parent;
  }
}

auto RuntimeProcess::DriveForeignVehicle(ForeignExecution& fe) -> bool {
  const ForeignProcessGuard guard(*this);
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

auto RuntimeProcess::EnterForeignExecution(std::unique_ptr<ForeignExecution> fe)
    -> bool {
  ForeignExecution& entered = *foreign_calls_.emplace_back(std::move(fe));
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
  services.WakeParkedOn(termination_, Change::Whole());
  if (owned_by_ == nullptr) {
    throw InternalError(
        "RuntimeProcess::SettleTerminated: a process that has just ended is "
        "kept by nothing -- please report this as a bug");
  }
  owned_by_->Ended(*this, services);
}

auto RuntimeProcess::Run(RuntimeEffects& effects, Activation* activation)
    -> bool {
  // Taken before the resume, since `activation` may be an enabled task's frame
  // that is destroyed as control returns up the enable chain.
  const std::shared_ptr<RuntimeProcess> process =
      activation->Process().shared_from_this();
  if (!process->ResumeWith(effects, activation)) {
    return false;
  }
  ReleaseTerminatedLineage(*process);
  return true;
}

void RuntimeProcess::RunUndispatched(
    RuntimeEffects& effects, Activation* activation) {
  const std::shared_ptr<RuntimeProcess> process =
      activation->Process().shared_from_this();
  if (!Run(effects, activation) ||
      process->TerminationCause() != ProcessTerminationCause::kKilled) {
    return;
  }
  // Nothing outside it names an execution run this way, so what ended it is an
  // error it met, already reported where it was received. It was met where the
  // executing process stands, so it stops that process the same way; an
  // occurrence the scheduler itself carries out stops nothing more.
  RuntimeProcess* const caller = effects.TryCurrentProcess();
  if (caller == nullptr) return;
  caller->RequestTermination(ProcessTerminationCause::kKilled);
  RaiseUnclaimableEffect();
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
    // A leaf that blocked under a foreign call is resumed by driving the
    // innermost call its thread has entered, which re-enters the native stack
    // and continues the coroutine from within; a leaf blocked on the runtime's
    // own stack is resumed directly by a symmetric transfer into its frame.
    if (HasLiveForeignCall()) {
      // A foreign call that returned goes on in the frame that made it, on the
      // stack that frame runs on -- the call before it, or, once the outermost
      // has returned, the runtime's own, where the frame is resumed directly. A
      // drive that does not return ended where a frame inside that call blocked
      // again.
      while (DriveForeignVehicle(*foreign_calls_.back())) {
        foreign_calls_.pop_back();
        if (!HasLiveForeignCall()) {
          current_leaf_->coroutine.resume();
          break;
        }
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
