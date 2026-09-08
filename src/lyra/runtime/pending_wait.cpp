#include "lyra/runtime/pending_wait.hpp"

#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

void PendingWait::BlockOn(CoroutineHandle leaf) {
  waiting_process_ = leaf->process;
  waiting_process_->BlockLeaf(leaf, this);
}

void ConsumeWait(CoroutineHandle activation) {
  activation->RevokeRegistrations();
  // Both backends record the parked wait's flush-point status on the frame
  // where they register the wait, so resuming from one clears the process's
  // deferred report queue (LRM 16.4.2, 12.4.2.1) without the resume having to
  // know how the wait was realized.
  if (activation->wait_is_report_flush_point) {
    activation->Process().FlushDeferredReports();
  }
  activation->pending_wait = nullptr;
  activation->wait_is_report_flush_point = false;
}

void PendingWait::CheckAbortOnResume() const {
  // A wait whose condition already held never suspended, so its execution never
  // lost control and nothing can have disabled a target under it.
  if (waiting_process_ != nullptr) {
    RaiseControlEffectIfDisabled(*waiting_process_);
  }
}

}  // namespace lyra::runtime
