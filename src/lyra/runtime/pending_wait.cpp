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
  const PendingWait* wait = activation->pending_wait;
  if (wait != nullptr && wait->IsReportFlushPoint()) {
    activation->Process().FlushViolationReports();
  }
  activation->pending_wait = nullptr;
}

void PendingWait::CheckAbortOnResume() const {
  // A wait whose condition already held never suspended, so its execution never
  // lost control and nothing can have disabled a target under it.
  if (waiting_process_ != nullptr) {
    RaiseControlEffectIfDisabled(*waiting_process_);
  }
}

}  // namespace lyra::runtime
