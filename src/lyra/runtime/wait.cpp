#include "lyra/runtime/wait.hpp"

#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/registration.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

Wait::~Wait() = default;

void ConsumeWait(CoroutineHandle activation) {
  activation->RevokeRegistrations();
  if (activation->wait_is_report_flush_point) {
    activation->Process().FlushDeferredReports();
  }
  activation->wait.reset();
  activation->wait_is_report_flush_point = false;
}

}  // namespace lyra::runtime
