#include "lyra/runtime/execution_context.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

auto ExecutionContext::CurrentProcess() const -> RuntimeProcess& {
  RuntimeProcess* process = TryCurrentProcess();
  if (process == nullptr) {
    throw InternalError("ExecutionContext: no process is executing");
  }
  return *process;
}

}  // namespace lyra::runtime
