#include "lyra/runtime/execution_context.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/runtime_process.hpp"

namespace lyra::runtime {

auto ExecutionContext::CurrentProcess() const -> RuntimeProcess& {
  if (current_ == nullptr) {
    throw InternalError("ExecutionContext: no process is executing");
  }
  return *current_;
}

}  // namespace lyra::runtime
