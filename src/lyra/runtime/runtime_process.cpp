#include "lyra/runtime/runtime_process.hpp"

#include <utility>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::runtime {

RuntimeProcess::RuntimeProcess(
    RuntimeScope& owner, ProcessKind kind, Process process)
    : owner_(&owner), kind_(kind), process_(std::move(process)) {
}

auto RuntimeProcess::Owner() -> RuntimeScope& {
  return *owner_;
}

auto RuntimeProcess::Kind() const -> ProcessKind {
  return kind_;
}

void RuntimeProcess::Run() {
  process_.Resume();
  if (!process_.Done()) {
    throw support::InternalError(
        "RuntimeProcess::Run: process suspended without completing; this "
        "cut has no awaitables, so Done() must hold after Resume");
  }
}

}  // namespace lyra::runtime
