#pragma once

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class RuntimeScope;

class RuntimeProcess {
 public:
  RuntimeProcess(
      RuntimeScope& owner, ProcessKind kind, ProcessCoroutine coroutine);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  RuntimeProcess(RuntimeProcess&&) noexcept = default;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = default;
  ~RuntimeProcess() = default;

  auto Owner() -> RuntimeScope&;
  [[nodiscard]] auto Kind() const -> ProcessKind;
  auto Resume() -> ProcessRunResult;

 private:
  RuntimeScope* owner_ = nullptr;
  ProcessKind kind_;
  ProcessCoroutine coroutine_;
};

}  // namespace lyra::runtime
