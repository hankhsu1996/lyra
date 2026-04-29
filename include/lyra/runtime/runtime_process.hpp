#pragma once

#include <cstdint>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class RuntimeScope;

enum class ProcessState : std::uint8_t {
  kCreated,
  kRunning,
  kWaiting,
  kCompleted,
};

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
  [[nodiscard]] auto State() const -> ProcessState {
    return state_;
  }
  auto Resume() -> ProcessRunResult;

 private:
  RuntimeScope* owner_ = nullptr;
  ProcessKind kind_;
  ProcessCoroutine coroutine_;
  ProcessState state_ = ProcessState::kCreated;
};

}  // namespace lyra::runtime
