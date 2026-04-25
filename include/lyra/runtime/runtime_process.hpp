#pragma once

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"

namespace lyra::runtime {

class RuntimeScope;

class RuntimeProcess {
 public:
  RuntimeProcess(RuntimeScope& owner, ProcessKind kind, Process process);

  RuntimeProcess(const RuntimeProcess&) = delete;
  auto operator=(const RuntimeProcess&) -> RuntimeProcess& = delete;
  RuntimeProcess(RuntimeProcess&&) noexcept = default;
  auto operator=(RuntimeProcess&&) noexcept -> RuntimeProcess& = default;
  ~RuntimeProcess() = default;

  auto Owner() -> RuntimeScope&;
  [[nodiscard]] auto Kind() const -> ProcessKind;
  void Run();

 private:
  RuntimeScope* owner_ = nullptr;
  ProcessKind kind_;
  Process process_;
};

}  // namespace lyra::runtime
