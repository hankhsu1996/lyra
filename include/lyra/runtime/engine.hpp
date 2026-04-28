#pragma once

#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "lyra/runtime/bind_context.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"

namespace lyra::runtime {

class Module;

struct EngineOptions {
  OutputDispatcher::OutputSink output_sink;
};

[[nodiscard]] auto DefaultEngineOptions() -> EngineOptions;

class Engine {
 public:
  Engine();
  explicit Engine(EngineOptions options);

  void BindRoot(std::string root_name, Module& top);
  auto Run() -> int;

  void AddProcess(RuntimeScope& owner, ProcessKind kind, Process process);
  auto CreateChildScope(
      RuntimeScope& parent, std::string name, RuntimeScopeKind kind)
      -> RuntimeScope&;

  auto Output() -> OutputDispatcher& {
    return output_;
  }

 private:
  OutputDispatcher output_;
  std::vector<std::unique_ptr<RuntimeScope>> scopes_;
  std::vector<RuntimeProcess> processes_;
  std::deque<RuntimeProcess*> active_queue_;
  bool bound_ = false;
  bool ran_ = false;
};

}  // namespace lyra::runtime
