#pragma once

#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "lyra/runtime/bind_context.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"

namespace lyra::runtime {

class Module;

class Engine {
 public:
  void BindRoot(std::string root_name, Module& top);
  auto Run() -> int;

  void AddProcess(RuntimeScope& owner, ProcessKind kind, Process process);
  auto CreateChildScope(
      RuntimeScope& parent, std::string name, RuntimeScopeKind kind)
      -> RuntimeScope&;

 private:
  std::vector<std::unique_ptr<RuntimeScope>> scopes_;
  std::vector<RuntimeProcess> processes_;
  std::deque<RuntimeProcess*> active_queue_;
  bool bound_ = false;
  bool ran_ = false;
};

}  // namespace lyra::runtime
