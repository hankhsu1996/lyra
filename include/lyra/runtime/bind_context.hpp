#pragma once

#include <string>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"

namespace lyra::runtime {

class Engine;
class RuntimeScope;

class RuntimeBindContext {
 public:
  RuntimeBindContext(Engine& engine, RuntimeScope& scope);

  auto CurrentScope() -> RuntimeScope&;
  void AddProcess(ProcessKind kind, Process process);
  auto CreateChildScope(std::string name, RuntimeScopeKind kind)
      -> RuntimeBindContext;

 private:
  Engine* engine_ = nullptr;
  RuntimeScope* scope_ = nullptr;
};

}  // namespace lyra::runtime
