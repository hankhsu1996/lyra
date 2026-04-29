#pragma once

#include <string>

#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"

namespace lyra::runtime {

class RuntimeProcess;
class RuntimeScope;
class RuntimeServices;

class RuntimeBindContext {
 public:
  RuntimeBindContext(RuntimeScope& scope, RuntimeServices& services);

  auto CurrentScope() -> RuntimeScope&;
  auto Services() -> RuntimeServices&;

  auto AddProcess(ProcessKind kind, ProcessCoroutine coroutine)
      -> RuntimeProcess&;
  auto CreateChildScope(std::string name, RuntimeScopeKind kind)
      -> RuntimeBindContext;

 private:
  RuntimeScope* scope_ = nullptr;
  RuntimeServices* services_ = nullptr;
};

}  // namespace lyra::runtime
