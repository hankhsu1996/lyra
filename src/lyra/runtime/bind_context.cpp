#include "lyra/runtime/bind_context.hpp"

#include <utility>

#include "lyra/runtime/engine.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_scope.hpp"

namespace lyra::runtime {

RuntimeBindContext::RuntimeBindContext(Engine& engine, RuntimeScope& scope)
    : engine_(&engine), scope_(&scope) {
}

auto RuntimeBindContext::CurrentScope() -> RuntimeScope& {
  return *scope_;
}

void RuntimeBindContext::AddProcess(ProcessKind kind, Process process) {
  engine_->AddProcess(*scope_, kind, std::move(process));
}

auto RuntimeBindContext::CreateChildScope(
    std::string name, RuntimeScopeKind kind) -> RuntimeBindContext {
  RuntimeScope& child =
      engine_->CreateChildScope(*scope_, std::move(name), kind);
  return {*engine_, child};
}

}  // namespace lyra::runtime
