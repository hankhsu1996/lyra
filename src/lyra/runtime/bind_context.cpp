#include "lyra/runtime/bind_context.hpp"

#include <string>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

RuntimeBindContext::RuntimeBindContext(
    RuntimeScope& scope, RuntimeServices& services)
    : scope_(&scope), services_(&services) {
}

auto RuntimeBindContext::CurrentScope() -> RuntimeScope& {
  if (scope_ == nullptr) {
    throw InternalError("RuntimeBindContext has no current scope");
  }
  return *scope_;
}

auto RuntimeBindContext::Services() -> RuntimeServices& {
  if (services_ == nullptr) {
    throw InternalError("RuntimeBindContext has no runtime services");
  }
  return *services_;
}

auto RuntimeBindContext::AddProcess(ProcessKind kind, Process process)
    -> RuntimeProcess& {
  return CurrentScope().AddProcess(kind, std::move(process));
}

auto RuntimeBindContext::CreateChildScope(
    std::string name, RuntimeScopeKind kind) -> RuntimeBindContext {
  RuntimeScope& child = CurrentScope().AddChildScope(std::move(name), kind);
  return {child, Services()};
}

}  // namespace lyra::runtime
