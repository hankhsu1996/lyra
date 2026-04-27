#include "lyra/runtime/engine.hpp"

#include <memory>
#include <string>
#include <utility>

#include "lyra/runtime/bind_context.hpp"
#include "lyra/runtime/module.hpp"
#include "lyra/runtime/process.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"
#include "lyra/support/internal_error.hpp"

namespace lyra::runtime {

void Engine::BindRoot(std::string root_name, Module& top) {
  if (bound_) {
    throw support::InternalError("Engine::BindRoot called more than once");
  }
  bound_ = true;
  scopes_.push_back(
      std::make_unique<RuntimeScope>(
          nullptr, std::move(root_name), RuntimeScopeKind::kModule));
  RuntimeBindContext ctx(*this, *scopes_.front());
  top.Bind(ctx);
}

void Engine::AddProcess(
    RuntimeScope& owner, ProcessKind kind, Process process) {
  processes_.emplace_back(owner, kind, std::move(process));
}

auto Engine::CreateChildScope(
    RuntimeScope& parent, std::string name, RuntimeScopeKind kind)
    -> RuntimeScope& {
  scopes_.push_back(
      std::make_unique<RuntimeScope>(&parent, std::move(name), kind));
  RuntimeScope& child = *scopes_.back();
  parent.AddChild(child);
  return child;
}

auto Engine::Run() -> int {
  if (!bound_) {
    throw support::InternalError("Engine::Run called before BindRoot");
  }
  if (ran_) {
    throw support::InternalError("Engine::Run called more than once");
  }
  ran_ = true;
  for (auto& proc : processes_) {
    switch (proc.Kind()) {
      case ProcessKind::kInitial:
        active_queue_.push_back(&proc);
        break;
      case ProcessKind::kAlways:
      case ProcessKind::kAlwaysComb:
      case ProcessKind::kAlwaysFf:
      case ProcessKind::kFinal:
        throw support::InternalError(
            "Engine::Run: unsupported runtime ProcessKind in this cut");
    }
  }
  while (!active_queue_.empty()) {
    auto* proc = active_queue_.front();
    active_queue_.pop_front();
    proc->Run();
  }
  return 0;
}

}  // namespace lyra::runtime
