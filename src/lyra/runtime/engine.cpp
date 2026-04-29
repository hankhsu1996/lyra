#include "lyra/runtime/engine.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/bind_context.hpp"
#include "lyra/runtime/module.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"
#include "lyra/runtime/runtime_traversal.hpp"

namespace lyra::runtime {

auto DefaultEngineOptions() -> EngineOptions {
  return EngineOptions{
      .output_sink = [](std::string_view text) { std::cout << text; }};
}

Engine::Engine() : Engine(DefaultEngineOptions()) {
}

Engine::Engine(EngineOptions options)
    : output_(std::move(options.output_sink)) {
}

void Engine::BindRoot(std::string root_name, Module& top) {
  if (bound_) {
    throw InternalError("Engine::BindRoot called more than once");
  }
  bound_ = true;
  root_ = std::make_unique<RuntimeScope>(
      nullptr, std::move(root_name), RuntimeScopeKind::kModuleInstance);
  RuntimeBindContext ctx(*root_, services_);
  top.Bind(ctx);
}

auto Engine::Run() -> int {
  if (!bound_) {
    throw InternalError("Engine::Run called before BindRoot");
  }
  if (ran_) {
    throw InternalError("Engine::Run called more than once");
  }
  ran_ = true;
  EnqueueInitialProcesses(*root_);
  while (!active_queue_.empty()) {
    auto* proc = active_queue_.front();
    active_queue_.pop_front();
    proc->Run();
  }
  output_.Drain();
  return 0;
}

void Engine::EnqueueInitialProcesses(RuntimeScope& root) {
  WalkScopePreOrder(root, [this](RuntimeScope& scope) {
    scope.ForEachProcess([this](RuntimeProcess& process) {
      switch (process.Kind()) {
        case ProcessKind::kInitial:
          active_queue_.push_back(&process);
          break;
        case ProcessKind::kAlways:
        case ProcessKind::kAlwaysComb:
        case ProcessKind::kAlwaysFf:
        case ProcessKind::kFinal:
          throw InternalError(
              "Engine::Run: unsupported runtime ProcessKind in this cut");
      }
    });
  });
}

}  // namespace lyra::runtime
