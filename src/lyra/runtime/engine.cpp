#include "lyra/runtime/engine.hpp"

#include <iostream>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/time.hpp"
#include "lyra/runtime/bind_context.hpp"
#include "lyra/runtime/module.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/process_kind.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_scope_kind.hpp"
#include "lyra/runtime/runtime_traversal.hpp"
#include "lyra/runtime/wait_request.hpp"

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
  while (!active_queue_.empty() || !inactive_queue_.empty() ||
         !delay_queue_.empty()) {
    while (!active_queue_.empty() || !inactive_queue_.empty()) {
      DrainActiveQueue();
      if (active_queue_.empty() && !inactive_queue_.empty()) {
        MoveInactiveToActive();
      }
    }
    if (!delay_queue_.empty()) {
      MoveNextDelayedTimeToActive();
    }
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

void Engine::DrainActiveQueue() {
  while (!active_queue_.empty()) {
    auto* proc = active_queue_.front();
    active_queue_.pop_front();
    auto result = proc->Resume();
    if (result.IsCompleted()) {
      continue;
    }
    auto wait = result.TakeWait();
    if (auto* delay = std::get_if<DelayRequest>(&wait)) {
      ScheduleDelay(*proc, delay->duration);
      continue;
    }
    throw InternalError("Engine::DrainActiveQueue: unsupported wait request");
  }
}

void Engine::MoveInactiveToActive() {
  while (!inactive_queue_.empty()) {
    active_queue_.push_back(inactive_queue_.front());
    inactive_queue_.pop_front();
  }
}

void Engine::MoveNextDelayedTimeToActive() {
  auto it = delay_queue_.begin();
  if (it == delay_queue_.end()) {
    return;
  }
  now_ = it->first;
  for (auto* proc : it->second) {
    active_queue_.push_back(proc);
  }
  delay_queue_.erase(it);
}

void Engine::ScheduleDelay(RuntimeProcess& proc, SimDuration duration) {
  if (duration == 0) {
    inactive_queue_.push_back(&proc);
    return;
  }
  if (duration > std::numeric_limits<SimTime>::max() - now_) {
    throw InternalError("Engine::ScheduleDelay: wake time overflow");
  }
  delay_queue_[now_ + duration].push_back(&proc);
}

}  // namespace lyra::runtime
