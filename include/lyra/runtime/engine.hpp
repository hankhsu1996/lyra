#pragma once

#include <deque>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/output_sink.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_services.hpp"

namespace lyra::runtime {

class Module;
class RuntimeProcess;

struct EngineOptions {
  OutputDispatcher::OutputSink output_sink;
};

[[nodiscard]] auto DefaultEngineOptions() -> EngineOptions;

class Engine {
 public:
  Engine();
  explicit Engine(EngineOptions options);

  // services_ holds a pointer into output_; copying or moving Engine would
  // dangle that pointer.
  Engine(const Engine&) = delete;
  auto operator=(const Engine&) -> Engine& = delete;
  Engine(Engine&&) = delete;
  auto operator=(Engine&&) -> Engine& = delete;
  ~Engine() = default;

  void BindRoot(std::string root_name, Module& top);
  auto Run() -> int;

  auto Output() -> OutputDispatcher& {
    return output_;
  }

  auto Services() -> RuntimeServices& {
    return services_;
  }

 private:
  void EnqueueInitialProcesses(RuntimeScope& root);
  void DrainActiveQueue();
  void MoveInactiveToActive();
  void MoveNextDelayedTimeToActive();
  void ScheduleDelay(RuntimeProcess& proc, SimDuration duration);

  OutputDispatcher output_;
  RuntimeServices services_{output_};
  std::unique_ptr<RuntimeScope> root_;
  SimTime now_ = 0;
  std::deque<RuntimeProcess*> active_queue_;
  std::deque<RuntimeProcess*> inactive_queue_;
  std::map<SimTime, std::vector<RuntimeProcess*>> delay_queue_;
  bool bound_ = false;
  bool ran_ = false;
};

}  // namespace lyra::runtime
