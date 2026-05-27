#pragma once

#include <cstddef>
#include <cstdint>
#include <deque>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/runtime/diagnostic.hpp"
#include "lyra/runtime/runtime_scope.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/stream_dispatcher.hpp"
#include "lyra/runtime/wait_request.hpp"

namespace lyra::runtime {

class Module;
class Observable;
class RuntimeEvent;
class RuntimeProcess;
enum class EdgeTransition : std::uint8_t;

enum class SchedulerPhase : std::uint8_t {
  kIdle,
  kActive,
  kInactive,
  kFlushUpdates,
  kCommitNba,
  kObserved,
  kReactive,
  kPostponed,
  kAdvanceTime,
};

struct EngineOptions {
  StreamDispatcher::StreamSink stream_sink;
  DiagnosticDispatcher::DiagnosticSink diagnostic_sink;
};

[[nodiscard]] auto DefaultEngineOptions() -> EngineOptions;

class Engine {
 public:
  Engine();
  explicit Engine(EngineOptions options);

  // services_ holds a pointer into stream_; copying or moving Engine would
  // dangle that pointer.
  Engine(const Engine&) = delete;
  auto operator=(const Engine&) -> Engine& = delete;
  Engine(Engine&&) = delete;
  auto operator=(Engine&&) -> Engine& = delete;
  ~Engine() = default;

  void BindRoot(std::string root_name, Module& top);
  auto Run() -> int;

  auto Stream() -> StreamDispatcher& {
    return stream_;
  }

  auto Services() -> RuntimeServices& {
    return services_;
  }

  void SubmitNba(std::function<void()> closure);

  void TriggerValueChange(Observable& observable, EdgeTransition transition);

 private:
  struct PostponedWorkItem {};

  struct SchedulerQueues {
    std::deque<RuntimeProcess*> active;
    std::deque<RuntimeProcess*> inactive;
    std::vector<RuntimeProcess*> next_delta;
    std::vector<std::function<void()>> nba;
    std::vector<PostponedWorkItem> postponed;
    std::map<SimTime, std::vector<RuntimeProcess*>> delayed;
    std::vector<RuntimeProcess*> finals;
  };

  static constexpr std::size_t kMaxCurrentTimeIterations = 10000;
  static constexpr std::size_t kMaxDeltaCyclesPerTimeSlot = 10000;

  void EnsureReadyToRun();
  void RegisterProcesses();
  void RunProcess(RuntimeProcess& process);
  void DrainRunnableQueue(std::deque<RuntimeProcess*>& queue);

  void ExecuteCurrentTimeSlot();
  void ExecuteActiveRegion();
  void ExecuteInactiveRegion();
  void FlushRuntimeUpdates();
  void ExecuteNbaRegion();
  void ExecuteObservedRegion();
  void ExecuteReactiveRegion();
  void ExecutePostponedRegion();
  void ExecuteFinalProcesses();

  void AdvanceToNextTime();
  void AdvanceDeltaCycle();
  void PromoteNextDeltaToActive();

  [[nodiscard]] auto HasCurrentTimeWork() const -> bool;
  [[nodiscard]] auto HasFutureTimedWork() const -> bool;
  [[nodiscard]] auto HasScheduledWork() const -> bool;
  [[nodiscard]] auto HasNextDeltaWork() const -> bool;
  [[nodiscard]] auto IsRunnablePhase() const -> bool;

  void ScheduleWait(RuntimeProcess& process, WaitRequest wait);
  void ScheduleDelayWait(RuntimeProcess& process, DelayWait wait);
  static void ScheduleEventWait(RuntimeProcess& process, EventWait wait);
  static void ScheduleValueChangeWait(
      RuntimeProcess& process, ValueChangeWait wait);

  void ScheduleActive(RuntimeProcess& process);
  void ScheduleInactive(RuntimeProcess& process);
  void ScheduleNextDelta(RuntimeProcess& process);
  void ScheduleDelayed(SimTime wake_time, RuntimeProcess& process);

  void TriggerEvent(RuntimeEvent& event);

  [[nodiscard]] static auto CheckedAdd(SimTime base, SimDuration delta)
      -> SimTime;

  StreamDispatcher stream_;
  DiagnosticDispatcher diagnostic_;
  RuntimeServices services_{stream_, diagnostic_, *this};
  std::unique_ptr<RuntimeScope> root_;
  SchedulerQueues queues_;
  std::vector<Module*> registered_modules_;
  SimTime now_ = 0;
  SchedulerPhase phase_ = SchedulerPhase::kIdle;
  std::size_t current_delta_ = 0;
  bool bound_ = false;
  bool ran_ = false;
  bool finished_ = false;
};

}  // namespace lyra::runtime
