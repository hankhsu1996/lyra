#pragma once

#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

// Forward declarations
struct ScheduledEvent;

// Info about a waiting process including its instance context
struct WaitingProcessInfo {
  std::shared_ptr<InstanceContext> process_instance;  // Where process runs
  std::shared_ptr<InstanceContext> watch_instance;    // Where variable lives
  std::size_t block_index;
  std::size_t instruction_index;
  common::EdgeKind edge_kind;
};

// Composite key for (process, instance) pairs - used for triggering
struct ProcessInstanceKey {
  std::shared_ptr<lir::Process> process;
  std::shared_ptr<InstanceContext> instance;

  auto operator==(const ProcessInstanceKey& other) const -> bool {
    return process.get() == other.process.get() &&
           instance.get() == other.instance.get();
  }
};

struct ProcessInstanceKeyHash {
  auto operator()(const ProcessInstanceKey& key) const -> std::size_t {
    auto h1 = std::hash<lir::Process*>{}(key.process.get());
    auto h2 = std::hash<InstanceContext*>{}(key.instance.get());
    return h1 ^ (h2 << 1);
  }
};

// Composite key for (process, instance, variable) - used for storing edge kind
// A process can wait on multiple variables with different edge kinds
// (e.g., @(posedge clk or negedge rst))
struct ProcessInstanceVarKey {
  std::shared_ptr<lir::Process> process;
  std::shared_ptr<InstanceContext> instance;
  SymbolRef variable;

  auto operator==(const ProcessInstanceVarKey& other) const -> bool {
    return process.get() == other.process.get() &&
           instance.get() == other.instance.get() && variable == other.variable;
  }
};

struct ProcessInstanceVarKeyHash {
  auto operator()(const ProcessInstanceVarKey& key) const -> std::size_t {
    auto h1 = std::hash<lir::Process*>{}(key.process.get());
    auto h2 = std::hash<InstanceContext*>{}(key.instance.get());
    auto h3 = std::hash<SymbolRef>{}(key.variable);
    return h1 ^ (h2 << 1) ^ (h3 << 2);
  }
};

// Process pointer hash and equality for unordered containers
struct ProcessPtrHash {
  auto operator()(const std::shared_ptr<lir::Process>& ptr) const
      -> std::size_t {
    return std::hash<lir::Process*>{}(ptr.get());
  }
};

struct ProcessPtrEqual {
  auto operator()(
      const std::shared_ptr<lir::Process>& lhs,
      const std::shared_ptr<lir::Process>& rhs) const -> bool {
    return lhs.get() == rhs.get();
  }
};

// Manages process waiting and triggering based on variable changes
class TriggerManager {
 public:
  explicit TriggerManager(SimulationContext& context) : context_(context) {
  }

  // Register a process to wait on variable changes
  // process_instance: where the process runs (for resumption)
  // watch_instance: where the variable lives (for trigger detection)
  void RegisterWaitingProcess(
      const std::shared_ptr<lir::Process>& process,
      const std::shared_ptr<InstanceContext>& process_instance,
      const std::shared_ptr<InstanceContext>& watch_instance,
      const SymbolRef& variable, common::EdgeKind edge_kind,
      std::size_t block_index, std::size_t instruction_index);

  // Process variable changes and return processes that should be triggered
  auto CheckTriggers(const std::vector<ModifiedVariable>& modified_variables)
      -> std::vector<ScheduledEvent>;

 private:
  // Check if a variable change should trigger based on edge kind
  static auto ShouldTrigger(
      const RuntimeValue& old_value, const RuntimeValue& new_value,
      common::EdgeKind edge_kind) -> bool;

  // WaitMap: variable -> set of (process, instance) keys waiting on it
  using WaitMap = std::unordered_map<
      SymbolRef,
      std::unordered_set<ProcessInstanceKey, ProcessInstanceKeyHash>>;
  // WaitSet: (process, instance, variable) -> waiting info
  // Each (process, instance, variable) tuple has its own edge kind
  using WaitSet = std::unordered_map<
      ProcessInstanceVarKey, WaitingProcessInfo, ProcessInstanceVarKeyHash>;

  WaitMap wait_map_;
  WaitSet wait_set_;
  std::vector<SymbolRef> vars_to_remove_;
  std::reference_wrapper<SimulationContext> context_;
};

}  // namespace lyra::interpreter
