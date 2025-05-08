#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/execution_context.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

// Forward declarations
struct ScheduledEvent;
struct WaitingProcessInfo;

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
  explicit TriggerManager(ExecutionContext& context) : context_(context) {
  }

  // Register a process to wait on variable changes
  void RegisterWaitingProcess(
      const std::shared_ptr<lir::Process>& process, const std::string& variable,
      common::EdgeKind edge_kind, std::size_t block_index,
      std::size_t instruction_index);

  // Process variable changes and return processes that should be triggered
  auto CheckTriggers(const std::vector<std::string>& modified_variables)
      -> std::vector<ScheduledEvent>;

 private:
  // Check if a variable change should trigger based on edge kind
  static auto ShouldTrigger(
      const RuntimeValue& old_value, const RuntimeValue& new_value,
      common::EdgeKind edge_kind) -> bool;

  // Using the same typedefs from SimulationRunner
  using WaitMap = std::unordered_map<
      std::string,
      std::unordered_set<
          std::shared_ptr<lir::Process>, ProcessPtrHash, ProcessPtrEqual>>;
  using WaitSet = std::unordered_map<
      std::shared_ptr<lir::Process>, WaitingProcessInfo, ProcessPtrHash,
      ProcessPtrEqual>;

  WaitMap wait_map_;
  WaitSet wait_set_;
  std::vector<std::string> vars_to_remove_;
  std::reference_wrapper<ExecutionContext> context_;
};

}  // namespace lyra::interpreter
