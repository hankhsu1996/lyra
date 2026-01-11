#pragma once

#include <memory>
#include <utility>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

class InstanceContext;
class ProcessEffect;
class ProcessFrame;
class SimulationContext;
class TempTable;

/// Context for system call execution.
/// Bundles all dependencies and provides operand accessor methods.
class SystemCallContext {
 public:
  SystemCallContext(
      SimulationContext& simulation_context, ProcessFrame& frame,
      ProcessEffect& effect, TempTable& temp_table,
      std::shared_ptr<InstanceContext> instance_context);

  // Accessors for simulation state
  [[nodiscard]] auto GetSimulationContext() -> SimulationContext& {
    return simulation_context_;
  }
  [[nodiscard]] auto GetSimulationContext() const -> const SimulationContext& {
    return simulation_context_;
  }
  [[nodiscard]] auto GetFrame() -> ProcessFrame& {
    return frame_;
  }
  [[nodiscard]] auto GetEffect() -> ProcessEffect& {
    return effect_;
  }
  [[nodiscard]] auto GetTempTable() -> TempTable& {
    return temp_table_;
  }
  [[nodiscard]] auto GetInstanceContext() const
      -> const std::shared_ptr<InstanceContext>& {
    return instance_context_;
  }

  // Operand access methods

  /// Read a temporary value from the temp table.
  [[nodiscard]] auto GetTemp(const lir::Operand& operand) const -> RuntimeValue;

  /// Read a variable (checks function locals, process locals, instance, global)
  [[nodiscard]] auto ReadVariable(const lir::Operand& operand) const
      -> RuntimeValue;

  /// Store a variable value with proper scoping and effect tracking.
  void StoreVariable(const lir::Operand& operand, const RuntimeValue& value);

  /// Get value from any operand type (temp, variable, or literal).
  [[nodiscard]] auto GetOperandValue(const lir::Operand& operand) const
      -> RuntimeValue;

  /// Resolve symbol through port bindings.
  [[nodiscard]] auto ResolveBinding(common::SymbolRef symbol) const
      -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>>;

 private:
  SimulationContext& simulation_context_;
  ProcessFrame& frame_;
  ProcessEffect& effect_;
  TempTable& temp_table_;
  std::shared_ptr<InstanceContext> instance_context_;
};

}  // namespace lyra::interpreter
