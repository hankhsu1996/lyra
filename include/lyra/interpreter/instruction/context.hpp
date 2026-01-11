#pragma once

#include <functional>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

class InstanceContext;
class ProcessEffect;
class ProcessFrame;
class SimulationContext;
class TempTable;

/// Context for instruction execution.
/// Bundles all dependencies and provides operand accessor methods.
/// Similar to SystemCallContext but includes eval operations.
class InstructionContext {
 public:
  InstructionContext(
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
  void StoreVariable(
      const lir::Operand& operand, const RuntimeValue& value,
      bool is_non_blocking);

  /// Store to hierarchical target: traverse instance path and store to target
  void StoreHierarchical(
      const std::vector<common::SymbolRef>& instances, common::SymbolRef target,
      const RuntimeValue& value, bool is_non_blocking);

  /// Load from hierarchical target: traverse instance path and load from target
  [[nodiscard]] auto LoadHierarchical(
      const std::vector<common::SymbolRef>& instances,
      common::SymbolRef target) const -> RuntimeValue;

  /// Get value from any operand type (temp, variable, or literal).
  [[nodiscard]] auto GetOperandValue(const lir::Operand& operand) const
      -> RuntimeValue;

  /// Resolve symbol through port bindings.
  [[nodiscard]] auto ResolveBinding(common::SymbolRef symbol) const
      -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>>;

  // Operation evaluation helpers

  /// Evaluate unary operation and write result to temp table.
  auto EvalUnaryOp(
      const lir::Operand& operand, lir::TempRef result,
      const std::function<RuntimeValue(RuntimeValue)>& op) -> InstructionResult;

  /// Evaluate binary operation and write result to temp table.
  auto EvalBinaryOp(
      const lir::Operand& lhs, const lir::Operand& rhs, lir::TempRef result,
      const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
      -> InstructionResult;

  /// Write value to temp table.
  void WriteTemp(lir::TempRef result, RuntimeValue value);

 private:
  SimulationContext& simulation_context_;
  ProcessFrame& frame_;
  ProcessEffect& effect_;
  TempTable& temp_table_;
  std::shared_ptr<InstanceContext> instance_context_;
};

}  // namespace lyra::interpreter
