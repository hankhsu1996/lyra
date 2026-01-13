#pragma once

#include <cstddef>
#include <functional>
#include <memory>
#include <utility>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/hierarchy_context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

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
      std::shared_ptr<HierarchyContext> hierarchy_context);

  // Accessors for simulation state
  [[nodiscard]] auto GetSimulationContext() -> SimulationContext& {
    return *simulation_context_;
  }
  [[nodiscard]] auto GetSimulationContext() const -> const SimulationContext& {
    return *simulation_context_;
  }
  [[nodiscard]] auto GetFrame() -> ProcessFrame& {
    return *frame_;
  }
  [[nodiscard]] auto GetEffect() -> ProcessEffect& {
    return *effect_;
  }
  [[nodiscard]] auto GetTempTable() -> TempTable& {
    return *temp_table_;
  }
  [[nodiscard]] auto GetHierarchyContext() const
      -> const std::shared_ptr<HierarchyContext>& {
    return hierarchy_context_;
  }

  // Operand access methods

  /// Read a temporary value from the temp table.
  [[nodiscard]] auto GetTemp(lir::TempRef temp) const -> RuntimeValue;

  /// Read a variable by symbol (checks function locals, process locals, then
  /// flat storage with port binding resolution).
  [[nodiscard]] auto ReadVariable(common::SymbolRef symbol) const
      -> RuntimeValue;

  /// Store a variable value by symbol with proper scoping and effect tracking.
  void StoreVariable(
      common::SymbolRef symbol, const RuntimeValue& value,
      bool is_non_blocking);

  /// Read value through a pointer (dereference).
  [[nodiscard]] auto ReadPointer(const PointerValue& ptr) const -> RuntimeValue;

  /// Write value through a pointer.
  void WritePointer(
      const PointerValue& ptr, const RuntimeValue& value, bool is_non_blocking);

  /// Allocate anonymous storage and return its ID.
  /// Used by kAllocate to create addressable temporary storage.
  auto AllocateAnonymous(RuntimeValue initial) -> uint64_t;

  /// Get value from any operand type (temp, variable, or literal).
  [[nodiscard]] auto GetOperandValue(const lir::Operand& operand) const
      -> RuntimeValue;

  /// Resolve symbol through port bindings (output port → parent signal).
  /// Returns (resolved_symbol, target_instance) for flat storage lookup.
  [[nodiscard]] auto ResolveBinding(common::SymbolRef symbol) const
      -> std::pair<common::SymbolRef, std::shared_ptr<HierarchyContext>>;

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
  SimulationContext* simulation_context_;
  ProcessFrame* frame_;
  ProcessEffect* effect_;
  TempTable* temp_table_;
  std::shared_ptr<HierarchyContext> hierarchy_context_;
};

}  // namespace lyra::interpreter
