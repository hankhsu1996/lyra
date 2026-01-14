#pragma once

#include <cstddef>
#include <functional>
#include <memory>
#include <utility>
#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/address.hpp"
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

/// Execution scope for temp access.
/// Captures whether we're in process or function context.
/// Computed once at instruction start, eliminates repeated call_stack.empty()
/// branching.
///
/// Note: This is a cleanup, not a design fix. Process temps and function temps
/// still share the same vector API when they're semantically different things.
/// The real fix would be typed ProcessTempId/FunctionTempId that can't be
/// mixed.
struct ExecutionScope {
  const std::vector<lir::TempMeta>* temps;  // Points to process or function
                                            // temps
  TempTable* temp_table;                    // Points to appropriate table
};

/// Context for instruction execution.
/// Bundles all dependencies and provides operand accessor methods.
/// Similar to SystemCallContext but includes eval operations.
class InstructionContext {
 public:
  InstructionContext(
      SimulationContext& simulation_context, ProcessFrame& frame,
      ProcessEffect& effect, ExecutionScope scope,
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
    return *scope_.temp_table;
  }
  [[nodiscard]] auto GetHierarchyContext() const
      -> const std::shared_ptr<HierarchyContext>& {
    return hierarchy_context_;
  }

  // Operand access methods

  /// Read a temporary value from the temp table.
  [[nodiscard]] auto GetTemp(lir::TempRef temp) const -> RuntimeValue;

  /// Get the type of a temp from per-unit metadata.
  /// Looks up in function temps if in call, otherwise process temps.
  [[nodiscard]] auto GetTempType(lir::TempRef temp) const
      -> const common::Type&;

  /// Read a variable by symbol (checks function locals, process locals, then
  /// flat storage with port binding resolution).
  [[nodiscard]] auto ReadVariable(common::SymbolId symbol) const
      -> RuntimeValue;

  /// Store a variable value by symbol with proper scoping and effect tracking.
  void StoreVariable(
      common::SymbolId symbol, const RuntimeValue& value, bool is_non_blocking);

  /// Resolve address and read value (flat path traversal).
  [[nodiscard]] auto ResolveForRead(const Address& addr) const -> RuntimeValue;

  /// Resolve address and write value (flat path traversal).
  void ResolveForWrite(
      const Address& addr, const RuntimeValue& value, bool is_non_blocking);

  /// Resolve address and write slice (read-modify-write for bit fields).
  void ResolveForWriteSlice(
      const Address& addr, const RuntimeValue& value, size_t bit_offset,
      size_t width, bool is_non_blocking);

  /// Allocate permanent storage and return its ID.
  /// Used by kAllocate to create addressable storage that persists until
  /// ProcessFrame destruction.
  auto AllocatePermanent(RuntimeValue initial) -> uint64_t;

  /// Get value from any operand type (temp, variable, or literal).
  [[nodiscard]] auto GetOperandValue(const lir::Operand& operand) const
      -> RuntimeValue;

  /// Resolve symbol through port bindings (output port -> parent signal).
  /// Returns (resolved_symbol, target_instance) for flat storage lookup.
  [[nodiscard]] auto ResolveBinding(common::SymbolId symbol) const
      -> std::pair<common::SymbolId, std::shared_ptr<HierarchyContext>>;

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
  ExecutionScope scope_;
  std::shared_ptr<HierarchyContext> hierarchy_context_;
};

}  // namespace lyra::interpreter
