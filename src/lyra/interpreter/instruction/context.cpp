#include "lyra/interpreter/instruction/context.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

InstructionContext::InstructionContext(
    SimulationContext& simulation_context, ProcessFrame& frame,
    ProcessEffect& effect, TempTable& temp_table,
    std::shared_ptr<HierarchyContext> hierarchy_context)
    : simulation_context_(&simulation_context),
      frame_(&frame),
      effect_(&effect),
      temp_table_(&temp_table),
      hierarchy_context_(std::move(hierarchy_context)) {
}

auto InstructionContext::ResolveBinding(common::SymbolRef symbol) const
    -> std::pair<common::SymbolRef, std::shared_ptr<HierarchyContext>> {
  if (hierarchy_context_ != nullptr) {
    return hierarchy_context_->ResolveBinding(symbol);
  }
  return {symbol, nullptr};
}

auto InstructionContext::GetTemp(lir::TempRef temp) const -> RuntimeValue {
  return temp_table_->Read(temp);
}

auto InstructionContext::ReadVariable(const lir::Operand& operand) const
    -> RuntimeValue {
  assert(operand.IsVariable());
  const auto* symbol = std::get<lir::SymbolRef>(operand.value);

  // Check function-local variables first (parameters and locals)
  if (!frame_->call_stack.empty()) {
    const auto& call_frame = frame_->call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      return it->second;
    }
  }

  // Check process-local next
  if (frame_->variable_table.Exists(symbol)) {
    return frame_->variable_table.Read(symbol);
  }

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);

  // Read from flat storage (either resolved target or original symbol)
  const auto* actual_symbol =
      (target_instance != nullptr) ? target_symbol : symbol;
  return simulation_context_->variable_store.Read(actual_symbol);
}

void InstructionContext::StoreVariable(
    const lir::Operand& operand, const RuntimeValue& value,
    bool is_non_blocking) {
  assert(operand.IsVariable());
  const auto* symbol = std::get<lir::SymbolRef>(operand.value);

  // Deep copy arrays for value semantics
  const RuntimeValue actual_value = value.DeepCopy();

  // Check function-local variables first (parameters and locals)
  if (!frame_->call_stack.empty()) {
    auto& call_frame = frame_->call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      it->second = actual_value;
      return;
    }
  }

  // Check process-local next
  if (frame_->variable_table.Exists(symbol)) {
    frame_->variable_table.Write(symbol, actual_value);
    return;
  }

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);
  const auto* actual_symbol =
      (target_instance != nullptr) ? target_symbol : symbol;

  // Write to flat storage
  if (!is_non_blocking) {
    simulation_context_->variable_store.Write(actual_symbol, actual_value);
    effect_->RecordVariableModification(actual_symbol);
  } else {
    effect_->RecordNbaAction(
        {.symbol = actual_symbol,
         .value = actual_value,
         .array_index = std::nullopt});
  }
}

auto InstructionContext::ReadPointer(const PointerValue& ptr) const
    -> RuntimeValue {
  // Phase B: Only VarPointer is supported
  assert(ptr.IsVar() && "Only VarPointer supported in Phase B");
  const auto* symbol = ptr.AsVar().symbol;

  // Check function-local variables first (parameters and locals)
  if (!frame_->call_stack.empty()) {
    const auto& call_frame = frame_->call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      return it->second;
    }
  }

  // Check process-local next
  if (frame_->variable_table.Exists(symbol)) {
    return frame_->variable_table.Read(symbol);
  }

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);

  // Read from flat storage (either resolved target or original symbol)
  const auto* actual_symbol =
      (target_instance != nullptr) ? target_symbol : symbol;
  return simulation_context_->variable_store.Read(actual_symbol);
}

void InstructionContext::WritePointer(
    const PointerValue& ptr, const RuntimeValue& value, bool is_non_blocking) {
  // Phase B: Only VarPointer is supported
  assert(ptr.IsVar() && "Only VarPointer supported in Phase B");
  const auto* symbol = ptr.AsVar().symbol;

  // Deep copy arrays for value semantics
  const RuntimeValue actual_value = value.DeepCopy();

  // Check function-local variables first (parameters and locals)
  if (!frame_->call_stack.empty()) {
    auto& call_frame = frame_->call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      it->second = actual_value;
      return;
    }
  }

  // Check process-local next
  if (frame_->variable_table.Exists(symbol)) {
    frame_->variable_table.Write(symbol, actual_value);
    return;
  }

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);
  const auto* actual_symbol =
      (target_instance != nullptr) ? target_symbol : symbol;

  // Write to flat storage
  if (!is_non_blocking) {
    simulation_context_->variable_store.Write(actual_symbol, actual_value);
    effect_->RecordVariableModification(actual_symbol);
  } else {
    effect_->RecordNbaAction(
        {.symbol = actual_symbol,
         .value = actual_value,
         .array_index = std::nullopt});
  }
}

namespace {

/// Compute actual array index after adjusting for lower bound.
/// Throws std::runtime_error if out of bounds.
auto ComputeArrayIndex(const RuntimeValue& array_value, int64_t sv_index)
    -> size_t {
  int32_t lower_bound = 0;
  if (array_value.type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data =
        std::get<common::UnpackedArrayData>(array_value.type.data);
    lower_bound = array_data.lower_bound;
  }

  auto actual_idx = static_cast<size_t>(sv_index - lower_bound);

  size_t container_size = array_value.IsQueue() ? array_value.AsQueue().size()
                                                : array_value.AsArray().size();
  if (actual_idx >= container_size) {
    throw std::runtime_error(
        fmt::format(
            "array index {} out of bounds (size {})", sv_index,
            container_size));
  }

  return actual_idx;
}

}  // namespace

void InstructionContext::StoreElement(
    const lir::Operand& aggregate_operand, size_t index,
    const RuntimeValue& element_value, bool is_non_blocking) {
  // Temp operand: modify in place, no sensitivity tracking needed
  if (!aggregate_operand.IsVariable()) {
    auto aggregate_value = GetTemp(aggregate_operand.AsTempRef());
    if (aggregate_value.IsArray() || aggregate_value.IsQueue()) {
      auto actual_idx =
          ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
      aggregate_value.SetElement(actual_idx, element_value);
    } else {
      aggregate_value.SetField(index, element_value);
    }
    return;
  }

  // Variable operand: need sensitivity tracking
  const auto* symbol = std::get<lir::SymbolRef>(aggregate_operand.value);
  auto aggregate_value = ReadVariable(aggregate_operand);

  // Check if this is a local variable (no triggers needed)
  bool is_local = false;
  if (!frame_->call_stack.empty()) {
    auto& call_frame = frame_->call_stack.back();
    is_local = call_frame.local_variables.contains(symbol);
  }
  if (!is_local) {
    is_local = frame_->variable_table.Exists(symbol);
  }

  // Compute actual index for arrays and queues
  size_t actual_idx = index;
  if (aggregate_value.IsArray() || aggregate_value.IsQueue()) {
    actual_idx =
        ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
  }

  // Resolve port bindings
  auto [target_symbol, target_instance] = ResolveBinding(symbol);
  const auto* actual_symbol =
      (target_instance != nullptr) ? target_symbol : symbol;

  if (is_non_blocking) {
    // Queue NBA action with array index
    effect_->RecordNbaAction(
        {.symbol = actual_symbol,
         .value = element_value.DeepCopy(),
         .array_index = actual_idx});
    return;
  }

  // Blocking assignment: snapshot before modification
  if (!is_local) {
    simulation_context_->variable_store.UpdatePrevious(actual_symbol);
  }

  // Perform the element store
  if (aggregate_value.IsArray() || aggregate_value.IsQueue()) {
    aggregate_value.SetElement(actual_idx, element_value);
  } else {
    aggregate_value.SetField(index, element_value);
  }

  // Write modified aggregate back to flat storage
  simulation_context_->variable_store.Write(actual_symbol, aggregate_value);

  // Record modification for trigger system
  if (!is_local) {
    effect_->RecordVariableModification(actual_symbol);
  }
}

auto InstructionContext::GetOperandValue(const lir::Operand& operand) const
    -> RuntimeValue {
  if (operand.IsTemp()) {
    return GetTemp(operand.AsTempRef());
  }
  if (operand.IsVariable()) {
    return ReadVariable(operand);
  }
  if (operand.IsConstant()) {
    const auto& constant = std::get<lir::ConstantRef>(operand.value);
    return RuntimeValue::FromConstant(constant);
  }
  throw common::InternalError(
      "interpreter", "unexpected operand kind for instruction");
}

auto InstructionContext::EvalUnaryOp(
    const lir::Operand& operand, lir::TempRef result,
    const std::function<RuntimeValue(RuntimeValue)>& op) -> InstructionResult {
  const auto result_value = op(GetTemp(operand.AsTempRef()));
  temp_table_->Write(result, result_value);
  return InstructionResult::Continue();
}

auto InstructionContext::EvalBinaryOp(
    const lir::Operand& lhs, const lir::Operand& rhs, lir::TempRef result,
    const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
    -> InstructionResult {
  const auto result_value =
      op(GetTemp(lhs.AsTempRef()), GetTemp(rhs.AsTempRef()));
  temp_table_->Write(result, result_value);
  return InstructionResult::Continue();
}

void InstructionContext::WriteTemp(lir::TempRef result, RuntimeValue value) {
  temp_table_->Write(result, std::move(value));
}

}  // namespace lyra::interpreter
