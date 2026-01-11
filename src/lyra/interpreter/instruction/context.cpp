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
#include "lyra/interpreter/instance_context.hpp"
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
    std::shared_ptr<InstanceContext> instance_context)
    : simulation_context_(&simulation_context),
      frame_(&frame),
      effect_(&effect),
      temp_table_(&temp_table),
      instance_context_(std::move(instance_context)) {
}

auto InstructionContext::ResolveBinding(common::SymbolRef symbol) const
    -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>> {
  if (instance_context_ != nullptr) {
    return instance_context_->ResolveBinding(symbol);
  }
  return {symbol, nullptr};
}

auto InstructionContext::GetTemp(const lir::Operand& operand) const
    -> RuntimeValue {
  assert(operand.IsTemp());
  return temp_table_->Read(std::get<lir::TempRef>(operand.value));
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

  // Resolve through port bindings (output port -> target signal/instance)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);

  // If bound, read from target instance's storage
  if (target_instance != nullptr) {
    return target_instance->Read(target_symbol);
  }

  // Otherwise, read from per-instance storage (local vars, input ports)
  if (instance_context_ != nullptr && instance_context_->Exists(symbol)) {
    return instance_context_->Read(symbol);
  }

  // Fallback to global table
  return simulation_context_->variable_table.Read(symbol);
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

  // Resolve through port bindings (output port -> target signal/instance)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);

  // If bound (output port), write to target instance's storage
  if (target_instance != nullptr) {
    if (!is_non_blocking) {
      target_instance->Write(target_symbol, actual_value);
      effect_->RecordVariableModification(target_symbol, target_instance);
    } else {
      effect_->RecordNbaAction(
          {.variable = target_symbol,
           .value = actual_value,
           .instance = target_instance,
           .array_index = std::nullopt});
    }
    return;
  }

  // Otherwise, write to per-instance storage (local vars, input ports)
  if (instance_context_ != nullptr) {
    if (!is_non_blocking) {
      instance_context_->Write(symbol, actual_value);
      effect_->RecordVariableModification(symbol, instance_context_);
    } else {
      effect_->RecordNbaAction(
          {.variable = symbol,
           .value = actual_value,
           .instance = instance_context_,
           .array_index = std::nullopt});
    }
    return;
  }

  // Fallback to global table
  if (!is_non_blocking) {
    simulation_context_->variable_table.Write(symbol, actual_value);
    effect_->RecordVariableModification(symbol);
  } else {
    effect_->RecordNbaAction(
        {.variable = symbol,
         .value = actual_value,
         .instance = nullptr,
         .array_index = std::nullopt});
  }
}

void InstructionContext::StoreHierarchical(
    const std::vector<common::SymbolRef>& instances, common::SymbolRef target,
    const RuntimeValue& value, bool is_non_blocking) {
  // Deep copy arrays for value semantics
  const RuntimeValue actual_value = value.DeepCopy();

  // Traverse instance path
  auto target_instance = instance_context_;
  for (const auto& inst_sym : instances) {
    target_instance = target_instance->LookupChild(inst_sym);
    if (!target_instance) {
      throw std::runtime_error(
          fmt::format("unknown child instance: {}", inst_sym->name));
    }
  }

  // Write to target instance
  if (!is_non_blocking) {
    target_instance->Write(target, actual_value);
    effect_->RecordVariableModification(target, target_instance);
  } else {
    effect_->RecordNbaAction(
        {.variable = target,
         .value = actual_value,
         .instance = target_instance,
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

  if (actual_idx >= array_value.AsArray().size()) {
    throw std::runtime_error(
        fmt::format(
            "array index {} out of bounds (size {})", sv_index,
            array_value.AsArray().size()));
  }

  return actual_idx;
}

}  // namespace

void InstructionContext::StoreElement(
    const lir::Operand& aggregate_operand, size_t index,
    const RuntimeValue& element_value, bool is_non_blocking) {
  // Temp operand: modify in place, no sensitivity tracking needed
  if (!aggregate_operand.IsVariable()) {
    auto aggregate_value = GetTemp(aggregate_operand);
    if (aggregate_value.IsArray()) {
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

  // Compute actual index for arrays
  size_t actual_idx = index;
  if (aggregate_value.IsArray()) {
    actual_idx =
        ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
  }

  if (is_non_blocking) {
    // Queue NBA action with array index
    auto [target_symbol, target_instance] = ResolveBinding(symbol);
    if (target_instance != nullptr) {
      effect_->RecordNbaAction(
          {.variable = target_symbol,
           .value = element_value.DeepCopy(),
           .instance = target_instance,
           .array_index = actual_idx});
    } else if (instance_context_ != nullptr) {
      effect_->RecordNbaAction(
          {.variable = symbol,
           .value = element_value.DeepCopy(),
           .instance = instance_context_,
           .array_index = actual_idx});
    } else {
      effect_->RecordNbaAction(
          {.variable = symbol,
           .value = element_value.DeepCopy(),
           .instance = nullptr,
           .array_index = actual_idx});
    }
    return;
  }

  // Blocking assignment: snapshot, modify, record
  if (!is_local) {
    auto [target_symbol, target_instance] = ResolveBinding(symbol);
    if (target_instance != nullptr) {
      target_instance->UpdatePrevious(target_symbol, aggregate_value);
    } else if (instance_context_ != nullptr) {
      instance_context_->UpdatePrevious(symbol, aggregate_value);
    }
  }

  // Perform the element store
  if (aggregate_value.IsArray()) {
    aggregate_value.SetElement(actual_idx, element_value);
  } else {
    aggregate_value.SetField(index, element_value);
  }

  // Record modification for trigger system
  if (!is_local) {
    auto [target_symbol, target_instance] = ResolveBinding(symbol);
    if (target_instance != nullptr) {
      effect_->RecordVariableModification(target_symbol, target_instance);
    } else if (instance_context_ != nullptr) {
      effect_->RecordVariableModification(symbol, instance_context_);
    } else {
      effect_->RecordVariableModification(symbol);
    }
  }
}

auto InstructionContext::LoadHierarchical(
    const std::vector<common::SymbolRef>& instances,
    common::SymbolRef target) const -> RuntimeValue {
  // Traverse instance path
  auto target_instance = instance_context_;
  for (const auto& inst_sym : instances) {
    target_instance = target_instance->LookupChild(inst_sym);
    if (!target_instance) {
      throw std::runtime_error(
          fmt::format("unknown child instance: {}", inst_sym->name));
    }
  }

  // Read from target instance
  return target_instance->Read(target);
}

auto InstructionContext::GetOperandValue(const lir::Operand& operand) const
    -> RuntimeValue {
  if (operand.IsTemp()) {
    return GetTemp(operand);
  }
  if (operand.IsVariable()) {
    return ReadVariable(operand);
  }
  if (operand.IsLiteral()) {
    const auto& literal = std::get<lir::LiteralRef>(operand.value);
    return RuntimeValue::FromLiteral(literal);
  }
  throw common::InternalError(
      "interpreter", "unexpected operand kind for instruction");
}

auto InstructionContext::EvalUnaryOp(
    const lir::Operand& operand, lir::TempRef result,
    const std::function<RuntimeValue(RuntimeValue)>& op) -> InstructionResult {
  const auto result_value = op(GetTemp(operand));
  temp_table_->Write(result, result_value);
  return InstructionResult::Continue();
}

auto InstructionContext::EvalBinaryOp(
    const lir::Operand& lhs, const lir::Operand& rhs, lir::TempRef result,
    const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
    -> InstructionResult {
  const auto result_value = op(GetTemp(lhs), GetTemp(rhs));
  temp_table_->Write(result, result_value);
  return InstructionResult::Continue();
}

void InstructionContext::WriteTemp(lir::TempRef result, RuntimeValue value) {
  temp_table_->Write(result, std::move(value));
}

}  // namespace lyra::interpreter
