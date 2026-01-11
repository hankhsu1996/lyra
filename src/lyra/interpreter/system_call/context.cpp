#include "lyra/interpreter/system_call/context.hpp"

#include <cassert>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

SystemCallContext::SystemCallContext(
    SimulationContext& simulation_context, ProcessFrame& frame,
    ProcessEffect& effect, TempTable& temp_table,
    std::shared_ptr<InstanceContext> instance_context)
    : simulation_context_(simulation_context),
      frame_(frame),
      effect_(effect),
      temp_table_(temp_table),
      instance_context_(std::move(instance_context)) {
}

auto SystemCallContext::ResolveBinding(common::SymbolRef symbol) const
    -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>> {
  if (instance_context_ != nullptr) {
    return instance_context_->ResolveBinding(symbol);
  }
  return {symbol, nullptr};
}

auto SystemCallContext::GetTemp(const lir::Operand& operand) const
    -> RuntimeValue {
  assert(operand.IsTemp());
  return temp_table_.Read(std::get<lir::TempRef>(operand.value));
}

auto SystemCallContext::ReadVariable(const lir::Operand& operand) const
    -> RuntimeValue {
  assert(operand.IsVariable());
  const auto* symbol = std::get<lir::SymbolRef>(operand.value);

  // Check function-local variables first (parameters and locals)
  if (!frame_.call_stack.empty()) {
    const auto& call_frame = frame_.call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      return it->second;
    }
  }

  // Check process-local next
  if (frame_.variable_table.Exists(symbol)) {
    return frame_.variable_table.Read(symbol);
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
  return simulation_context_.variable_table.Read(symbol);
}

void SystemCallContext::StoreVariable(
    const lir::Operand& operand, const RuntimeValue& value) {
  assert(operand.IsVariable());
  const auto* symbol = std::get<lir::SymbolRef>(operand.value);

  // Deep copy arrays for value semantics
  const RuntimeValue actual_value = value.DeepCopy();

  // Check function-local variables first (parameters and locals)
  if (!frame_.call_stack.empty()) {
    auto& call_frame = frame_.call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      it->second = actual_value;
      return;
    }
  }

  // Check process-local next
  if (frame_.variable_table.Exists(symbol)) {
    frame_.variable_table.Write(symbol, actual_value);
    return;
  }

  // Resolve through port bindings (output port -> target signal/instance)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);

  // If bound (output port), write to target instance's storage
  if (target_instance != nullptr) {
    target_instance->Write(target_symbol, actual_value);
    effect_.RecordVariableModification(target_symbol, target_instance);
    return;
  }

  // Otherwise, write to per-instance storage
  if (instance_context_ != nullptr) {
    instance_context_->Write(symbol, actual_value);
    effect_.RecordVariableModification(symbol, instance_context_);
    return;
  }

  // Fallback to global table
  simulation_context_.variable_table.Write(symbol, actual_value);
  effect_.RecordVariableModification(symbol);
}

auto SystemCallContext::GetOperandValue(const lir::Operand& operand) const
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
      "interpreter", "unexpected operand kind for system call");
}

}  // namespace lyra::interpreter
