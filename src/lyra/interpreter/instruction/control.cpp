#include "lyra/interpreter/instruction/control.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <optional>
#include <utility>

#include <fmt/format.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/system_call_runner.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

namespace {

/// Check if a value is truthy for branch conditions.
auto IsTruthy(const RuntimeValue& value) -> bool {
  if (value.IsReal()) {
    return value.AsDouble() != 0.0;
  }
  if (value.IsShortReal()) {
    return value.AsFloat() != 0.0F;
  }
  assert(value.IsTwoState());
  if (value.IsWide()) {
    return !value.AsWideBit().IsZero();
  }
  return value.AsNarrow().raw != 0;
}

/// Handle kCall instruction: use resolved function pointer, prepare frame.
auto HandleCall(const lir::Instruction& instr, TempTable& temp_table)
    -> InstructionResult {
  const auto* func = instr.callee;
  if (func == nullptr) {
    throw common::InternalError(
        "kCall",
        fmt::format("function '{}' not resolved", instr.called_function_name));
  }

  // Create call frame (return address will be set by process_runner)
  auto frame = std::make_unique<CallFrame>();
  frame->function = func;
  frame->return_value_dest = instr.result;

  // Initialize parameters from arguments
  for (size_t i = 0; i < func->parameters.size(); ++i) {
    const auto& param = func->parameters[i];
    auto temp_ref = std::get<lir::TempRef>(instr.operands[i].value);
    RuntimeValue arg_value = temp_table.Read(temp_ref);
    frame->local_variables[param.variable.symbol] = std::move(arg_value);
  }

  // Initialize local variables with default values
  for (const auto& local : func->local_variables) {
    frame->local_variables[local.symbol] =
        RuntimeValue::DefaultValueForType(local.type);
  }

  return InstructionResult::CallFunction(func->entry_label, std::move(frame));
}

/// Handle kReturn instruction: get return value, pop frame, store result.
auto HandleReturn(
    const lir::Instruction& instr, ProcessFrame& frame, TempTable& temp_table)
    -> InstructionResult {
  if (frame.call_stack.empty()) {
    throw common::InternalError("kReturn", "return outside of function");
  }

  // Get return value BEFORE popping (temp_table points to current frame)
  std::optional<RuntimeValue> return_value;
  if (!instr.operands.empty()) {
    auto temp_ref = std::get<lir::TempRef>(instr.operands[0].value);
    return_value = temp_table.Read(temp_ref);
  }

  // Pop the call frame
  CallFrame call_frame = std::move(frame.call_stack.back());
  frame.call_stack.pop_back();

  // Store return value in caller's temp table (after pop)
  if (return_value && call_frame.return_value_dest) {
    TempTable& caller_temp_table = frame.call_stack.empty()
                                       ? frame.temp_table
                                       : frame.call_stack.back().temp_table;
    caller_temp_table.Write(
        *call_frame.return_value_dest, std::move(*return_value));
  }

  return InstructionResult::ReturnFromFunction(
      call_frame.return_block_index, call_frame.return_instruction_index);
}

}  // namespace

auto HandleControlFlowOps(
    const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  switch (instr.kind) {
    case lir::InstructionKind::kComplete:
      return InstructionResult::Complete();

    case lir::InstructionKind::kWaitEvent:
      return InstructionResult::WaitEvent(instr.wait_triggers);

    case lir::InstructionKind::kDelay: {
      assert(instr.operands[0].IsLiteral());
      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
      const auto delay_amount =
          RuntimeValue::FromLiteral(literal).AsNarrow().AsUInt64();
      return InstructionResult::Delay(delay_amount);
    }

    case lir::InstructionKind::kSystemCall: {
      auto& sim = ctx.GetSimulationContext();
      auto& frame = ctx.GetFrame();
      auto& effect = ctx.GetEffect();
      auto& temp_table = ctx.GetTempTable();
      const auto& instance = ctx.GetHierarchyContext();
      return RunSystemCall(instr, sim, frame, effect, temp_table, instance);
    }

    case lir::InstructionKind::kMethodCall: {
      // Generic method call - handles enum methods and array methods.
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      // Array/queue methods have empty enum_members
      if (instr.enum_members.empty()) {
        // SSA-style: mutating methods return the modified receiver as result
        // This avoids mutation - each operation produces a new value
        auto receiver = ctx.GetTemp(instr.operands[0]).DeepCopy();
        auto& arr = receiver.AsArray();

        // Helper to check if bounded queue is full (cannot add more elements)
        auto is_queue_full = [&]() -> bool {
          if (instr.result_type->IsQueue()) {
            const auto& queue_data =
                std::get<common::QueueData>(instr.result_type->data);
            if (queue_data.max_bound > 0) {
              size_t max_size = queue_data.max_bound + 1;
              return arr.size() >= max_size;
            }
          }
          return false;
        };

        if (instr.method_name == "size") {
          auto size = static_cast<int32_t>(arr.size());
          ctx.WriteTemp(
              instr.result.value(), RuntimeValue::IntegralSigned(size, 32));
        } else if (instr.method_name == "delete") {
          if (instr.operands.size() > 1) {
            auto index = ctx.GetTemp(instr.operands[1]).AsNarrow().AsInt64();
            if (index >= 0 && static_cast<size_t>(index) < arr.size()) {
              arr.erase(arr.begin() + index);
            }
          } else {
            arr.clear();
          }
          // Return modified receiver (SSA style)
          ctx.WriteTemp(instr.result.value(), std::move(receiver));
        } else if (instr.method_name == "push_back") {
          if (!is_queue_full()) {
            auto item = ctx.GetTemp(instr.operands[1]);
            arr.push_back(std::move(item));
          }
          // Return modified receiver (SSA style)
          ctx.WriteTemp(instr.result.value(), std::move(receiver));
        } else if (instr.method_name == "push_front") {
          if (!is_queue_full()) {
            auto item = ctx.GetTemp(instr.operands[1]);
            arr.insert(arr.begin(), std::move(item));
          }
          // Return modified receiver (SSA style)
          ctx.WriteTemp(instr.result.value(), std::move(receiver));
        } else if (instr.method_name == "pop_back") {
          if (arr.empty()) {
            ctx.WriteTemp(
                instr.result.value(),
                RuntimeValue::DefaultValueForType(*instr.result_type));
          } else {
            auto value = std::move(arr.back());
            arr.pop_back();
            ctx.WriteTemp(instr.result.value(), std::move(value));
          }
        } else if (instr.method_name == "pop_front") {
          if (arr.empty()) {
            ctx.WriteTemp(
                instr.result.value(),
                RuntimeValue::DefaultValueForType(*instr.result_type));
          } else {
            auto value = std::move(arr.front());
            arr.erase(arr.begin());
            ctx.WriteTemp(instr.result.value(), std::move(value));
          }
        } else if (instr.method_name == "insert") {
          if (!is_queue_full()) {
            auto index = ctx.GetTemp(instr.operands[1]).AsNarrow().AsInt64();
            auto item = ctx.GetTemp(instr.operands[2]);
            if (index >= 0 && static_cast<size_t>(index) <= arr.size()) {
              arr.insert(arr.begin() + index, std::move(item));
            }
          }
          // Return modified receiver (SSA style)
          ctx.WriteTemp(instr.result.value(), std::move(receiver));
        } else {
          assert(false && "unsupported array/queue method call");
        }
        return InstructionResult::Continue();
      }

      // Enum methods
      const auto& receiver = ctx.GetTemp(instr.operands[0]);
      int64_t current_value = receiver.AsNarrow().AsInt64();
      const auto& members = instr.enum_members;

      // Find current position in enum member list
      size_t current_pos = 0;
      bool found = false;
      for (size_t i = 0; i < members.size(); ++i) {
        if (members[i].value == current_value) {
          current_pos = i;
          found = true;
          break;
        }
      }

      RuntimeValue result;
      if (instr.method_name == "next") {
        if (found) {
          auto step = static_cast<size_t>(instr.method_step);
          size_t target_pos = (current_pos + step) % members.size();
          result = RuntimeValue::IntegralSigned(
              members[target_pos].value, instr.result_type->GetBitWidth());
        } else {
          result =
              RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
        }
      } else if (instr.method_name == "prev") {
        if (found) {
          size_t step = static_cast<size_t>(instr.method_step) % members.size();
          size_t target_pos =
              (current_pos + members.size() - step) % members.size();
          result = RuntimeValue::IntegralSigned(
              members[target_pos].value, instr.result_type->GetBitWidth());
        } else {
          result =
              RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
        }
      } else if (instr.method_name == "name") {
        if (found) {
          result = RuntimeValue::String(members[current_pos].name);
        } else {
          result = RuntimeValue::String("");
        }
      } else {
        assert(false && "unsupported method call");
      }

      ctx.WriteTemp(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kJump: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLabel());
      const auto& target = std::get<lir::LabelRef>(instr.operands[0].value);
      return InstructionResult::Jump(target);
    }

    case lir::InstructionKind::kBranch: {
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsLabel());
      assert(instr.operands[2].IsLabel());

      const auto& condition = ctx.GetTemp(instr.operands[0]);
      const auto& true_target =
          std::get<lir::LabelRef>(instr.operands[1].value);
      const auto& false_target =
          std::get<lir::LabelRef>(instr.operands[2].value);

      return InstructionResult::Jump(
          IsTruthy(condition) ? true_target : false_target);
    }

    case lir::InstructionKind::kCall:
      return HandleCall(instr, ctx.GetTempTable());

    case lir::InstructionKind::kReturn:
      return HandleReturn(instr, ctx.GetFrame(), ctx.GetTempTable());

    case lir::InstructionKind::kLoadCapture: {
      auto& sim = ctx.GetSimulationContext();
      if (!sim.active_monitor.has_value()) {
        throw common::InternalError(
            "kLoadCapture", "no active monitor (closure context)");
      }
      auto& captures = sim.active_monitor->closure.captures;

      auto it = captures.find(instr.capture_name);
      if (it == captures.end()) {
        throw common::InternalError(
            "kLoadCapture",
            std::format("capture '{}' not found", instr.capture_name));
      }

      ctx.WriteTemp(instr.result.value(), it->second);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreCapture: {
      auto& sim = ctx.GetSimulationContext();
      if (!sim.active_monitor.has_value()) {
        throw common::InternalError(
            "kStoreCapture", "no active monitor (closure context)");
      }

      auto& captures = sim.active_monitor->closure.captures;
      auto value = ctx.GetTemp(instr.operands[0]);

      captures[instr.capture_name] = value;
      return InstructionResult::Continue();
    }

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in control flow handler");
  }
}

}  // namespace lyra::interpreter
