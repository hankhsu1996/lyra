#include "lyra/interpreter/instruction/control.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <optional>
#include <utility>
#include <vector>

#include <fmt/format.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/intrinsic.hpp"
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
    auto temp_ref = instr.operands[i].AsTempRef();
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
    auto temp_ref = instr.operands[0].AsTempRef();
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
      assert(instr.delay_amount.has_value());
      const auto delay_value =
          RuntimeValue::FromConstant(*instr.delay_amount).AsNarrow().AsUInt64();
      return InstructionResult::Delay(delay_value);
    }

    case lir::InstructionKind::kSystemCall: {
      auto& sim = ctx.GetSimulationContext();
      auto& frame = ctx.GetFrame();
      auto& effect = ctx.GetEffect();
      auto& temp_table = ctx.GetTempTable();
      const auto& instance = ctx.GetHierarchyContext();
      return RunSystemCall(instr, sim, frame, effect, temp_table, instance);
    }

    case lir::InstructionKind::kIntrinsicCall: {
      assert(instr.result.has_value());
      assert(!instr.temp_operands.empty());
      assert(instr.intrinsic_fn != nullptr);

      auto receiver = ctx.GetTemp(instr.temp_operands[0]).DeepCopy();

      std::vector<RuntimeValue> args;
      args.reserve(instr.temp_operands.size() - 1);
      for (size_t i = 1; i < instr.temp_operands.size(); ++i) {
        args.push_back(ctx.GetTemp(instr.temp_operands[i]));
      }

      // NOLINTNEXTLINE(cppcoreguidelines-pro-type-reinterpret-cast)
      auto* fn = reinterpret_cast<IntrinsicFn>(instr.intrinsic_fn);
      RuntimeValue result = fn(std::move(receiver), args, instr);

      ctx.WriteTemp(instr.result.value(), std::move(result));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kIntrinsicOp: {
      assert(instr.result.has_value());
      assert(instr.type_context.has_value());
      assert(!instr.temp_operands.empty());

      int64_t value = ctx.GetTemp(instr.temp_operands[0]).AsNarrow().AsInt64();
      const auto& enum_data = instr.type_context->GetEnumData();

      RuntimeValue result;
      switch (instr.op_kind) {
        case lir::IntrinsicOpKind::kEnumNext: {
          int64_t step =
              ctx.GetTemp(instr.temp_operands[1]).AsNarrow().AsInt64();
          result = ExecuteEnumNext(
              value, step, enum_data, (*instr.result)->type.GetBitWidth());
          break;
        }
        case lir::IntrinsicOpKind::kEnumPrev: {
          int64_t step =
              ctx.GetTemp(instr.temp_operands[1]).AsNarrow().AsInt64();
          result = ExecuteEnumPrev(
              value, step, enum_data, (*instr.result)->type.GetBitWidth());
          break;
        }
        case lir::IntrinsicOpKind::kEnumName: {
          result = ExecuteEnumName(value, enum_data);
          break;
        }
      }

      ctx.WriteTemp(instr.result.value(), std::move(result));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kJump: {
      assert(instr.jump_target.has_value());
      return InstructionResult::Jump(*instr.jump_target);
    }

    case lir::InstructionKind::kBranch: {
      assert(instr.temp_operands.size() == 1);
      assert(instr.branch_true.has_value());
      assert(instr.branch_false.has_value());

      const auto& condition = ctx.GetTemp(instr.temp_operands[0]);
      return InstructionResult::Jump(
          IsTruthy(condition) ? *instr.branch_true : *instr.branch_false);
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
      auto value = ctx.GetTemp(instr.operands[0].AsTempRef());

      captures[instr.capture_name] = value;
      return InstructionResult::Continue();
    }

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in control flow handler");
  }
}

}  // namespace lyra::interpreter
