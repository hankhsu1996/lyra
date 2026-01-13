#include <cassert>
#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/interpreter/intrinsic.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Constant = common::Constant;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

namespace {

// Synthesizes a check process for $monitor that periodically re-evaluates
// monitored expressions and prints when any change.
// format_literal: optional format string literal (nullptr if no format string)
// display_call: display variant to use ($display, $displayb, $displayo,
// $displayh)
auto SynthesizeMonitorCheckProcess(
    const std::vector<const mir::Expression*>& monitored_exprs,
    const mir::Expression* format_literal, const std::string& display_call,
    LirBuilder& builder) -> std::string {
  // Generate unique process name
  std::string process_name = builder.MakeSyntheticFunctionName("monitor_check");

  // Begin synthetic process (saves current process state)
  builder.BeginSyntheticProcess(process_name);

  builder.StartBlock(builder.MakeLabel("entry"));

  // Handle empty monitor case: $monitor() with no expressions
  // Just complete immediately - nothing to track for changes
  if (monitored_exprs.empty()) {
    builder.AddInstruction(Instruction::Complete());
    return builder.EndSyntheticProcess();
  }

  // Generate capture names for prev values (matches $monitor handler)
  auto make_capture_name = [](size_t i) {
    return "__capture_prev_" + std::to_string(i);
  };

  // Phase 1: Evaluate all monitored expressions
  std::vector<TempRef> current_temps;
  current_temps.reserve(monitored_exprs.size());
  for (const auto* expr : monitored_exprs) {
    current_temps.push_back(LowerExpression(*expr, builder));
  }

  // Phase 2: Load previous values from captures and compare
  std::vector<TempRef> change_flags;
  change_flags.reserve(monitored_exprs.size());

  for (size_t i = 0; i < monitored_exprs.size(); ++i) {
    const auto& expr_type = monitored_exprs[i]->type;

    // Load previous value from capture
    TempRef prev = builder.AllocateTemp("prev", expr_type);
    builder.AddInstruction(
        Instruction::LoadCapture(prev, make_capture_name(i), expr_type));

    // Compare: changed_i = (current != prev)
    TempRef changed = builder.AllocateTemp("chg", Type::Bool());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryNotEqual, changed,
            {Operand::Temp(current_temps[i]), Operand::Temp(prev)}));
    change_flags.push_back(changed);
  }

  // Phase 3: OR all change flags together
  TempRef any_changed = change_flags[0];
  for (size_t i = 1; i < change_flags.size(); ++i) {
    TempRef new_changed = builder.AllocateTemp("any", Type::Bool());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryLogicalOr, new_changed,
            {Operand::Temp(any_changed), Operand::Temp(change_flags[i])}));
    any_changed = new_changed;
  }

  // Phase 4: Branch - if any changed, print and update; else return
  auto print_label = builder.MakeLabel("do_print");
  auto end_label = builder.MakeLabel("end");
  builder.AddInstruction(
      Instruction::Branch(any_changed, print_label, end_label));
  builder.EndBlock();

  // do_print block: emit $display and store new prev values
  builder.StartBlock(print_label);

  // Build operands for $display: current values (format string is separate)
  std::vector<Operand> display_operands;
  for (const auto& temp : current_temps) {
    display_operands.push_back(Operand::Temp(temp));
  }

  // Use the provided display variant ($display, $displayb, $displayo,
  // $displayh)
  auto display_instr =
      Instruction::SystemCall(display_call, std::move(display_operands));
  if (format_literal != nullptr) {
    TempRef fmt_temp = LowerExpression(*format_literal, builder);
    display_instr.format_operand = Operand::Temp(fmt_temp);
  }
  display_instr.format_string_is_literal = (format_literal != nullptr);
  builder.AddInstruction(std::move(display_instr));

  // Store current values as new prev values in captures
  for (size_t i = 0; i < current_temps.size(); ++i) {
    builder.AddInstruction(
        Instruction::StoreCapture(current_temps[i], make_capture_name(i)));
  }

  builder.AddInstruction(Instruction::Jump(end_label));
  builder.EndBlock();

  // end block: complete (process terminator)
  builder.StartBlock(end_label);
  builder.AddInstruction(Instruction::Complete());

  // End synthetic process (restores process state, adds process to module)
  return builder.EndSyntheticProcess();
}

}  // namespace

auto LowerSystemCallExpression(
    const mir::SystemCallExpression& system_call, LirBuilder& builder)
    -> lir::TempRef {
  // Supported system calls are validated in AST->MIR
  assert(common::IsSystemFunctionSupported(system_call.name));

  // Check if this is a $monitor variant that needs symbol tracking
  bool is_monitor =
      (system_call.name == "$monitor" || system_call.name == "$monitorb" ||
       system_call.name == "$monitoro" || system_call.name == "$monitorh");

  std::vector<Operand> operands;
  std::vector<TempRef> arguments;

  auto lower_system_call_operand =
      [&](const mir::Expression& argument) -> Operand {
    if (argument.kind == mir::Expression::Kind::kIdentifier) {
      const auto& ident = mir::As<mir::IdentifierExpression>(argument);
      return Operand::Variable(ident.symbol);
    }
    // For constant expressions, keep as constant operand to preserve metadata
    if (argument.kind == mir::Expression::Kind::kConstant) {
      const auto& lit = mir::As<mir::ConstantExpression>(argument);
      auto constant_ref = builder.InternConstant(lit.constant);
      return Operand::Constant(constant_ref);
    }
    auto temp = LowerExpression(argument, builder);
    return Operand::Temp(temp);
  };

  if (is_monitor) {
    // Format string is now in format_expr (separate from arguments)
    const mir::Expression* format_literal = nullptr;
    if (system_call.format_expr && system_call.format_expr_is_literal) {
      format_literal = system_call.format_expr->get();
    }

    // Collect pointers to monitored expressions for synthesis
    // All arguments are now monitored values (format string is separate)
    std::vector<const mir::Expression*> monitored_exprs;

    for (const auto& argument : system_call.arguments) {
      if (argument) {
        // Collect expression for check function synthesis
        monitored_exprs.push_back(argument.get());

        // Lower expression at call site for initial print
        TempRef result = LowerExpression(*argument, builder);
        arguments.push_back(result);
      }
    }

    // Determine display variant based on monitor variant
    // $monitor -> $display, $monitorb -> $displayb, etc.
    std::string display_call;
    if (system_call.name == "$monitor") {
      display_call = "$display";
    } else if (system_call.name == "$monitorb") {
      display_call = "$displayb";
    } else if (system_call.name == "$monitoro") {
      display_call = "$displayo";
    } else if (system_call.name == "$monitorh") {
      display_call = "$displayh";
    } else {
      display_call = "$display";  // fallback
    }

    // Synthesize the check process for periodic re-evaluation
    std::string check_process_name = SynthesizeMonitorCheckProcess(
        monitored_exprs, format_literal, display_call, builder);

    auto instruction = Instruction::SystemCallWithMonitor(
        system_call.name, std::move(arguments), std::move(check_process_name));
    instruction.format_string_is_literal = (format_literal != nullptr);
    if (format_literal != nullptr) {
      TempRef format_temp = LowerExpression(*format_literal, builder);
      instruction.format_operand = Operand::Temp(format_temp);
    }
    builder.AddInstruction(std::move(instruction));
    return builder.AllocateTemp("sys", system_call.type);
  }

  for (const auto& argument : system_call.arguments) {
    if (argument) {
      operands.push_back(lower_system_call_operand(*argument));
    }
  }

  // Add default argument (1) for $finish and $stop if not provided
  // $exit takes no arguments per LRM
  if ((system_call.name == "$finish" || system_call.name == "$stop") &&
      operands.empty() && !is_monitor) {
    auto temp = builder.AllocateTemp("sys", system_call.type);
    auto const_one = builder.InternConstant(Constant::Int(1));
    auto instruction = Instruction::Basic(IK::kConstant, temp, const_one);
    builder.AddInstruction(std::move(instruction));
    operands.push_back(Operand::Temp(temp));
  }

  // System functions return a value, system tasks do not
  bool is_function = common::IsSystemFunction(system_call.name);

  auto result = builder.AllocateTemp("sys", system_call.type);

  // Lower format_expr if present (for display-like tasks)
  std::optional<Operand> format_operand;
  if (system_call.format_expr) {
    auto format_temp = LowerExpression(**system_call.format_expr, builder);
    format_operand = Operand::Temp(format_temp);
  }

  // Collect output targets for system calls like $value$plusargs
  std::vector<lir::SymbolRef> output_targets;
  for (const auto& target : system_call.output_targets) {
    output_targets.push_back(target.symbol);
  }

  // Create instruction - functions get result temp, tasks don't
  auto instruction =
      is_function
          ? Instruction::SystemCall(
                system_call.name, std::move(operands), result, system_call.type)
          : Instruction::SystemCall(system_call.name, std::move(operands));
  instruction.format_operand = format_operand;
  instruction.format_string_is_literal = system_call.format_expr_is_literal;
  instruction.source_file = system_call.source_file;
  instruction.source_line = system_call.source_line;
  instruction.output_targets = std::move(output_targets);
  builder.AddInstruction(std::move(instruction));

  return result;
}

auto LowerFunctionCallExpression(
    const mir::FunctionCallExpression& call, LirBuilder& builder)
    -> lir::TempRef {
  // Lower arguments
  std::vector<Operand> arg_operands;
  arg_operands.reserve(call.arguments.size());
  for (const auto& arg : call.arguments) {
    auto arg_temp = LowerExpression(*arg, builder);
    arg_operands.push_back(Operand::Temp(arg_temp));
  }

  // Allocate result temp if non-void
  std::optional<TempRef> result_temp;
  std::optional<common::Type> result_type;
  if (call.type.kind != common::Type::Kind::kVoid) {
    result_temp = builder.AllocateTemp("call", call.type);
    result_type = call.type;
  }

  // Emit call instruction (function_name is already qualified for package
  // functions)
  auto instr = Instruction::Call(
      call.function_name, std::move(arg_operands), result_temp, result_type);
  builder.AddInstruction(std::move(instr));

  if (result_temp) {
    return *result_temp;
  }
  // Void functions: LowerExpression requires a TempRef return, so we
  // allocate a dummy temp. The caller (typically ExpressionStatement)
  // discards it. This is wasteful but harmless - a proper fix would be
  // changing LowerExpression to return std::optional<TempRef>, which
  // requires updating many call sites.
  return builder.AllocateTemp("void", common::Type::Void());
}

auto LowerMethodCallExpression(
    const mir::MethodCallExpression& method_call, LirBuilder& builder)
    -> lir::TempRef {
  auto receiver = LowerExpression(*method_call.receiver, builder);
  auto result = builder.AllocateTemp("method_call", method_call.type);

  // Enum methods use kIntrinsicOp (value-semantic)
  if (!method_call.enum_members.empty()) {
    using enum common::BuiltinMethod;
    using enum lir::IntrinsicOpKind;

    auto to_op_kind = [](common::BuiltinMethod m) {
      if (m == kNext) {
        return kEnumNext;
      }
      if (m == kPrev) {
        return kEnumPrev;
      }
      return kEnumName;
    };
    auto op_kind = to_op_kind(method_call.method);

    std::vector<Operand> operands{Operand::Temp(receiver)};

    // next/prev take optional step argument (default 1)
    if (method_call.method == kNext || method_call.method == kPrev) {
      TempRef step_temp;
      if (!method_call.args.empty()) {
        step_temp = LowerExpression(*method_call.args[0], builder);
      } else {
        auto one = builder.InternConstant(Constant::Int(1));
        step_temp = builder.AllocateTemp("step", Type::Int());
        builder.AddInstruction(
            Instruction::Basic(IK::kConstant, step_temp, one));
      }
      operands.push_back(Operand::Temp(step_temp));
    }

    builder.AddInstruction(
        Instruction::IntrinsicOp(
            op_kind, std::move(operands), result, method_call.type));
    return result;
  }

  // Container methods (array/queue) use kIntrinsicCall (reference-semantic)
  auto method_name = mir::ToString(method_call.method);
  void* intrinsic_fn = interpreter::ResolveIntrinsicMethod(
      method_call.receiver->type.kind, method_name);
  if (intrinsic_fn == nullptr) {
    throw common::InternalError(
        "LowerMethodCallExpression",
        std::format("unknown intrinsic method: {}", method_name));
  }

  std::vector<Operand> arg_operands;
  arg_operands.reserve(method_call.args.size());
  for (const auto& arg : method_call.args) {
    auto arg_temp = LowerExpression(*arg, builder);
    arg_operands.push_back(Operand::Temp(arg_temp));
  }

  builder.AddInstruction(
      Instruction::IntrinsicCall(
          intrinsic_fn, receiver, std::move(arg_operands), result,
          method_call.type));
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
