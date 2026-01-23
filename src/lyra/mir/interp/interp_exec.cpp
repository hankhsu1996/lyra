#include <cstddef>
#include <cstdint>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/interp/format.hpp"
#include "lyra/mir/interp/interp_helpers.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/runtime_real_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"

namespace lyra::mir::interp {

namespace {

// Helper: format severity prefix for $info/$warning/$error messages
auto FormatSeverityPrefix(Severity level) -> std::string_view {
  switch (level) {
    case Severity::kInfo:
      return "info: ";
    case Severity::kWarning:
      return "warning: ";
    case Severity::kError:
      return "error: ";
  }
  return "unknown: ";
}

}  // namespace

auto Interpreter::Run(ProcessState& state) -> ProcessStatus {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

    // Execute all instructions in block
    while (state.instruction_index < block.instructions.size()) {
      ExecInstruction(state, block.instructions[state.instruction_index]);
      state.instruction_index++;
    }

    // Execute terminator
    auto next = ExecTerminator(state, block.terminator);
    if (!next) {
      state.status = ProcessStatus::kFinished;
      break;
    }

    state.current_block = *next;
    state.instruction_index = 0;
  }

  return state.status;
}

auto Interpreter::RunFunction(
    FunctionId func_id, const std::vector<RuntimeValue>& args,
    DesignState* design_state) -> RuntimeValue {
  const auto& func = (*arena_)[func_id];

  // Use function's metadata for storage allocation
  const auto& local_types = func.local_types;
  const auto& temp_types = func.temp_types;

  // Initialize locals with default values
  std::vector<RuntimeValue> locals;
  locals.reserve(local_types.size());
  for (TypeId type_id : local_types) {
    locals.push_back(
        type_id ? CreateDefaultValue(*types_, type_id) : RuntimeValue{});
  }

  // Initialize temps with default values
  std::vector<RuntimeValue> temps;
  temps.reserve(temp_types.size());
  for (TypeId type_id : temp_types) {
    temps.push_back(
        type_id ? CreateDefaultValue(*types_, type_id) : RuntimeValue{});
  }

  // Copy arguments to parameter locals
  // For non-void functions: local 0 = return value, parameters start at local 1
  // For void functions: parameters start at local 0
  // We detect this by checking if there are more locals than args
  size_t param_start = 0;
  if (local_types.size() > args.size()) {
    param_start = 1;  // local 0 is return place
  }

  for (size_t i = 0; i < args.size(); ++i) {
    size_t local_idx = param_start + i;
    if (local_idx < locals.size()) {
      locals[local_idx] = Clone(args[i]);
    }
  }

  // Create function state (reuse ProcessState structure for convenience)
  ProcessState func_state{
      .process = ProcessId{0},  // Unused for function execution
      .current_block = func.entry,
      .instruction_index = 0,
      .frame = Frame(std::move(locals), std::move(temps)),
      .design_state = design_state,
      .status = ProcessStatus::kRunning,
      .pending_suspend = std::nullopt,
  };

  // Execute function (look up blocks from func directly)
  while (func_state.status == ProcessStatus::kRunning) {
    const auto& block = func.blocks[func_state.current_block.value];

    while (func_state.instruction_index < block.instructions.size()) {
      ExecInstruction(
          func_state, block.instructions[func_state.instruction_index]);
      func_state.instruction_index++;
    }

    auto next = ExecTerminator(func_state, block.terminator);
    if (!next) {
      break;  // Return reached
    }

    func_state.current_block = *next;
    func_state.instruction_index = 0;
  }

  // Return value is in local 0 (for non-void functions)
  if (param_start == 1 && func_state.frame.NumLocals() > 0) {
    return Clone(func_state.frame.GetLocal(0));
  }

  // Void function - return monostate
  return std::monostate{};
}

void Interpreter::ExecAssign(ProcessState& state, const Assign& assign) {
  auto value = EvalOperand(state, assign.source);
  StoreToPlace(state, assign.target, std::move(value));
}

void Interpreter::ExecCompute(ProcessState& state, const Compute& compute) {
  TypeId result_type = TypeOfPlace(*types_, (*arena_)[compute.target]);
  auto value = EvalRvalue(state, compute.value, result_type);
  StoreToPlace(state, compute.target, std::move(value));
}

void Interpreter::ExecEffect(ProcessState& state, const Effect& effect) {
  std::visit(
      Overloaded{
          [&](const DisplayEffect& op) { ExecDisplayEffect(state, op); },
          [&](const SeverityEffect& op) { ExecSeverityEffect(state, op); },
      },
      effect.op);
}

void Interpreter::ExecDisplayEffect(
    const ProcessState& state, const DisplayEffect& disp) {
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  FormatContext ctx{};

  for (const auto& op : disp.ops) {
    if (op.kind == FormatKind::kLiteral) {
      out << op.literal;
    } else {
      // Evaluate the value and format it
      RuntimeValue value = EvalOperand(state, *op.value);
      TypedValue typed{.value = std::move(value), .type = op.type};

      // Convert FormatKind and modifiers to FormatSpec
      FormatSpec spec{
          .kind = op.kind,
          .width = op.mods.width,
          .precision = op.mods.precision,
          .zero_pad = op.mods.zero_pad,
          .left_align = op.mods.left_align};

      out << FormatValue(typed, spec, *types_, ctx);
    }
  }

  if (disp.print_kind == PrintKind::kDisplay) {
    out << "\n";
  }
}

void Interpreter::ExecSeverityEffect(
    const ProcessState& state, const SeverityEffect& severity) {
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  // Print severity prefix
  out << FormatSeverityPrefix(severity.level);

  // Evaluate and format message arguments
  std::vector<TypedValue> typed_args;
  typed_args.reserve(severity.args.size());
  for (const auto& arg : severity.args) {
    TypeId type = TypeOfOperand(arg, *arena_, *types_);
    RuntimeValue value = EvalOperand(state, arg);
    typed_args.push_back(TypedValue{.value = std::move(value), .type = type});
  }

  // Use decimal as default radix for severity messages
  FormatContext ctx{};
  std::string message = FormatMessage(typed_args, 'd', *types_, ctx);

  out << message << "\n";
}

void Interpreter::ExecGuardedAssign(
    ProcessState& state, const GuardedAssign& guarded) {
  // Always evaluate source unconditionally (per spec: only the write is
  // guarded)
  auto value = EvalOperand(state, guarded.source);

  // Evaluate validity predicate
  auto validity = EvalOperand(state, guarded.validity);
  if (!IsIntegral(validity)) {
    throw common::InternalError(
        "ExecGuardedAssign", "validity must be integral");
  }
  const auto& valid_int = AsIntegral(validity);

  // If invalid (OOB/X/Z), the write is a no-op
  if (valid_int.IsZero()) {
    return;
  }

  // Valid: perform the assignment
  StoreToPlace(state, guarded.target, std::move(value));
}

void Interpreter::ExecInstruction(
    ProcessState& state, const Instruction& inst) {
  std::visit(
      [&](const auto& i) {
        using T = std::decay_t<decltype(i)>;
        if constexpr (std::is_same_v<T, Assign>) {
          ExecAssign(state, i);
        } else if constexpr (std::is_same_v<T, Compute>) {
          ExecCompute(state, i);
        } else if constexpr (std::is_same_v<T, GuardedAssign>) {
          ExecGuardedAssign(state, i);
        } else if constexpr (std::is_same_v<T, Effect>) {
          ExecEffect(state, i);
        }
      },
      inst.data);
}

auto Interpreter::ExecTerminator(ProcessState& state, const Terminator& term)
    -> std::optional<BasicBlockId> {
  return std::visit(
      Overloaded{
          [](const Jump& t) -> std::optional<BasicBlockId> { return t.target; },

          [&](const Branch& t) -> std::optional<BasicBlockId> {
            auto cond = ReadPlace(state, t.condition);
            // Support integral, real, and shortreal conditions
            if (IsReal(cond)) {
              return RealIsTrue(AsReal(cond)) ? t.then_target : t.else_target;
            }
            if (IsShortReal(cond)) {
              return ShortRealIsTrue(AsShortReal(cond)) ? t.then_target
                                                        : t.else_target;
            }
            if (!IsIntegral(cond)) {
              throw common::InternalError(
                  "ExecTerminator",
                  "branch condition must be integral, real, or shortreal");
            }
            const auto& cond_int = AsIntegral(cond);
            if (!cond_int.IsKnown()) {
              throw std::runtime_error("branch condition is X/Z");
            }
            return cond_int.IsZero() ? t.else_target : t.then_target;
          },

          [&](const Switch& t) -> std::optional<BasicBlockId> {
            if (t.targets.empty()) {
              throw common::InternalError(
                  "ExecTerminator", "switch terminator has no targets");
            }
            auto selector = ReadPlace(state, t.selector);
            if (!IsIntegral(selector)) {
              throw common::InternalError(
                  "ExecTerminator", "switch selector must be integral");
            }
            const auto& sel_int = AsIntegral(selector);
            if (!sel_int.IsKnown()) {
              return t.targets.back();
            }
            uint64_t val = sel_int.value.empty() ? 0 : sel_int.value[0];
            if (val >= t.targets.size() - 1) {
              return t.targets.back();
            }
            return t.targets[static_cast<size_t>(val)];
          },

          [&](const QualifiedDispatch& t) -> std::optional<BasicBlockId> {
            // Validate invariant: targets = one per condition + else
            if (t.targets.size() != t.conditions.size() + 1) {
              throw common::InternalError(
                  "ExecTerminator",
                  "QualifiedDispatch: targets.size() != conditions.size() + 1");
            }

            std::ostream& out = output_ != nullptr ? *output_ : std::cout;

            // Read all condition values and count how many are true
            size_t first_true_index = t.conditions.size();  // sentinel
            size_t true_count = 0;
            for (size_t i = 0; i < t.conditions.size(); ++i) {
              auto cond = ReadPlace(state, t.conditions[i]);
              if (!IsIntegral(cond)) {
                throw common::InternalError(
                    "ExecTerminator",
                    "QualifiedDispatch condition must be integral");
              }
              const auto& cond_int = AsIntegral(cond);
              // Throw on X/Z conditions (same as Branch)
              if (!cond_int.IsKnown()) {
                throw std::runtime_error("QualifiedDispatch condition is X/Z");
              }
              if (!cond_int.IsZero()) {
                ++true_count;
                if (first_true_index == t.conditions.size()) {
                  first_true_index = i;
                }
              }
            }

            const char* qualifier_name =
                (t.qualifier == DispatchQualifier::kUnique) ? "unique"
                                                            : "unique0";
            const char* stmt_name =
                (t.statement_kind == DispatchStatementKind::kIf) ? "if"
                                                                 : "case";

            // Check for overlap (multiple conditions true)
            if (true_count > 1) {
              const char* what =
                  (t.statement_kind == DispatchStatementKind::kIf)
                      ? "multiple conditions true"
                      : "multiple case items match";
              out << "warning: " << what << " in " << qualifier_name << " "
                  << stmt_name << "\n";
            }

            // Check for no-match (only for kUnique, and only if no else)
            if (true_count == 0 && !t.has_else &&
                t.qualifier == DispatchQualifier::kUnique) {
              const char* what =
                  (t.statement_kind == DispatchStatementKind::kIf)
                      ? "no condition matched"
                      : "no matching case item";
              out << "warning: " << what << " in " << qualifier_name << " "
                  << stmt_name << "\n";
            }

            // Dispatch to first true target or else target (last one)
            if (first_true_index < t.conditions.size()) {
              return t.targets[first_true_index];
            }
            return t.targets.back();
          },

          [&](const Delay& t) -> std::optional<BasicBlockId> {
            // Signal suspension - RunUntilSuspend will return SuspendDelay
            state.pending_suspend = SuspendDelay{
                .ticks = t.ticks,
                .resume_block = t.resume,
            };
            return std::nullopt;
          },

          [](const Wait& /*t*/) -> std::optional<BasicBlockId> {
            throw common::InternalError(
                "ExecTerminator", "wait terminator requires runtime/scheduler");
          },

          [](const Return& /*t*/) -> std::optional<BasicBlockId> {
            return std::nullopt;
          },

          [&](const Finish& t) -> std::optional<BasicBlockId> {
            std::ostream& out = output_ != nullptr ? *output_ : std::cout;

            if (t.kind == TerminationKind::kFatal) {
              if (t.level >= 1) {
                out << "fatal: ";
                if (!t.message_args.empty()) {
                  std::vector<TypedValue> typed_args;
                  typed_args.reserve(t.message_args.size());
                  for (const auto& arg : t.message_args) {
                    TypeId type = TypeOfOperand(arg, *arena_, *types_);
                    RuntimeValue value = EvalOperand(state, arg);
                    typed_args.push_back(
                        TypedValue{.value = std::move(value), .type = type});
                  }
                  FormatContext ctx{};
                  std::string message =
                      FormatMessage(typed_args, 'd', *types_, ctx);
                  out << message;
                }
                out << "\n";
              }
            } else if (t.level >= 1) {
              const char* name = nullptr;
              switch (t.kind) {
                case TerminationKind::kFinish:
                  name = "$finish";
                  break;
                case TerminationKind::kStop:
                  name = "$stop";
                  break;
                case TerminationKind::kFatal:
                  name = "$fatal";
                  break;
                case TerminationKind::kExit:
                  name = "$exit";
                  break;
              }
              out << name << " called at time 0\n";
            }
            return std::nullopt;
          },

          [](const Repeat& /*t*/) -> std::optional<BasicBlockId> {
            throw common::InternalError(
                "ExecTerminator",
                "repeat terminator requires runtime/scheduler");
          },
      },
      term.data);
}

auto Interpreter::RunUntilSuspend(ProcessState& state) -> SuspendReason {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

    // Execute all instructions in block
    while (state.instruction_index < block.instructions.size()) {
      ExecInstruction(state, block.instructions[state.instruction_index]);
      state.instruction_index++;
    }

    // Execute terminator
    auto next_block = ExecTerminator(state, block.terminator);

    if (next_block) {
      // Continue to next block
      state.current_block = *next_block;
      state.instruction_index = 0;
    } else {
      // Check if a suspension terminator set pending_suspend
      if (state.pending_suspend) {
        SuspendReason result = *state.pending_suspend;
        state.pending_suspend = std::nullopt;
        return result;
      }
      // Process finished (Return or Finish terminator)
      state.status = ProcessStatus::kFinished;
      return SuspendFinished{};
    }
  }

  return SuspendFinished{};
}

}  // namespace lyra::mir::interp
