#include <cstddef>
#include <cstdint>
#include <format>
#include <fstream>
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
#include "lyra/common/mem_io.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
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

  // Copy arguments to parameter locals using explicit slot mapping
  for (size_t i = 0; i < args.size(); ++i) {
    uint32_t local_slot = func.param_local_slots[i];
    if (local_slot < locals.size()) {
      locals[local_slot] = Clone(args[i]);
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
      .function_return_value = std::nullopt,
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

  // Return value comes from the Return terminator
  if (func_state.function_return_value.has_value()) {
    return std::move(*func_state.function_return_value);
  }

  // Void function or process - return monostate
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
          [&](const MemIOEffect& op) { ExecMemIOEffect(state, op); },
          [&](const FcloseEffect& op) { ExecFcloseEffect(state, op); },
      },
      effect.op);
}

auto Interpreter::FormatDisplayOps(
    const ProcessState& state, std::span<const FormatOp> ops) -> std::string {
  std::string result;
  FormatContext ctx{};

  for (const auto& op : ops) {
    if (op.kind == FormatKind::kLiteral) {
      result += op.literal;
    } else {
      RuntimeValue value = EvalOperand(state, *op.value);
      TypedValue typed{.value = std::move(value), .type = op.type};

      FormatSpec spec{
          .kind = op.kind,
          .width = op.mods.width,
          .precision = op.mods.precision,
          .zero_pad = op.mods.zero_pad,
          .left_align = op.mods.left_align};

      result += FormatValue(typed, spec, *types_, ctx);
    }
  }

  return result;
}

void Interpreter::ExecDisplayEffect(
    const ProcessState& state, const DisplayEffect& disp) {
  if (disp.descriptor) {
    // File-directed output: evaluate descriptor and route through FileManager
    RuntimeValue desc_val = EvalOperand(state, *disp.descriptor);
    if (!IsIntegral(desc_val)) {
      return;  // Invalid descriptor: no-op
    }
    const auto& desc_int = AsIntegral(desc_val);
    auto udesc =
        static_cast<uint32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);

    StreamTargets targets = file_manager_.CollectStreams(udesc);
    if (!targets.include_stdout && targets.file_stream_count == 0) {
      return;  // No targets: no-op
    }

    std::string formatted = FormatDisplayOps(state, disp.ops);
    if (disp.print_kind == PrintKind::kDisplay) {
      formatted += "\n";
    }

    if (targets.include_stdout) {
      std::ostream& out = output_ != nullptr ? *output_ : std::cout;
      out << formatted;
    }
    for (int i = 0; i < targets.file_stream_count; ++i) {
      *targets.file_streams.at(i) << formatted;
    }
  } else {
    // Direct stdout output (original $display/$write path)
    std::ostream& out = output_ != nullptr ? *output_ : std::cout;
    out << FormatDisplayOps(state, disp.ops);
    if (disp.print_kind == PrintKind::kDisplay) {
      out << "\n";
    }
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

void Interpreter::ExecMemIOEffect(
    ProcessState& state, const MemIOEffect& mem_io) {
  // Evaluate filename
  RuntimeValue filename_val = EvalOperand(state, mem_io.filename);
  if (!IsString(filename_val)) {
    throw std::runtime_error("$readmem/$writemem: filename must be a string");
  }
  const std::string& filename = AsString(filename_val).value;

  // Get array type info
  const Type& arr_type = (*types_)[mem_io.target_type];
  if (arr_type.Kind() != TypeKind::kUnpackedArray) {
    throw std::runtime_error(
        "$readmem/$writemem: target must be an unpacked array");
  }
  const auto& arr_info = arr_type.AsUnpackedArray();
  const Type& elem_type = (*types_)[arr_info.element_type];
  if (!IsPacked(elem_type)) {
    throw std::runtime_error(
        "$readmem/$writemem: array element must be a packed type");
  }
  uint32_t elem_width = PackedBitWidth(elem_type, *types_);
  auto elem_count = static_cast<int64_t>(arr_info.range.Size());
  int64_t min_addr = arr_info.range.Lower();
  int64_t max_addr = min_addr + elem_count - 1;

  // Evaluate optional start/end address
  int64_t start_addr = min_addr;
  int64_t end_addr = max_addr;
  if (mem_io.start_addr) {
    RuntimeValue addr_val = EvalOperand(state, *mem_io.start_addr);
    if (!IsIntegral(addr_val)) {
      throw std::runtime_error("$readmem/$writemem: address must be integral");
    }
    const auto& addr_int = AsIntegral(addr_val);
    if (!addr_int.IsKnown()) {
      throw std::runtime_error("$readmem/$writemem: address contains X/Z bits");
    }
    start_addr = static_cast<int64_t>(addr_int.value[0]);
  }
  if (mem_io.end_addr) {
    RuntimeValue addr_val = EvalOperand(state, *mem_io.end_addr);
    if (!IsIntegral(addr_val)) {
      throw std::runtime_error("$readmem/$writemem: address must be integral");
    }
    const auto& addr_int = AsIntegral(addr_val);
    if (!addr_int.IsKnown()) {
      throw std::runtime_error("$readmem/$writemem: address contains X/Z bits");
    }
    end_addr = static_cast<int64_t>(addr_int.value[0]);
  }

  auto resolved_path = common::mem_io::ResolveMemPath(filename);
  auto get_task_name = [&]() -> std::string_view {
    if (mem_io.is_read) {
      return mem_io.is_hex ? "$readmemh" : "$readmemb";
    }
    return mem_io.is_hex ? "$writememh" : "$writememb";
  };
  std::string_view task_name = get_task_name();

  if (mem_io.is_read) {
    // Read the file
    std::ifstream file(resolved_path);
    if (!file.is_open()) {
      throw std::runtime_error(
          std::format("{}: cannot open file '{}'", task_name, filename));
    }
    std::string content(
        (std::istreambuf_iterator<char>(file)),
        std::istreambuf_iterator<char>());

    // Read current array value
    RuntimeValue arr_val = ReadPlace(state, mem_io.target);
    if (!IsArray(arr_val)) {
      throw std::runtime_error(
          std::format("{}: target is not an array", task_name));
    }
    auto& arr = AsArray(arr_val);

    int64_t current_addr = start_addr;
    auto result = common::mem_io::ParseMemFile(
        content, mem_io.is_hex, min_addr, max_addr, current_addr, end_addr,
        task_name, [&](std::string_view token, int64_t addr) {
          auto words_result = common::mem_io::ParseMemTokenToWords(
              token, elem_width, mem_io.is_hex);
          if (!words_result) {
            throw std::runtime_error(
                std::format("{}: {}", task_name, words_result.error()));
          }
          // Convert words to RuntimeIntegral
          IntegralConstant ic;
          ic.value = std::move(*words_result);
          ic.unknown.resize(ic.value.size(), 0);
          RuntimeValue elem_val = MakeIntegralFromConstant(ic, elem_width);

          // Store to array element
          auto index = static_cast<size_t>(addr - min_addr);
          if (index < arr.elements.size()) {
            arr.elements[index] = std::move(elem_val);
          }
        });

    if (!result.success) {
      throw std::runtime_error(std::format("{}: {}", task_name, result.error));
    }

    // Write back the modified array
    StoreToPlace(state, mem_io.target, std::move(arr_val));
  } else {
    // Write mode: read array and write to file
    RuntimeValue arr_val = ReadPlace(state, mem_io.target);
    if (!IsArray(arr_val)) {
      throw std::runtime_error(
          std::format("{}: target is not an array", task_name));
    }
    const auto& arr = AsArray(arr_val);

    std::ofstream file(resolved_path);
    if (!file.is_open()) {
      throw std::runtime_error(
          std::format(
              "{}: cannot open file '{}' for writing", task_name, filename));
    }

    for (int64_t addr = start_addr; addr <= end_addr; ++addr) {
      auto index = static_cast<size_t>(addr - min_addr);
      if (index >= arr.elements.size()) {
        break;
      }
      const auto& elem = arr.elements[index];
      if (!IsIntegral(elem)) {
        continue;
      }
      const auto& integral = AsIntegral(elem);
      std::vector<uint64_t> words = integral.value;
      words.resize((elem_width + 63) / 64, 0);
      std::string formatted =
          common::mem_io::FormatMemWords(words, elem_width, mem_io.is_hex);
      file << formatted << "\n";
    }
  }
}

void Interpreter::ExecFcloseEffect(
    ProcessState& state, const FcloseEffect& effect) {
  RuntimeValue desc_val = EvalOperand(state, effect.descriptor);
  if (!IsIntegral(desc_val)) {
    return;  // Invalid descriptor: no-op
  }
  const auto& desc_int = AsIntegral(desc_val);
  auto descriptor =
      static_cast<int32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);
  file_manager_.Fclose(descriptor);
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
        } else {
          throw common::UnsupportedErrorException(
              common::UnsupportedError{
                  .layer = common::UnsupportedLayer::kExecution,
                  .kind = common::UnsupportedKind::kFeature,
                  .origin = inst.origin,
                  .detail = "non-blocking assignments require the LLVM backend "
                            "(use --backend=llvm)",
              });
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

          [&](const Return& t) -> std::optional<BasicBlockId> {
            // If return has a value (function return), evaluate and store it
            if (t.value.has_value()) {
              state.function_return_value = Clone(EvalOperand(state, *t.value));
            }
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
