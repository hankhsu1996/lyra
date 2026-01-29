#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/memfile.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/interp/format.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/runtime_real_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/file_manager.hpp"

namespace lyra::mir::interp {

auto Interpreter::Run(ProcessState& state) -> Result<ProcessStatus> {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

    // Execute all instructions in block
    while (state.instruction_index < block.instructions.size()) {
      auto result =
          ExecInstruction(state, block.instructions[state.instruction_index]);
      if (!result) {
        return std::unexpected(std::move(result).error());
      }
      state.instruction_index++;
    }

    // Execute terminator
    auto next_result = ExecTerminator(state, block.terminator);
    if (!next_result) {
      return std::unexpected(std::move(next_result).error());
    }
    auto next = *next_result;
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
    DesignState* design_state) -> Result<RuntimeValue> {
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
      auto result = ExecInstruction(
          func_state, block.instructions[func_state.instruction_index]);
      if (!result) {
        return std::unexpected(std::move(result).error());
      }
      func_state.instruction_index++;
    }

    auto next_result = ExecTerminator(func_state, block.terminator);
    if (!next_result) {
      return std::unexpected(std::move(next_result).error());
    }
    auto next = *next_result;
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

auto Interpreter::ExecAssign(ProcessState& state, const Assign& assign)
    -> Result<void> {
  auto value_result = EvalOperand(state, assign.source);
  if (!value_result) {
    return std::unexpected(std::move(value_result).error());
  }
  return StoreToPlace(state, assign.target, std::move(*value_result));
}

auto Interpreter::ExecCompute(ProcessState& state, const Compute& compute)
    -> Result<void> {
  TypeId result_type = TypeOfPlace(*types_, (*arena_)[compute.target]);
  auto value_result = EvalRvalue(state, compute.value, result_type);
  if (!value_result) {
    return std::unexpected(std::move(value_result).error());
  }
  return StoreToPlace(state, compute.target, std::move(*value_result));
}

auto Interpreter::ExecEffect(ProcessState& state, const Effect& effect)
    -> Result<void> {
  std::optional<Diagnostic> error;

  std::visit(
      common::Overloaded{
          [&](const DisplayEffect& op) {
            if (error) {
              return;
            }
            auto result = ExecDisplayEffect(state, op);
            if (!result) {
              error = std::move(result).error();
            }
          },
          [&](const SeverityEffect& op) {
            if (error) {
              return;
            }
            auto result = ExecSeverityEffect(state, op);
            if (!result) {
              error = std::move(result).error();
            }
          },
          [&](const MemIOEffect& op) {
            if (error) {
              return;
            }
            auto result = ExecMemIOEffect(state, op);
            if (!result) {
              error = std::move(result).error();
            }
          },
          [&](const TimeFormatEffect&) {
            // $timeformat not supported in MIR interpreter (debug-only backend)
          },
          [&](const SystemTfEffect& op) {
            if (error) {
              return;
            }
            auto result = ExecSystemTfEffect(state, op);
            if (!result) {
              error = std::move(result).error();
            }
          },
          [&](const StrobeEffect&) {
            // $strobe not supported in MIR interpreter (requires Postponed
            // region scheduling). Use LLVM backend for full $strobe semantics.
          },
          [&](const MonitorEffect&) {
            // $monitor not supported in MIR interpreter (requires Postponed
            // region scheduling). Use LLVM backend for full $monitor semantics.
          },
          [&](const MonitorControlEffect&) {
            // $monitoron/$monitoroff not supported in MIR interpreter.
            // Use LLVM backend for full monitor control.
          },
      },
      effect.op);

  if (error) {
    return std::unexpected(std::move(*error));
  }
  return {};
}

auto Interpreter::FormatDisplayOps(
    const ProcessState& state, std::span<const FormatOp> ops)
    -> Result<std::string> {
  std::string result;
  FormatContext ctx{};

  for (const auto& op : ops) {
    switch (op.kind) {
      case FormatKind::kLiteral:
        result += op.literal;
        break;

      case FormatKind::kModulePath: {
        // %m: emit hierarchical instance path (no argument consumed)
        if (state.instance_id >= instance_paths_.size()) {
          throw common::InternalError(
              "FormatDisplayOps",
              std::format(
                  "invalid instance_id {} (max {})", state.instance_id,
                  instance_paths_.size()));
        }
        result += instance_paths_[state.instance_id];
        break;
      }

      case FormatKind::kDecimal:
      case FormatKind::kHex:
      case FormatKind::kBinary:
      case FormatKind::kOctal:
      case FormatKind::kString:
      case FormatKind::kReal:
      case FormatKind::kTime:
      case FormatKind::kChar: {
        // Value-consuming format kinds
        if (!op.value.has_value()) {
          throw common::InternalError(
              "FormatDisplayOps",
              std::format(
                  "format kind {} requires operand but has none",
                  static_cast<int>(op.kind)));
        }
        auto value_result = EvalOperand(state, *op.value);
        if (!value_result) {
          return std::unexpected(std::move(value_result).error());
        }
        TypedValue typed{.value = std::move(*value_result), .type = op.type};

        FormatSpec spec{
            .kind = op.kind,
            .width = op.mods.width,
            .precision = op.mods.precision,
            .zero_pad = op.mods.zero_pad,
            .left_align = op.mods.left_align};

        result += FormatValue(typed, spec, *types_, ctx);
        break;
      }
    }
  }

  return result;
}

auto Interpreter::ExecDisplayEffect(
    const ProcessState& state, const DisplayEffect& disp) -> Result<void> {
  if (disp.descriptor) {
    // File-directed output: evaluate descriptor and route through FileManager
    auto desc_val_result = EvalOperand(state, *disp.descriptor);
    if (!desc_val_result) {
      return std::unexpected(std::move(desc_val_result).error());
    }
    auto& desc_val = *desc_val_result;
    if (!IsIntegral(desc_val)) {
      return {};  // Invalid descriptor: no-op
    }
    const auto& desc_int = AsIntegral(desc_val);
    auto udesc =
        static_cast<uint32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);

    runtime::StreamTargets targets = file_manager_.CollectStreams(udesc);
    if (!targets.include_stdout && targets.file_stream_count == 0) {
      return {};  // No targets: no-op
    }

    auto formatted_result = FormatDisplayOps(state, disp.ops);
    if (!formatted_result) {
      return std::unexpected(std::move(formatted_result).error());
    }
    std::string formatted = std::move(*formatted_result);
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
    auto formatted_result = FormatDisplayOps(state, disp.ops);
    if (!formatted_result) {
      return std::unexpected(std::move(formatted_result).error());
    }
    out << *formatted_result;
    if (disp.print_kind == PrintKind::kDisplay) {
      out << "\n";
    }
  }
  return {};
}

auto Interpreter::ExecSeverityEffect(
    const ProcessState& state, const SeverityEffect& severity) -> Result<void> {
  std::ostream& out = output_ != nullptr ? *output_ : std::cout;

  // Print severity prefix (use shared constant)
  out << SeverityPrefixCStr(severity.level);

  // Format the message using the same path as display (FormatOps)
  auto formatted_result = FormatDisplayOps(state, severity.ops);
  if (!formatted_result) {
    return std::unexpected(std::move(formatted_result).error());
  }

  out << *formatted_result << "\n";
  return {};
}

auto Interpreter::ExecMemIOEffect(
    ProcessState& state, const MemIOEffect& mem_io) -> Result<void> {
  // Evaluate filename
  auto filename_val_result = EvalOperand(state, mem_io.filename);
  if (!filename_val_result) {
    return std::unexpected(std::move(filename_val_result).error());
  }
  auto& filename_val = *filename_val_result;
  if (!IsString(filename_val)) {
    throw common::InternalError("ExecMemIOEffect", "filename must be a string");
  }
  const std::string& filename = AsString(filename_val).value;

  // Get array type info
  const Type& arr_type = (*types_)[mem_io.target_type];
  if (arr_type.Kind() != TypeKind::kUnpackedArray) {
    throw common::InternalError(
        "ExecMemIOEffect", "target must be an unpacked array");
  }
  const auto& arr_info = arr_type.AsUnpackedArray();
  const Type& elem_type = (*types_)[arr_info.element_type];
  if (!IsPacked(elem_type)) {
    throw common::InternalError(
        "ExecMemIOEffect", "array element must be a packed type");
  }
  uint32_t elem_width = PackedBitWidth(elem_type, *types_);
  auto elem_count = static_cast<int64_t>(arr_info.range.Size());
  int64_t min_addr = arr_info.range.Lower();
  int64_t max_addr = min_addr + elem_count - 1;

  // Determine step from array direction
  int64_t step = arr_info.range.IsDescending() ? -1 : 1;

  // Evaluate optional start/end address
  // Default: start from left bound, end at right bound (IEEE 1800 semantics)
  int64_t start_addr = arr_info.range.left;
  int64_t end_addr = arr_info.range.right;
  if (mem_io.start_addr) {
    auto addr_val_result = EvalOperand(state, *mem_io.start_addr);
    if (!addr_val_result) {
      return std::unexpected(std::move(addr_val_result).error());
    }
    auto& addr_val = *addr_val_result;
    if (!IsIntegral(addr_val)) {
      throw common::InternalError(
          "ExecMemIOEffect", "address must be integral");
    }
    const auto& addr_int = AsIntegral(addr_val);
    if (!addr_int.IsKnown()) {
      return std::unexpected(
          Diagnostic::HostError(
              "$readmem/$writemem: address contains X/Z bits"));
    }
    start_addr = static_cast<int64_t>(addr_int.value[0]);
  }
  if (mem_io.end_addr) {
    auto addr_val_result = EvalOperand(state, *mem_io.end_addr);
    if (!addr_val_result) {
      return std::unexpected(std::move(addr_val_result).error());
    }
    auto& addr_val = *addr_val_result;
    if (!IsIntegral(addr_val)) {
      throw common::InternalError(
          "ExecMemIOEffect", "address must be integral");
    }
    const auto& addr_int = AsIntegral(addr_val);
    if (!addr_int.IsKnown()) {
      return std::unexpected(
          Diagnostic::HostError(
              "$readmem/$writemem: address contains X/Z bits"));
    }
    end_addr = static_cast<int64_t>(addr_int.value[0]);
  }

  // Compute current/final consistent with step direction
  int64_t eff_low = std::min(start_addr, end_addr);
  int64_t eff_high = std::max(start_addr, end_addr);
  int64_t current_addr = step > 0 ? eff_low : eff_high;
  int64_t final_addr = step > 0 ? eff_high : eff_low;

  // Resolve path relative to fs_base_dir
  std::filesystem::path resolved_path{filename};
  if (resolved_path.is_relative()) {
    resolved_path = fs_base_dir_ / resolved_path;
  }
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
      return std::unexpected(
          Diagnostic::HostError(
              std::format("{}: cannot open file '{}'", task_name, filename)));
    }
    std::string content(
        (std::istreambuf_iterator<char>(file)),
        std::istreambuf_iterator<char>());

    // Read current array value
    auto arr_val_result = ReadPlace(state, mem_io.target);
    if (!arr_val_result) {
      return std::unexpected(std::move(arr_val_result).error());
    }
    auto& arr_val = *arr_val_result;
    if (!IsArray(arr_val)) {
      throw common::InternalError("ExecMemIOEffect", "target is not an array");
    }
    auto& arr = AsArray(arr_val);

    // Error captured from callback
    std::optional<Diagnostic> callback_error;

    // Store element callback
    auto store_element = [&](std::string_view token, int64_t addr) {
      if (callback_error) {
        return;  // Already have an error
      }
      auto words_result =
          common::ParseMemTokenToWords(token, elem_width, mem_io.is_hex);
      if (!words_result) {
        callback_error = Diagnostic::HostError(
            std::format("{}: {}", task_name, words_result.error()));
        return;
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
    };

    // Use unified ParseMemFile with step parameter (handles both directions)
    auto result = common::ParseMemFile(
        content, mem_io.is_hex, min_addr, max_addr, current_addr, final_addr,
        step, task_name, store_element);

    if (callback_error) {
      return std::unexpected(std::move(*callback_error));
    }

    if (!result.success) {
      return std::unexpected(
          Diagnostic::HostError(
              std::format("{}: {}", task_name, result.error)));
    }

    // Write back the modified array
    return StoreToPlace(state, mem_io.target, std::move(arr_val));
  }

  // Write mode: read array and write to file
  auto arr_val_result = ReadPlace(state, mem_io.target);
  if (!arr_val_result) {
    return std::unexpected(std::move(arr_val_result).error());
  }
  auto& arr_val = *arr_val_result;
  if (!IsArray(arr_val)) {
    throw common::InternalError("ExecMemIOEffect", "target is not an array");
  }
  const auto& arr = AsArray(arr_val);

  std::ofstream file(resolved_path);
  if (!file.is_open()) {
    return std::unexpected(
        Diagnostic::HostError(
            std::format(
                "{}: cannot open file '{}' for writing", task_name, filename)));
  }

  // Write each element with direction-aware iteration
  int64_t addr = current_addr;
  while (step > 0 ? addr <= final_addr : addr >= final_addr) {
    auto index = static_cast<size_t>(addr - min_addr);
    if (index < arr.elements.size()) {
      const auto& elem = arr.elements[index];
      if (IsIntegral(elem)) {
        const auto& integral = AsIntegral(elem);
        std::vector<uint64_t> words = integral.value;
        words.resize((elem_width + 63) / 64, 0);
        std::string formatted =
            common::FormatMemWords(words, elem_width, mem_io.is_hex);
        file << formatted << "\n";
      }
    }
    addr += step;
  }
  return {};
}

auto Interpreter::ExecSystemTfEffect(
    ProcessState& state, const SystemTfEffect& effect) -> Result<void> {
  switch (effect.opcode) {
    case SystemTfOpcode::kFclose: {
      if (effect.args.size() != 1) {
        throw common::InternalError(
            "ExecSystemTfEffect:kFclose",
            std::format("expected 1 arg, got {}", effect.args.size()));
      }
      auto desc_val_result = EvalOperand(state, effect.args[0]);
      if (!desc_val_result) {
        return std::unexpected(std::move(desc_val_result).error());
      }
      auto& desc_val = *desc_val_result;
      if (!IsIntegral(desc_val)) {
        return {};  // Invalid descriptor: no-op
      }
      const auto& desc_int = AsIntegral(desc_val);
      auto descriptor =
          static_cast<int32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);
      file_manager_.Fclose(descriptor);
      return {};
    }
    case SystemTfOpcode::kFflush: {
      if (effect.args.empty()) {
        // Flush all
        file_manager_.Fflush(std::nullopt);
      } else {
        auto desc_val_result = EvalOperand(state, effect.args[0]);
        if (!desc_val_result) {
          return std::unexpected(std::move(desc_val_result).error());
        }
        auto& desc_val = *desc_val_result;
        if (!IsIntegral(desc_val)) {
          return {};  // Invalid descriptor: no-op
        }
        const auto& desc_int = AsIntegral(desc_val);
        auto descriptor = static_cast<int32_t>(
            desc_int.value.empty() ? 0 : desc_int.value[0]);
        file_manager_.Fflush(descriptor);
      }
      return {};
    }
    case SystemTfOpcode::kFopen:
      throw common::InternalError(
          "ExecSystemTfEffect", "$fopen is an rvalue, not an effect");
  }
  throw common::InternalError("ExecSystemTfEffect", "unhandled SystemTfOpcode");
}

auto Interpreter::ExecGuardedAssign(
    ProcessState& state, const GuardedAssign& guarded) -> Result<void> {
  // Always evaluate source unconditionally (per spec: only the write is
  // guarded)
  auto value_result = EvalOperand(state, guarded.source);
  if (!value_result) {
    return std::unexpected(std::move(value_result).error());
  }

  // Evaluate validity predicate
  auto validity_result = EvalOperand(state, guarded.validity);
  if (!validity_result) {
    return std::unexpected(std::move(validity_result).error());
  }
  auto& validity = *validity_result;
  if (!IsIntegral(validity)) {
    throw common::InternalError(
        "ExecGuardedAssign", "validity must be integral");
  }
  const auto& valid_int = AsIntegral(validity);

  // If invalid (OOB/X/Z), the write is a no-op
  if (valid_int.IsZero()) {
    return {};
  }

  // Valid: perform the assignment
  return StoreToPlace(state, guarded.target, std::move(*value_result));
}

auto Interpreter::ExecInstruction(ProcessState& state, const Instruction& inst)
    -> Result<void> {
  std::optional<Diagnostic> error;

  std::visit(
      [&](const auto& i) {
        if (error) {
          return;
        }
        using T = std::decay_t<decltype(i)>;
        if constexpr (std::is_same_v<T, Assign>) {
          auto result = ExecAssign(state, i);
          if (!result) {
            error = std::move(result).error();
          }
        } else if constexpr (std::is_same_v<T, Compute>) {
          auto result = ExecCompute(state, i);
          if (!result) {
            error = std::move(result).error();
          }
        } else if constexpr (std::is_same_v<T, GuardedAssign>) {
          auto result = ExecGuardedAssign(state, i);
          if (!result) {
            error = std::move(result).error();
          }
        } else if constexpr (std::is_same_v<T, Effect>) {
          auto result = ExecEffect(state, i);
          if (!result) {
            error = std::move(result).error();
          }
        } else {
          // Non-blocking assignments require the LLVM backend runtime
          if (diag_ctx_ != nullptr) {
            error = diag_ctx_->MakeUnsupported(
                inst.origin,
                "non-blocking assignments require the LLVM backend "
                "(use --backend=llvm)",
                UnsupportedCategory::kFeature);
          } else {
            error = Diagnostic{
                .primary =
                    {.kind = DiagKind::kUnsupported,
                     .span = UnknownSpan{},
                     .message =
                         "non-blocking assignments require the LLVM backend "
                         "(use --backend=llvm)",
                     .category = UnsupportedCategory::kFeature},
                .notes = {},
            };
          }
        }
      },
      inst.data);

  if (error) {
    return std::unexpected(std::move(*error));
  }
  return {};
}

auto Interpreter::ExecTerminator(ProcessState& state, const Terminator& term)
    -> Result<std::optional<BasicBlockId>> {
  using ResultType = Result<std::optional<BasicBlockId>>;

  return std::visit(
      common::Overloaded{
          [](const Jump& t) -> ResultType { return t.target; },

          [&](const Branch& t) -> ResultType {
            auto cond_result = ReadPlace(state, t.condition);
            if (!cond_result) {
              return std::unexpected(std::move(cond_result).error());
            }
            auto& cond = *cond_result;
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
              // TODO(hankhsu): SV semantics - should return X, not throw
              return std::unexpected(
                  Diagnostic::HostError("branch condition is X/Z"));
            }
            return cond_int.IsZero() ? t.else_target : t.then_target;
          },

          [&](const Switch& t) -> ResultType {
            if (t.targets.empty()) {
              throw common::InternalError(
                  "ExecTerminator", "switch terminator has no targets");
            }
            auto selector_result = ReadPlace(state, t.selector);
            if (!selector_result) {
              return std::unexpected(std::move(selector_result).error());
            }
            auto& selector = *selector_result;
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

          [&](const QualifiedDispatch& t) -> ResultType {
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
              auto cond_result = ReadPlace(state, t.conditions[i]);
              if (!cond_result) {
                return std::unexpected(std::move(cond_result).error());
              }
              auto& cond = *cond_result;
              if (!IsIntegral(cond)) {
                throw common::InternalError(
                    "ExecTerminator",
                    "QualifiedDispatch condition must be integral");
              }
              const auto& cond_int = AsIntegral(cond);
              // Return error on X/Z conditions (same as Branch)
              if (!cond_int.IsKnown()) {
                // TODO(hankhsu): SV semantics - should return X, not error
                return std::unexpected(
                    Diagnostic::HostError(
                        "QualifiedDispatch condition is X/Z"));
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

          [&](const Delay& t) -> ResultType {
            // Signal suspension - RunUntilSuspend will return SuspendDelay
            state.pending_suspend = SuspendDelay{
                .ticks = t.ticks,
                .resume_block = t.resume,
            };
            return std::nullopt;
          },

          [](const Wait& /*t*/) -> ResultType {
            throw common::InternalError(
                "ExecTerminator", "wait terminator requires runtime/scheduler");
          },

          [&](const Return& t) -> ResultType {
            // If return has a value (function return), evaluate and store it
            if (t.value.has_value()) {
              auto val_result = EvalOperand(state, *t.value);
              if (!val_result) {
                return std::unexpected(std::move(val_result).error());
              }
              state.function_return_value = Clone(*val_result);
            }
            return std::nullopt;
          },

          [&](const Finish& t) -> ResultType {
            std::ostream& out = output_ != nullptr ? *output_ : std::cout;

            if (t.kind == TerminationKind::kFatal) {
              if (t.level >= 1) {
                out << "fatal: ";
                if (t.message.has_value()) {
                  // Message is already a formatted string from SFormat
                  auto msg_result = EvalOperand(state, *t.message);
                  if (!msg_result) {
                    return std::unexpected(std::move(msg_result).error());
                  }
                  if (IsString(*msg_result)) {
                    out << AsString(*msg_result).value;
                  }
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
              out << name << " called at time " << GetSimulationTime() << "\n";
            }
            return std::nullopt;
          },

          [](const Repeat& /*t*/) -> ResultType {
            throw common::InternalError(
                "ExecTerminator",
                "repeat terminator requires runtime/scheduler");
          },
      },
      term.data);
}

auto Interpreter::RunUntilSuspend(ProcessState& state)
    -> Result<SuspendReason> {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

    // Execute all instructions in block
    while (state.instruction_index < block.instructions.size()) {
      auto result =
          ExecInstruction(state, block.instructions[state.instruction_index]);
      if (!result) {
        return std::unexpected(std::move(result).error());
      }
      state.instruction_index++;
    }

    // Execute terminator
    auto next_block_result = ExecTerminator(state, block.terminator);
    if (!next_block_result) {
      return std::unexpected(std::move(next_block_result).error());
    }
    auto next_block = *next_block_result;

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
