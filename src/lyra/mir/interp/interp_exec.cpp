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

#include "lyra/common/binary_pack.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/memfile.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/plusargs.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/system_tf.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/interp/format.hpp"
#include "lyra/mir/interp/frame_temps.hpp"
#include "lyra/mir/interp/interp_helpers.hpp"
#include "lyra/mir/interp/interpreter.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_real_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/routine.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/terminator.hpp"
#include "lyra/runtime/file_manager.hpp"

namespace lyra::mir::interp {

auto Interpreter::Run(ProcessState& state) -> Result<ProcessStatus> {
  while (state.status == ProcessStatus::kRunning) {
    const auto& process = (*arena_)[state.process];
    const auto& block = process.blocks[state.current_block.value];

    // Execute all instructions in block
    while (state.instruction_index < block.statements.size()) {
      auto result =
          ExecStatement(state, block.statements[state.instruction_index]);
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
    auto& term_result = *next_result;
    if (!term_result.target) {
      state.status = ProcessStatus::kFinished;
      break;
    }

    // Bind edge args to target block's params before entering
    const auto& next_block = process.blocks[term_result.target->value];
    BindBlockParams(state, next_block, std::move(term_result.edge_args));

    state.current_block = *term_result.target;
    state.instruction_index = 0;
  }

  return state.status;
}

auto Interpreter::RunFunction(
    FunctionId func_id, const std::vector<RuntimeValue>& args,
    DesignState* design_state) -> Result<FunctionCallResult> {
  const auto& func = (*arena_)[func_id];

  // Use function's metadata for storage allocation
  const auto& local_types = func.local_types;

  // Initialize locals with default values
  std::vector<RuntimeValue> locals;
  locals.reserve(local_types.size());
  for (TypeId type_id : local_types) {
    locals.push_back(
        type_id ? CreateDefaultValue(*types_, type_id) : RuntimeValue{});
  }

  // Initialize temps from canonical func.temp_metadata
  auto [temps, temp_types] = BuildFrameTemps(func.temp_metadata, *types_);

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
      .frame =
          Frame(std::move(locals), std::move(temps), std::move(temp_types)),
      .design_state = design_state,
      .status = ProcessStatus::kRunning,
      .pending_suspend = std::nullopt,
      .function_return_value = std::nullopt,
  };

  // Execute function (look up blocks from func directly)
  while (func_state.status == ProcessStatus::kRunning) {
    const auto& block = func.blocks[func_state.current_block.value];

    while (func_state.instruction_index < block.statements.size()) {
      auto result = ExecStatement(
          func_state, block.statements[func_state.instruction_index]);
      if (!result) {
        return std::unexpected(std::move(result).error());
      }
      func_state.instruction_index++;
    }

    auto next_result = ExecTerminator(func_state, block.terminator);
    if (!next_result) {
      return std::unexpected(std::move(next_result).error());
    }
    auto& term_result = *next_result;
    if (!term_result.target) {
      break;  // Return reached
    }

    // Bind edge args to target block's params before entering
    const auto& next_block = func.blocks[term_result.target->value];
    BindBlockParams(func_state, next_block, std::move(term_result.edge_args));

    func_state.current_block = *term_result.target;
    func_state.instruction_index = 0;
  }

  // Collect output values for kOut/kInOut parameters
  std::vector<std::pair<size_t, RuntimeValue>> outputs;
  for (size_t i = 0; i < func.signature.params.size(); ++i) {
    if (func.signature.params[i].kind != PassingKind::kValue) {
      uint32_t local_slot = func.param_local_slots[i];
      outputs.emplace_back(
          i, Clone(func_state.frame.GetLocal(static_cast<int>(local_slot))));
    }
  }

  // Build result
  FunctionCallResult result;
  result.outputs = std::move(outputs);

  if (func_state.function_return_value.has_value()) {
    result.return_value = std::move(*func_state.function_return_value);
  } else {
    result.return_value = std::monostate{};
  }

  return result;
}

auto Interpreter::ExecAssign(ProcessState& state, const Assign& assign)
    -> Result<void> {
  // Dispatch on RightHandSide: Operand or Rvalue
  return std::visit(
      common::Overloaded{
          [&](const Operand& operand) -> Result<void> {
            auto value_result = EvalOperand(state, operand);
            if (!value_result) {
              return std::unexpected(std::move(value_result).error());
            }
            return StoreToPlace(state, assign.dest, std::move(*value_result));
          },
          [&](const Rvalue& rvalue) -> Result<void> {
            TypeId result_type = TypeOfPlace(*types_, (*arena_)[assign.dest]);
            auto value_result = EvalRvalue(state, rvalue, result_type);
            if (!value_result) {
              return std::unexpected(std::move(value_result).error());
            }
            return StoreToPlace(state, assign.dest, std::move(*value_result));
          },
      },
      assign.rhs);
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
          [&](const FillPackedEffect& op) {
            if (error) {
              return;
            }
            auto result = ExecFillPackedEffect(state, op);
            if (!result) {
              error = std::move(result).error();
            }
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
  // Evaluate filename and coerce to string if packed
  auto filename_val_result = EvalOperand(state, mem_io.filename.operand);
  if (!filename_val_result) {
    return std::unexpected(std::move(filename_val_result).error());
  }
  std::string filename =
      CoerceToString(*filename_val_result, "ExecMemIOEffect");

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
    auto store_result = StoreToPlace(state, mem_io.target, std::move(arr_val));
    if (!store_result) return store_result;

    // Emit MemoryDirty trace event
    if (trace_manager_ != nullptr && trace_manager_->IsEnabled()) {
      const auto& place = (*arena_)[mem_io.target];
      if (place.root.kind == mir::PlaceRoot::Kind::kDesign) {
        trace_manager_->EmitMemoryDirty(static_cast<uint32_t>(place.root.id));
      }
    }
    return {};
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
    case SystemTfOpcode::kValuePlusargs:
      throw common::InternalError(
          "ExecSystemTfEffect",
          "$value$plusargs should be lowered via Call, not SystemTfEffect");
    case SystemTfOpcode::kRandom:
      throw common::InternalError(
          "ExecSystemTfEffect", "$random is an rvalue, not an effect");
    case SystemTfOpcode::kUrandom:
      throw common::InternalError(
          "ExecSystemTfEffect", "$urandom is an rvalue, not an effect");
    case SystemTfOpcode::kFgetc:
      throw common::InternalError(
          "ExecSystemTfEffect", "$fgetc is an rvalue, not an effect");
    case SystemTfOpcode::kUngetc:
      throw common::InternalError(
          "ExecSystemTfEffect", "$ungetc is an rvalue, not an effect");
    case SystemTfOpcode::kFgets:
      throw common::InternalError(
          "ExecSystemTfEffect",
          "$fgets should be lowered via Call, not SystemTfEffect");
    case SystemTfOpcode::kFread:
      throw common::InternalError(
          "ExecSystemTfEffect",
          "$fread should be lowered via Call, not SystemTfEffect");
    case SystemTfOpcode::kFscanf:
      throw common::InternalError(
          "ExecSystemTfEffect",
          "$fscanf should be lowered via Call, not SystemTfEffect");
  }
  throw common::InternalError("ExecSystemTfEffect", "unhandled SystemTfOpcode");
}

auto Interpreter::ExecFillPackedEffect(
    ProcessState& state, const FillPackedEffect& fill) -> Result<void> {
  // Validate effect invariants
  if (fill.total_bits != fill.unit_bits * fill.count) {
    throw common::InternalError(
        "ExecFillPackedEffect",
        "invariant violation: total_bits != unit_bits * count");
  }

  // Evaluate the fill value once (already exactly unit_bits wide)
  auto fill_value_result = EvalOperand(state, fill.fill_value);
  if (!fill_value_result) {
    return std::unexpected(std::move(fill_value_result).error());
  }
  RuntimeValue fill_value = std::move(*fill_value_result);

  if (!IsIntegral(fill_value)) {
    throw common::InternalError(
        "ExecFillPackedEffect", "fill value must be integral");
  }
  const auto& fill_int = AsIntegral(fill_value);

  // Create filled value based on unit_bits (destination-driven semantics)
  RuntimeValue filled_value;

  if (fill.unit_bits == 1) {
    // Bit fill: replicate single bit to all positions
    bool value_bit = (fill_int.value[0] & 1) != 0;
    bool unknown_bit =
        !fill_int.unknown.empty() && ((fill_int.unknown[0] & 1) != 0);
    filled_value =
        semantic::MakeIntegralFilled(fill.total_bits, value_bit, unknown_bit);
  } else {
    // Element fill: replicate unit_bits-wide value count times
    RuntimeIntegral target_int = MakeKnownIntegral(fill.total_bits);
    for (uint32_t i = 0; i < fill.count; ++i) {
      uint32_t bit_offset = i * fill.unit_bits;
      IntegralInsertSlice4StateInPlace(
          target_int, fill_int, bit_offset, fill.unit_bits);
    }
    filled_value = std::move(target_int);
  }

  // Store the filled value to the target place
  return StoreToPlace(state, fill.target, std::move(filled_value));
}

auto Interpreter::ExecGuardedAssign(
    ProcessState& state, const GuardedAssign& guarded) -> Result<void> {
  // Always evaluate rhs unconditionally (per spec: only the write is
  // guarded). Dispatch on RightHandSide: Operand or Rvalue.
  auto value_result = std::visit(
      common::Overloaded{
          [&](const Operand& operand) -> Result<RuntimeValue> {
            return EvalOperand(state, operand);
          },
          [&](const Rvalue& rvalue) -> Result<RuntimeValue> {
            TypeId result_type = TypeOfPlace(*types_, (*arena_)[guarded.dest]);
            return EvalRvalue(state, rvalue, result_type);
          },
      },
      guarded.rhs);
  if (!value_result) {
    return std::unexpected(std::move(value_result).error());
  }

  // Evaluate guard predicate
  auto guard_result = EvalOperand(state, guarded.guard);
  if (!guard_result) {
    return std::unexpected(std::move(guard_result).error());
  }
  auto& guard = *guard_result;
  if (!IsIntegral(guard)) {
    throw common::InternalError("ExecGuardedAssign", "guard must be integral");
  }
  const auto& guard_int = AsIntegral(guard);

  // If guard is false (OOB/X/Z), the write is a no-op
  if (guard_int.IsZero()) {
    return {};
  }

  // Guard is true: perform the assignment
  return StoreToPlace(state, guarded.dest, std::move(*value_result));
}

auto Interpreter::ExecCall(ProcessState& state, const Call& call)
    -> Result<void> {
  // Dispatch based on callee type
  return std::visit(
      common::Overloaded{
          [&](FunctionId func_id) -> Result<void> {
            return ExecUserCall(state, call, func_id);
          },
          [&](SystemTfOpcode opcode) -> Result<void> {
            return ExecSystemTfCall(state, call, opcode);
          },
      },
      call.callee);
}

auto Interpreter::ExecUserCall(
    ProcessState& state, const Call& call, FunctionId func_id) -> Result<void> {
  const auto& func = (*arena_)[func_id];
  const auto& sig = func.signature;

  // Build full args vector in parameter order:
  // - kValue: from in_args
  // - kOut: default value (callee will write new value)
  // - kInOut: current value from destination (callee reads and may modify)
  std::vector<RuntimeValue> args;
  args.reserve(sig.params.size());
  size_t in_arg_idx = 0;

  for (size_t param_idx = 0; param_idx < sig.params.size(); ++param_idx) {
    const auto& param = sig.params[param_idx];

    if (param.kind == PassingKind::kValue) {
      // Input param: evaluate from in_args
      if (in_arg_idx >= call.in_args.size()) {
        throw common::InternalError("ExecUserCall", "in_args underflow");
      }
      auto arg_result = EvalOperand(state, call.in_args[in_arg_idx]);
      if (!arg_result) {
        return std::unexpected(std::move(arg_result).error());
      }
      args.push_back(std::move(*arg_result));
      ++in_arg_idx;
    } else {
      // Output/inout param: find writeback entry
      const mir::CallWriteback* wb = nullptr;
      for (const auto& w : call.writebacks) {
        if (static_cast<size_t>(w.arg_index) == param_idx) {
          wb = &w;
          break;
        }
      }
      if (wb == nullptr) {
        throw common::InternalError(
            "ExecUserCall",
            std::format("missing writeback for param {}", param_idx));
      }

      if (param.kind == PassingKind::kInOut) {
        // Inout: load current value from destination
        auto val_result = ReadPlace(state, wb->dest);
        if (!val_result) {
          return std::unexpected(std::move(val_result).error());
        }
        args.push_back(std::move(*val_result));
      } else {
        // Output: default value (callee will overwrite)
        args.push_back(CreateDefaultValue(*types_, param.type));
      }
    }
  }

  // Call the function
  auto result = RunFunction(func_id, args, state.design_state);
  if (!result) {
    return std::unexpected(std::move(result).error());
  }

  // Commit output values to destinations
  for (auto& [param_idx, output_val] : result->outputs) {
    // Find writeback for this param
    const mir::CallWriteback* wb = nullptr;
    for (const auto& w : call.writebacks) {
      if (static_cast<size_t>(w.arg_index) == param_idx) {
        wb = &w;
        break;
      }
    }
    if (wb == nullptr) {
      throw common::InternalError(
          "ExecUserCall",
          std::format("missing writeback for output param {}", param_idx));
    }

    // Stage to tmp first, then commit to dest
    auto store_tmp = StoreToPlace(state, *wb->tmp, Clone(output_val));
    if (!store_tmp) {
      return std::unexpected(std::move(store_tmp).error());
    }
    auto store_dest = StoreToPlace(state, wb->dest, std::move(output_val));
    if (!store_dest) {
      return std::unexpected(std::move(store_dest).error());
    }
  }

  // Stage return to tmp, then commit to dest (if statement form)
  if (call.ret) {
    // Store to tmp first (staging parity requirement)
    auto store_result =
        StoreToPlace(state, call.ret->tmp, Clone(result->return_value));
    if (!store_result) {
      return std::unexpected(std::move(store_result).error());
    }

    // Commit to dest if statement form - reuse same value
    if (call.ret->dest.has_value()) {
      return StoreToPlace(
          state, *call.ret->dest, std::move(result->return_value));
    }
    // Expression form: outer Assign reads from tmp (which now holds the value)
  }

  return {};
}

auto Interpreter::ExecSystemTfCall(
    ProcessState& state, const Call& call, SystemTfOpcode opcode)
    -> Result<void> {
  switch (opcode) {
    case SystemTfOpcode::kValuePlusargs:
      return ExecValuePlusargsCall(state, call);
    case SystemTfOpcode::kFgets:
      return ExecFgetsCall(state, call);
    case SystemTfOpcode::kFread:
      return ExecFreadCall(state, call);
    case SystemTfOpcode::kFscanf:
      return ExecFscanfCall(state, call);
    default:
      throw common::InternalError(
          "ExecSystemTfCall", std::format(
                                  "Unhandled system TF opcode in Call: {}",
                                  static_cast<int>(opcode)));
  }
}

auto Interpreter::ExecValuePlusargsCall(ProcessState& state, const Call& call)
    -> Result<void> {
  // Shape: in_args[0]=query, ret=success, writebacks[0]=output

  // Evaluate query operand (always a string)
  auto query_val_result = EvalOperand(state, call.in_args[0]);
  if (!query_val_result) {
    return std::unexpected(std::move(query_val_result).error());
  }
  auto& query_val = *query_val_result;
  if (!IsString(query_val)) {
    throw common::InternalError(
        "ExecValuePlusargsCall", "query operand is not a string");
  }
  std::string_view format = AsString(query_val).value;

  // Get output writeback info (kStaged: tmp is present)
  const auto& wb = call.writebacks[0];
  PlaceId output_tmp = *wb.tmp;
  PlaceId output_dest = wb.dest;

  // Parse format to determine type
  auto [prefix, spec] = common::ParsePlusargsFormat(format);

  // Try integer match (%d)
  if (spec == 'd' || spec == 'D') {
    int32_t parsed_value = 0;
    int32_t found = common::MatchPlusargsInt(plusargs_, format, &parsed_value);
    if (found != 0) {
      return CommitValuePlusargsResult(
          state, call, MakeIntegralSigned(1, 32),
          MakeIntegralSigned(parsed_value, 32), output_tmp, output_dest);
    }
    return CommitValuePlusargsResult(
        state, call, MakeIntegralSigned(0, 32), std::nullopt, output_tmp,
        output_dest);
  }

  // Try string match (%s)
  if (spec == 's' || spec == 'S') {
    std::string parsed_str;
    int32_t found = common::MatchPlusargsString(plusargs_, format, &parsed_str);
    if (found != 0) {
      return CommitValuePlusargsResult(
          state, call, MakeIntegralSigned(1, 32),
          MakeString(std::move(parsed_str)), output_tmp, output_dest);
    }
    return CommitValuePlusargsResult(
        state, call, MakeIntegralSigned(0, 32), std::nullopt, output_tmp,
        output_dest);
  }

  // No format specifier or unsupported - treat as test
  int32_t found = common::TestPlusargs(plusargs_, prefix);
  return CommitValuePlusargsResult(
      state, call, MakeIntegralSigned(found, 32), std::nullopt, output_tmp,
      output_dest);
}

auto Interpreter::CommitValuePlusargsResult(
    ProcessState& state, const Call& call, RuntimeValue success,
    std::optional<RuntimeValue> parsed_value, PlaceId output_tmp,
    PlaceId output_dest) -> Result<void> {
  // 1. Stage return (success) to tmp, then commit if statement form
  if (call.ret) {
    auto store_result = StoreToPlace(state, call.ret->tmp, Clone(success));
    if (!store_result) {
      return std::unexpected(std::move(store_result).error());
    }
    if (call.ret->dest.has_value()) {
      store_result = StoreToPlace(state, *call.ret->dest, std::move(success));
      if (!store_result) {
        return std::unexpected(std::move(store_result).error());
      }
    }
  }

  // 2. Stage writeback (output) to tmp, then commit to dest
  if (parsed_value) {
    auto store_result = StoreToPlace(state, output_tmp, Clone(*parsed_value));
    if (!store_result) {
      return std::unexpected(std::move(store_result).error());
    }
    return StoreToPlace(state, output_dest, std::move(*parsed_value));
  }

  return {};
}

auto Interpreter::ExecFgetsCall(ProcessState& state, const Call& call)
    -> Result<void> {
  // Shape: in_args[0]=descriptor, ret=char_count, writebacks[0]=str_output

  // Evaluate descriptor operand
  auto desc_val_result = EvalOperand(state, call.in_args[0]);
  if (!desc_val_result) {
    return std::unexpected(std::move(desc_val_result).error());
  }
  auto& desc_val = *desc_val_result;
  if (!IsIntegral(desc_val)) {
    // Invalid descriptor: return 0 (error)
    RuntimeValue zero = MakeIntegralSigned(0, 32);
    if (call.ret) {
      auto store_result = StoreToPlace(state, call.ret->tmp, Clone(zero));
      if (!store_result) {
        return std::unexpected(std::move(store_result).error());
      }
      if (call.ret->dest.has_value()) {
        return StoreToPlace(state, *call.ret->dest, std::move(zero));
      }
    }
    return {};
  }

  const auto& desc_int = AsIntegral(desc_val);
  auto descriptor =
      static_cast<int32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);

  // Call FileManager::Fgets
  std::string str_out;
  int32_t count = file_manager_.Fgets(descriptor, str_out);

  // Commit the results
  // 1. Stage return (count) to tmp, then commit if statement form
  RuntimeValue count_val = MakeIntegralSigned(count, 32);
  if (call.ret) {
    auto store_result = StoreToPlace(state, call.ret->tmp, Clone(count_val));
    if (!store_result) {
      return std::unexpected(std::move(store_result).error());
    }
    if (call.ret->dest.has_value()) {
      auto store_dest =
          StoreToPlace(state, *call.ret->dest, std::move(count_val));
      if (!store_dest) {
        return std::unexpected(std::move(store_dest).error());
      }
    }
  }

  // 2. Stage writeback (str_output) to tmp, then commit to dest
  const auto& wb = call.writebacks[0];
  RuntimeValue str_val = MakeString(std::move(str_out));
  auto store_tmp = StoreToPlace(state, *wb.tmp, Clone(str_val));
  if (!store_tmp) {
    return std::unexpected(std::move(store_tmp).error());
  }
  return StoreToPlace(state, wb.dest, std::move(str_val));
}

auto Interpreter::ExecFreadCall(ProcessState& state, const Call& call)
    -> Result<void> {
  // Shape: in_args[0]=descriptor, in_args[1]=element_width,
  // in_args[2]=is_memory
  //        in_args[3]=start, in_args[4]=count
  //        ret=bytes_read, writebacks[0]=target

  // Helper to return error (bytes_read = 0)
  auto return_error = [&]() -> Result<void> {
    RuntimeValue zero = MakeIntegralSigned(0, 32);
    if (call.ret) {
      auto store_result = StoreToPlace(state, call.ret->tmp, Clone(zero));
      if (!store_result) {
        return std::unexpected(std::move(store_result).error());
      }
      if (call.ret->dest.has_value()) {
        return StoreToPlace(state, *call.ret->dest, std::move(zero));
      }
    }
    return {};
  };

  // Evaluate descriptor
  auto desc_result = EvalOperand(state, call.in_args[0]);
  if (!desc_result) return std::unexpected(desc_result.error());
  if (!IsIntegral(*desc_result)) return return_error();
  const auto& desc_int = AsIntegral(*desc_result);
  auto descriptor =
      static_cast<int32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);

  // Evaluate element_width (compile-time constant)
  auto width_result = EvalOperand(state, call.in_args[1]);
  if (!width_result) return std::unexpected(width_result.error());
  auto element_width = static_cast<int32_t>(AsIntegral(*width_result).value[0]);

  // Evaluate is_memory (compile-time constant)
  auto is_mem_result = EvalOperand(state, call.in_args[2]);
  if (!is_mem_result) return std::unexpected(is_mem_result.error());
  bool is_memory = AsIntegral(*is_mem_result).value[0] != 0;

  // Evaluate start (-1 = not specified)
  auto start_result = EvalOperand(state, call.in_args[3]);
  if (!start_result) return std::unexpected(start_result.error());
  // Sign-extend from 32-bit: cast to int32_t first, then to int64_t
  auto start_index = static_cast<int64_t>(
      static_cast<int32_t>(AsIntegral(*start_result).value[0]));

  // Evaluate count (-1 = not specified)
  auto count_result = EvalOperand(state, call.in_args[4]);
  if (!count_result) return std::unexpected(count_result.error());
  // Sign-extend from 32-bit: cast to int32_t first, then to int64_t
  auto max_count = static_cast<int64_t>(
      static_cast<int32_t>(AsIntegral(*count_result).value[0]));

  // Calculate bytes per element (ceil(element_width / 8))
  size_t bytes_per_elem = (static_cast<size_t>(element_width) + 7) / 8;

  const auto& wb = call.writebacks[0];
  int32_t total_bytes_read = 0;

  if (!is_memory) {
    // Integral variant: read bytes_per_elem bytes into a single integral
    std::vector<uint8_t> buffer(bytes_per_elem);
    int32_t bytes_read =
        file_manager_.FreadBytes(descriptor, buffer.data(), bytes_per_elem);

    if (bytes_read > 0) {
      auto words = common::PackBigEndianToWords(
          std::span(buffer).first(static_cast<size_t>(bytes_read)),
          bytes_per_elem, static_cast<size_t>(element_width));
      RuntimeValue integral_val = MakeIntegralWide(
          words.data(), words.size(), static_cast<uint32_t>(element_width));
      total_bytes_read = bytes_read;

      // kStaged: stage to tmp, then commit to dest
      auto store_tmp = StoreToPlace(state, *wb.tmp, Clone(integral_val));
      if (!store_tmp) return std::unexpected(store_tmp.error());
      auto store_dest = StoreToPlace(state, wb.dest, std::move(integral_val));
      if (!store_dest) return std::unexpected(store_dest.error());
    }
  } else {
    // Memory variant (kDirectToDest): write directly to dest via WritePlace.
    // No tmp staging - follows the $readmemh bulk-write pattern.
    auto write_result = WritePlace(state, wb.dest);
    if (!write_result) return std::unexpected(write_result.error());
    if (!IsArray(write_result->get())) {
      return return_error();
    }
    auto& arr = AsArray(write_result->get());
    auto& elements = arr.elements;

    if (elements.empty()) {
      return return_error();  // Empty array
    }

    // Determine start and count
    size_t start_idx = (start_index < 0) ? 0 : static_cast<size_t>(start_index);
    size_t count =
        (max_count < 0) ? elements.size() : static_cast<size_t>(max_count);

    // Validate start
    if (start_idx >= elements.size()) {
      return return_error();  // OOB start
    }

    // Limit count to available elements
    size_t end_idx = std::min(start_idx + count, elements.size());

    std::vector<uint8_t> elem_buffer(bytes_per_elem);
    for (size_t idx = start_idx; idx < end_idx; ++idx) {
      // Read bytes for one element
      int32_t bytes_read = file_manager_.FreadBytes(
          descriptor, elem_buffer.data(), bytes_per_elem);

      if (static_cast<size_t>(bytes_read) < bytes_per_elem) {
        // Partial element - stop at element boundary (don't update this elem)
        break;
      }

      auto words = common::PackBigEndianToWords(
          std::span(elem_buffer), bytes_per_elem,
          static_cast<size_t>(element_width));
      elements[idx] = MakeIntegralWide(
          words.data(), words.size(), static_cast<uint32_t>(element_width));
      total_bytes_read += bytes_read;
    }

    // Emit MemoryDirty trace event for bulk memory write
    if (trace_manager_ != nullptr && trace_manager_->IsEnabled()) {
      const auto& place = (*arena_)[wb.dest];
      if (place.root.kind == mir::PlaceRoot::Kind::kDesign) {
        trace_manager_->EmitMemoryDirty(static_cast<uint32_t>(place.root.id));
      }
    }
  }

  // Commit return value (bytes_read)
  RuntimeValue bytes_val = MakeIntegralSigned(total_bytes_read, 32);
  if (call.ret) {
    auto store_result = StoreToPlace(state, call.ret->tmp, Clone(bytes_val));
    if (!store_result) return std::unexpected(store_result.error());
    if (call.ret->dest.has_value()) {
      return StoreToPlace(state, *call.ret->dest, std::move(bytes_val));
    }
  }

  return {};
}

auto Interpreter::ExecFscanfCall(ProcessState& state, const Call& call)
    -> Result<void> {
  // Shape: in_args[0]=descriptor, in_args[1]=format
  //        ret=count, writebacks[0..N]=outputs

  // Helper to return result count
  auto return_count = [&](int32_t count) -> Result<void> {
    RuntimeValue count_val = MakeIntegralSigned(count, 32);
    if (call.ret) {
      auto store_result = StoreToPlace(state, call.ret->tmp, Clone(count_val));
      if (!store_result) {
        return std::unexpected(std::move(store_result).error());
      }
      if (call.ret->dest.has_value()) {
        return StoreToPlace(state, *call.ret->dest, std::move(count_val));
      }
    }
    return {};
  };

  // Evaluate descriptor
  auto desc_result = EvalOperand(state, call.in_args[0]);
  if (!desc_result) return std::unexpected(desc_result.error());
  if (!IsIntegral(*desc_result)) return return_count(0);
  const auto& desc_int = AsIntegral(*desc_result);
  auto descriptor =
      static_cast<int32_t>(desc_int.value.empty() ? 0 : desc_int.value[0]);

  // Evaluate format string
  auto format_result = EvalOperand(state, call.in_args[1]);
  if (!format_result) return std::unexpected(format_result.error());
  if (!IsString(*format_result)) return return_count(0);
  const std::string& format_str = AsString(*format_result).value;

  // Track output index and any errors during callback
  size_t output_idx = 0;
  std::optional<Diagnostic> callback_error;

  // Use FileManager::Fscanf for the actual scanning
  int32_t items_matched = file_manager_.Fscanf(
      descriptor, format_str, [&](const runtime::ScanResult& result) {
        if (callback_error) return;                        // Already failed
        if (output_idx >= call.writebacks.size()) return;  // No more outputs

        const auto& wb = call.writebacks[output_idx];
        RuntimeValue rv;

        if (result.IsInt()) {
          rv = MakeIntegralSigned(result.AsInt(), 32);
        } else {
          rv = MakeString(result.AsString());
        }

        auto store_tmp = StoreToPlace(state, *wb.tmp, Clone(rv));
        if (!store_tmp) {
          callback_error = std::move(store_tmp).error();
          return;
        }
        auto store_dest = StoreToPlace(state, wb.dest, std::move(rv));
        if (!store_dest) {
          callback_error = std::move(store_dest).error();
          return;
        }

        ++output_idx;
      });

  if (callback_error) {
    return std::unexpected(*callback_error);
  }

  return return_count(items_matched);
}

namespace {

// Truncate queue to max_bound if specified (0 = unbounded).
// In SystemVerilog, [$:N] means indices 0 to N (inclusive), so max capacity is
// N+1.
void TruncateToBound(std::vector<RuntimeValue>& elements, uint32_t max_bound) {
  if (max_bound > 0) {
    while (elements.size() > max_bound + 1) {
      elements.pop_back();  // Discard from back
    }
  }
}

}  // namespace

auto Interpreter::ExecBuiltinCall(ProcessState& state, const BuiltinCall& call)
    -> Result<void> {
  switch (call.method) {
    case BuiltinMethod::kQueuePopBack: {
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      auto& elements = AsArray(write_result->get()).elements;
      RuntimeValue result;
      if (elements.empty()) {
        TypeId elem_type =
            (*types_)[TypeOfPlace(*types_, (*arena_)[call.receiver])]
                .AsQueue()
                .element_type;
        result = CreateDefaultValue(*types_, elem_type);
      } else {
        result = std::move(elements.back());
        elements.pop_back();
      }
      if (call.dest.has_value()) {
        return StoreToPlace(state, *call.dest, std::move(result));
      }
      return {};
    }

    case BuiltinMethod::kQueuePopFront: {
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      auto& elements = AsArray(write_result->get()).elements;
      RuntimeValue result;
      if (elements.empty()) {
        TypeId elem_type =
            (*types_)[TypeOfPlace(*types_, (*arena_)[call.receiver])]
                .AsQueue()
                .element_type;
        result = CreateDefaultValue(*types_, elem_type);
      } else {
        result = std::move(elements.front());
        elements.erase(elements.begin());
      }
      if (call.dest.has_value()) {
        return StoreToPlace(state, *call.dest, std::move(result));
      }
      return {};
    }

    case BuiltinMethod::kQueuePushBack: {
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      auto operand_result = EvalOperand(state, call.args[0]);
      if (!operand_result) {
        return std::unexpected(std::move(operand_result).error());
      }
      auto& elements = AsArray(write_result->get()).elements;
      elements.push_back(std::move(*operand_result));
      TypeId receiver_type = TypeOfPlace(*types_, (*arena_)[call.receiver]);
      const auto& type = (*types_)[receiver_type];
      if (type.Kind() == TypeKind::kQueue) {
        TruncateToBound(elements, type.AsQueue().max_bound);
      }
      return {};
    }

    case BuiltinMethod::kQueuePushFront: {
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      auto operand_result = EvalOperand(state, call.args[0]);
      if (!operand_result) {
        return std::unexpected(std::move(operand_result).error());
      }
      auto& elements = AsArray(write_result->get()).elements;
      elements.insert(elements.begin(), std::move(*operand_result));
      TypeId receiver_type = TypeOfPlace(*types_, (*arena_)[call.receiver]);
      const auto& type = (*types_)[receiver_type];
      if (type.Kind() == TypeKind::kQueue) {
        TruncateToBound(elements, type.AsQueue().max_bound);
      }
      return {};
    }

    case BuiltinMethod::kQueueInsert: {
      TypeId idx_type =
          TypeOfOperand(call.args[0], *arena_, *types_, state.frame);
      auto idx_val_result = EvalOperand(state, call.args[0]);
      if (!idx_val_result) {
        return std::unexpected(std::move(idx_val_result).error());
      }
      auto idx_opt = TryGetIndex(*idx_val_result, *types_, idx_type);
      if (!idx_opt) {
        return {};  // X/Z -> no-op
      }
      auto idx = *idx_opt;
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      auto& elements = AsArray(write_result->get()).elements;
      if (idx < 0 || std::cmp_greater(idx, elements.size())) {
        return {};  // Invalid index -> no-op
      }
      auto val_result = EvalOperand(state, call.args[1]);
      if (!val_result) {
        return std::unexpected(std::move(val_result).error());
      }
      elements.insert(elements.begin() + idx, std::move(*val_result));
      TypeId receiver_type = TypeOfPlace(*types_, (*arena_)[call.receiver]);
      const auto& type = (*types_)[receiver_type];
      if (type.Kind() == TypeKind::kQueue) {
        TruncateToBound(elements, type.AsQueue().max_bound);
      }
      return {};
    }

    case BuiltinMethod::kArrayDelete:
    case BuiltinMethod::kQueueDelete: {
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      AsArray(write_result->get()).elements.clear();
      return {};
    }

    case BuiltinMethod::kQueueDeleteAt: {
      TypeId idx_type =
          TypeOfOperand(call.args[0], *arena_, *types_, state.frame);
      auto idx_val_result = EvalOperand(state, call.args[0]);
      if (!idx_val_result) {
        return std::unexpected(std::move(idx_val_result).error());
      }
      auto idx_opt = TryGetIndex(*idx_val_result, *types_, idx_type);
      if (!idx_opt) {
        return {};  // X/Z -> no-op
      }
      auto idx = *idx_opt;
      auto write_result = WritePlace(state, call.receiver);
      if (!write_result) {
        return std::unexpected(std::move(write_result).error());
      }
      auto& elements = AsArray(write_result->get()).elements;
      if (idx >= 0 && std::cmp_less(idx, elements.size())) {
        elements.erase(elements.begin() + idx);
      }
      return {};
    }

    default:
      throw common::InternalError(
          "ExecBuiltinCall",
          std::format(
              "unexpected builtin method: {}", static_cast<int>(call.method)));
  }
}

auto Interpreter::ExecStatement(ProcessState& state, const Statement& stmt)
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
        } else if constexpr (std::is_same_v<T, Call>) {
          auto result = ExecCall(state, i);
          if (!result) {
            error = std::move(result).error();
          }
        } else if constexpr (std::is_same_v<T, BuiltinCall>) {
          auto result = ExecBuiltinCall(state, i);
          if (!result) {
            error = std::move(result).error();
          }
        } else if constexpr (std::is_same_v<T, DefineTemp>) {
          // DefineTemp: evaluate RHS and bind to temp_id
          auto value_result = std::visit(
              common::Overloaded{
                  [&](const Operand& operand) -> Result<RuntimeValue> {
                    return EvalOperand(state, operand);
                  },
                  [&](const Rvalue& rvalue) -> Result<RuntimeValue> {
                    return EvalRvalue(state, rvalue, i.type);
                  },
              },
              i.rhs);
          if (!value_result) {
            error = std::move(value_result).error();
          } else {
            state.frame.GetTemp(i.temp_id) = std::move(*value_result);
          }
        } else {
          // Non-blocking assignments require the LLVM backend runtime
          if (diag_ctx_ != nullptr) {
            error = diag_ctx_->MakeUnsupported(
                stmt.origin,
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
      stmt.data);

  if (error) {
    return std::unexpected(std::move(*error));
  }
  return {};
}

auto Interpreter::EvalEdgeArgs(
    ProcessState& state, const std::vector<Operand>& args)
    -> Result<std::vector<RuntimeValue>> {
  std::vector<RuntimeValue> values;
  values.reserve(args.size());
  for (const auto& arg : args) {
    auto val = EvalOperand(state, arg);
    if (!val) return std::unexpected(std::move(val).error());
    values.push_back(std::move(*val));
  }
  return values;
}

void Interpreter::BindBlockParams(
    ProcessState& state, const BasicBlock& block,
    std::vector<RuntimeValue> edge_args) {
  if (block.params.size() != edge_args.size()) {
    throw common::InternalError(
        "BindBlockParams", std::format(
                               "param/arg count mismatch: {} params, {} args",
                               block.params.size(), edge_args.size()));
  }
  for (size_t i = 0; i < block.params.size(); ++i) {
    state.frame.GetTemp(block.params[i].temp_id) = std::move(edge_args[i]);
  }
}

auto Interpreter::ExecTerminator(ProcessState& state, const Terminator& term)
    -> Result<TerminatorResult> {
  using ResultType = Result<TerminatorResult>;

  return std::visit(
      common::Overloaded{
          [&](const Jump& t) -> ResultType {
            auto args = EvalEdgeArgs(state, t.args);
            if (!args) return std::unexpected(std::move(args).error());
            return TerminatorResult{
                .target = t.target, .edge_args = std::move(*args)};
          },

          [&](const Branch& t) -> ResultType {
            auto cond_result = EvalOperand(state, t.condition);
            if (!cond_result) {
              return std::unexpected(std::move(cond_result).error());
            }
            auto& cond = *cond_result;
            // Support integral, real, and shortreal conditions
            bool take_then = false;
            if (IsReal(cond)) {
              take_then = RealIsTrue(AsReal(cond));
            } else if (IsShortReal(cond)) {
              take_then = ShortRealIsTrue(AsShortReal(cond));
            } else if (!IsIntegral(cond)) {
              throw common::InternalError(
                  "ExecTerminator",
                  "branch condition must be integral, real, or shortreal");
            } else {
              const auto& cond_int = AsIntegral(cond);
              if (!cond_int.IsKnown()) {
                // TODO(hankhsu): SV semantics - should return X, not throw
                return std::unexpected(
                    Diagnostic::HostError("branch condition is X/Z"));
              }
              take_then = !cond_int.IsZero();
            }
            // Evaluate appropriate edge args based on branch direction
            auto args = take_then ? EvalEdgeArgs(state, t.then_args)
                                  : EvalEdgeArgs(state, t.else_args);
            if (!args) return std::unexpected(std::move(args).error());
            return TerminatorResult{
                .target = take_then ? t.then_target : t.else_target,
                .edge_args = std::move(*args)};
          },

          [&](const Switch& t) -> ResultType {
            if (t.targets.empty()) {
              throw common::InternalError(
                  "ExecTerminator", "switch terminator has no targets");
            }
            auto selector_result = EvalOperand(state, t.selector);
            if (!selector_result) {
              return std::unexpected(std::move(selector_result).error());
            }
            auto& selector = *selector_result;
            if (!IsIntegral(selector)) {
              throw common::InternalError(
                  "ExecTerminator", "switch selector must be integral");
            }
            const auto& sel_int = AsIntegral(selector);
            BasicBlockId target;
            if (!sel_int.IsKnown()) {
              target = t.targets.back();
            } else {
              uint64_t val = sel_int.value.empty() ? 0 : sel_int.value[0];
              if (val >= t.targets.size() - 1) {
                target = t.targets.back();
              } else {
                target = t.targets[static_cast<size_t>(val)];
              }
            }
            // Switch doesn't have edge args in this iteration
            return TerminatorResult{.target = target, .edge_args = {}};
          },

          [&](const QualifiedDispatch& t) -> ResultType {
            // Validate invariant: targets = one per condition + else
            if (t.targets.size() != t.conditions.size() + 1) {
              throw common::InternalError(
                  "ExecTerminator",
                  "QualifiedDispatch: targets.size() != conditions.size() + 1");
            }

            std::ostream& out = output_ != nullptr ? *output_ : std::cout;

            // Evaluate all condition values and count how many are true
            size_t first_true_index = t.conditions.size();  // sentinel
            size_t true_count = 0;
            for (size_t i = 0; i < t.conditions.size(); ++i) {
              auto cond_result = EvalOperand(state, t.conditions[i]);
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
            BasicBlockId target = (first_true_index < t.conditions.size())
                                      ? t.targets[first_true_index]
                                      : t.targets.back();
            // QualifiedDispatch doesn't have edge args in this iteration
            return TerminatorResult{.target = target, .edge_args = {}};
          },

          [&](const Delay& t) -> ResultType {
            // Signal suspension - RunUntilSuspend will return SuspendDelay
            state.pending_suspend = SuspendDelay{
                .ticks = t.ticks,
                .resume_block = t.resume,
            };
            return TerminatorResult{.target = std::nullopt, .edge_args = {}};
          },

          [&](const Wait& /*t*/) -> ResultType {
            if (diag_ctx_ != nullptr) {
              return std::unexpected(diag_ctx_->MakeUnsupported(
                  term.origin, "wait statements require the LLVM backend",
                  UnsupportedCategory::kFeature));
            }
            return std::unexpected(
                Diagnostic{
                    .primary =
                        {.kind = DiagKind::kUnsupported,
                         .span = UnknownSpan{},
                         .message = "wait statements require the LLVM backend",
                         .category = UnsupportedCategory::kFeature},
                    .notes = {},
                });
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
            return TerminatorResult{.target = std::nullopt, .edge_args = {}};
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
            return TerminatorResult{.target = std::nullopt, .edge_args = {}};
          },

          [&](const Repeat& /*t*/) -> ResultType {
            if (diag_ctx_ != nullptr) {
              return std::unexpected(diag_ctx_->MakeUnsupported(
                  term.origin, "always blocks require the LLVM backend",
                  UnsupportedCategory::kFeature));
            }
            return std::unexpected(
                Diagnostic{
                    .primary =
                        {.kind = DiagKind::kUnsupported,
                         .span = UnknownSpan{},
                         .message = "always blocks require the LLVM backend",
                         .category = UnsupportedCategory::kFeature},
                    .notes = {},
                });
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
    while (state.instruction_index < block.statements.size()) {
      auto result =
          ExecStatement(state, block.statements[state.instruction_index]);
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
    auto& term_result = *next_block_result;

    if (term_result.target) {
      // Bind edge args to target block's params before entering
      const auto& next_block = process.blocks[term_result.target->value];
      BindBlockParams(state, next_block, std::move(term_result.edge_args));

      // Continue to next block
      state.current_block = *term_result.target;
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
