#include "lyra/interpreter/instruction_runner.hpp"

#include <cassert>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <format>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/system_call_runner.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

namespace {

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

// Extract int64 value from RuntimeValue, handling both narrow and wide sources.
// For wide sources, extracts the low 64 bits.
auto ExtractInt64FromSource(const RuntimeValue& src) -> int64_t {
  return src.IsWide() ? static_cast<int64_t>(src.AsWideBit().GetWord(0))
                      : src.AsNarrow().AsInt64();
}

// Create a WideBit from an int64 value, with optional sign extension.
// Masks the final word to the specified bit width for correct behavior.
auto CreateWideFromInt64(int64_t value, size_t bit_width, bool sign_extend)
    -> common::WideBit {
  auto num_words = common::wide_ops::WordsForBits(bit_width);
  common::WideBit wide(num_words);
  wide.SetWord(0, static_cast<uint64_t>(value));

  if (sign_extend && value < 0) {
    for (size_t i = 1; i < num_words; ++i) {
      wide.SetWord(i, ~uint64_t{0});
    }
  }

  // Mask final word to bit width (matches SDK behavior)
  common::wide_ops::MaskToWidth(wide.Words(), bit_width);
  return wide;
}

// Convert integral RuntimeValue to string per LRM 6.16.
// Each 8 bits forms one character, MSB first, null bytes are skipped.
auto IntegralToString(const RuntimeValue& val) -> std::string {
  std::string result;
  size_t width = std::get<common::IntegralData>(val.type.data).bit_width;

  if (val.IsWide()) {
    const auto& wide = val.AsWideBit();
    // Extract bytes from MSB to LSB
    for (size_t i = width; i >= 8; i -= 8) {
      size_t byte_start = i - 8;
      // Extract 8-bit value by reading bits
      uint8_t ch = 0;
      for (size_t b = 0; b < 8; ++b) {
        ch |= static_cast<uint8_t>(wide.GetBit(byte_start + b) << b);
      }
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  } else {
    // Narrow: extract bytes from MSB to LSB
    uint64_t bits = val.AsNarrow().AsUInt64();
    for (size_t i = width; i >= 8; i -= 8) {
      auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  }
  return result;
}

// Compute the actual array index after adjusting for lower bound and checking
// bounds. Returns the adjusted index or throws DiagnosticException if out of
// bounds.
auto ComputeArrayIndex(const RuntimeValue& array_value, int64_t sv_index)
    -> size_t {
  // Dynamic arrays always have lower_bound 0
  int32_t lower_bound = 0;
  if (array_value.type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data =
        std::get<common::UnpackedArrayData>(array_value.type.data);
    lower_bound = array_data.lower_bound;
  }

  auto actual_idx = static_cast<size_t>(sv_index - lower_bound);

  if (actual_idx >= array_value.AsArray().size()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, fmt::format(
                    "array index {} out of bounds (size {})", sv_index,
                    array_value.AsArray().size())));
  }

  return actual_idx;
}

// Handle kCall instruction: use resolved function pointer, prepare frame.
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

// Handle kReturn instruction: get return value, pop frame, store result.
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

// Execute a single instruction in the given context
auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> InstructionResult {
  // Use function-local temp table when inside a function, otherwise use
  // process temp table. This ensures recursive calls don't overwrite temps.
  auto& temp_table = frame.call_stack.empty()
                         ? frame.temp_table
                         : frame.call_stack.back().temp_table;
  auto& module_variable_table = simulation_context.variable_table;
  auto& process_variable_table = frame.variable_table;

  // Helper to resolve symbol through instance context port bindings.
  // Returns (target_symbol, target_instance) where target_instance is nullptr
  // if not bound (use current instance or global).
  auto resolve_binding = [&instance_context](const auto* symbol)
      -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>> {
    if (instance_context != nullptr) {
      return instance_context->ResolveBinding(symbol);
    }
    return {symbol, nullptr};
  };

  auto get_temp = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsTemp());
    return temp_table.Read(std::get<lir::TempRef>(operand.value));
  };

  auto read_variable = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    // Check function-local variables first (parameters and locals)
    if (!frame.call_stack.empty()) {
      auto& call_frame = frame.call_stack.back();
      auto it = call_frame.local_variables.find(symbol);
      if (it != call_frame.local_variables.end()) {
        return it->second;
      }
    }

    // Check process-local next
    if (process_variable_table.Exists(symbol)) {
      return process_variable_table.Read(symbol);
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound, read from target instance's storage
    if (target_instance != nullptr) {
      return target_instance->Read(target_symbol);
    }

    // Otherwise, read from per-instance storage (local vars, input ports)
    if (instance_context != nullptr && instance_context->Exists(symbol)) {
      return instance_context->Read(symbol);
    }

    // Fallback to global table (for backwards compat with non-hierarchical
    // code)
    return module_variable_table.Read(symbol);
  };

  auto store_variable = [&](const lir::Operand& operand,
                            const RuntimeValue& value, bool is_non_blocking) {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    // Deep copy arrays for value semantics (nested arrays must be independent)
    const RuntimeValue actual_value = value.DeepCopy();

    // Check function-local variables first (parameters and locals)
    if (!frame.call_stack.empty()) {
      auto& call_frame = frame.call_stack.back();
      auto it = call_frame.local_variables.find(symbol);
      if (it != call_frame.local_variables.end()) {
        it->second = actual_value;
        return;
      }
    }

    // Check process-local next
    if (process_variable_table.Exists(symbol)) {
      process_variable_table.Write(symbol, actual_value);
      return;
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound (output port), write to target instance's storage
    if (target_instance != nullptr) {
      if (!is_non_blocking) {
        target_instance->Write(target_symbol, actual_value);
        effect.RecordVariableModification(target_symbol, target_instance);
      } else {
        effect.RecordNbaAction(
            {.variable = target_symbol,
             .value = actual_value,
             .instance = target_instance,
             .array_index = std::nullopt});
      }
      return;
    }

    // Otherwise, write to per-instance storage (local vars, input ports)
    if (instance_context != nullptr) {
      if (!is_non_blocking) {
        instance_context->Write(symbol, actual_value);
        effect.RecordVariableModification(symbol, instance_context);
      } else {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = actual_value,
             .instance = instance_context,
             .array_index = std::nullopt});
      }
      return;
    }

    // Fallback to global table (for backwards compat with non-hierarchical
    // code)
    if (!is_non_blocking) {
      module_variable_table.Write(symbol, actual_value);
      effect.RecordVariableModification(symbol);  // Global storage
    } else {
      effect.RecordNbaAction(
          {.variable = symbol,
           .value = actual_value,
           .instance = nullptr,
           .array_index = std::nullopt});
    }
  };

  // Store to hierarchical target: traverse instance path and store to target
  auto store_hierarchical = [&](const std::vector<common::SymbolRef>& instances,
                                common::SymbolRef target,
                                const RuntimeValue& value,
                                bool is_non_blocking) {
    // Deep copy arrays for value semantics (nested arrays must be independent)
    const RuntimeValue actual_value = value.DeepCopy();

    // Traverse instance path
    auto target_instance = instance_context;
    for (const auto& inst_sym : instances) {
      target_instance = target_instance->LookupChild(inst_sym);
      if (!target_instance) {
        throw DiagnosticException(
            Diagnostic::Error(
                {}, fmt::format("Unknown child instance: {}", inst_sym->name)));
      }
    }

    // Write to target instance
    if (!is_non_blocking) {
      target_instance->Write(target, actual_value);
      effect.RecordVariableModification(target, target_instance);
    } else {
      effect.RecordNbaAction(
          {.variable = target,
           .value = actual_value,
           .instance = target_instance,
           .array_index = std::nullopt});
    }
  };

  // Load from hierarchical target: traverse instance path and load from target
  auto load_hierarchical = [&](const std::vector<common::SymbolRef>& instances,
                               common::SymbolRef target) -> RuntimeValue {
    // Traverse instance path
    auto target_instance = instance_context;
    for (const auto& inst_sym : instances) {
      target_instance = target_instance->LookupChild(inst_sym);
      if (!target_instance) {
        throw DiagnosticException(
            Diagnostic::Error(
                {}, fmt::format("Unknown child instance: {}", inst_sym->name)));
      }
    }

    // Read from target instance
    return target_instance->Read(target);
  };

  auto eval_unary_op = [&](const lir::Operand& operand,
                           const std::function<RuntimeValue(RuntimeValue)>& op)
      -> InstructionResult {
    const auto result = op(get_temp(operand));
    assert(instr.result.has_value());
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  };

  auto eval_binary_op =
      [&](const lir::Operand& lhs, const lir::Operand& rhs,
          const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
      -> InstructionResult {
    const auto result = op(get_temp(lhs), get_temp(rhs));
    assert(instr.result.has_value());
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  };

  auto get_operand_value = [&](const lir::Operand& operand) -> RuntimeValue {
    if (operand.IsTemp()) {
      return get_temp(operand);
    }
    if (operand.IsVariable()) {
      return read_variable(operand);
    }
    if (operand.IsLiteral()) {
      const auto& literal = std::get<lir::LiteralRef>(operand.value);
      return RuntimeValue::FromLiteral(literal);
    }
    throw common::InternalError(
        "interpreter", "unexpected operand kind for system call");
  };

  switch (instr.kind) {
    // Memory operations
    case lir::InstructionKind::kLiteral: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLiteral());
      assert(instr.result.has_value());

      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
      RuntimeValue value = RuntimeValue::FromLiteral(literal);
      temp_table.Write(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadVariable: {
      if (instr.target_symbol != nullptr) {
        // Hierarchical load: traverse instance_path, load from target_symbol
        const auto value =
            load_hierarchical(instr.instance_path, instr.target_symbol);
        temp_table.Write(instr.result.value(), value);
      } else {
        // Regular load: variable in operands[0]
        const auto& src_variable = read_variable(instr.operands[0]);
        temp_table.Write(instr.result.value(), src_variable);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariable: {
      if (instr.target_symbol != nullptr) {
        // Hierarchical store: traverse instance_path, store to target_symbol
        const auto value = get_temp(instr.operands[0]);
        store_hierarchical(
            instr.instance_path, instr.target_symbol, value, false);
      } else {
        // Regular store: variable in operands[0], value in operands[1]
        const auto variable = instr.operands[0];
        const auto value = get_temp(instr.operands[1]);
        assert(variable.IsVariable());
        store_variable(variable, value, false);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariableNonBlocking: {
      if (instr.target_symbol != nullptr) {
        // Hierarchical store: traverse instance_path, store to target_symbol
        const auto value = get_temp(instr.operands[0]);
        store_hierarchical(
            instr.instance_path, instr.target_symbol, value, true);
      } else {
        // Regular store: variable in operands[0], value in operands[1]
        const auto variable = instr.operands[0];
        const auto value = get_temp(instr.operands[1]);
        assert(variable.IsVariable());
        store_variable(variable, value, true);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadElement: {
      // Polymorphic load: result = base[index]
      // base can be variable or temp, value can be array or struct
      assert(instr.operands.size() == 2);
      assert(instr.result.has_value());

      RuntimeValue base_value;
      if (instr.operands[0].IsVariable()) {
        base_value = read_variable(instr.operands[0]);
      } else {
        base_value = get_temp(instr.operands[0]);
      }

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      if (base_value.IsArray()) {
        auto actual_idx =
            ComputeArrayIndex(base_value, static_cast<int64_t>(index));
        temp_table.Write(
            instr.result.value(), base_value.GetElement(actual_idx));
      } else {
        assert(base_value.IsUnpackedStruct() || base_value.IsUnpackedUnion());
        temp_table.Write(instr.result.value(), base_value.GetField(index));
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElement: {
      // Polymorphic store: base[index] = value
      // base can be variable or temp, value can be array or struct
      assert(instr.operands.size() == 3);

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());
      auto element_value = get_temp(instr.operands[2]);

      if (instr.operands[0].IsVariable()) {
        // Store to variable - needs sensitivity tracking
        const auto* symbol = std::get<lir::SymbolRef>(instr.operands[0].value);

        // Check if this is a local variable (no triggers needed)
        bool is_local = false;
        if (!frame.call_stack.empty()) {
          auto& call_frame = frame.call_stack.back();
          is_local = call_frame.local_variables.contains(symbol);
        }
        if (!is_local) {
          is_local = process_variable_table.Exists(symbol);
        }

        // Read aggregate value (shared_ptr semantics - modifications affect
        // original)
        auto aggregate_value = read_variable(instr.operands[0]);

        // Snapshot old value BEFORE modification for non-local variables
        if (!is_local) {
          auto [target_symbol, target_instance] = resolve_binding(symbol);
          if (target_instance != nullptr) {
            target_instance->UpdatePrevious(target_symbol, aggregate_value);
          } else if (instance_context != nullptr) {
            instance_context->UpdatePrevious(symbol, aggregate_value);
          }
        }

        // Perform the element store
        if (aggregate_value.IsArray()) {
          auto actual_idx =
              ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
          aggregate_value.SetElement(actual_idx, element_value);
        } else {
          assert(
              aggregate_value.IsUnpackedStruct() ||
              aggregate_value.IsUnpackedUnion());
          aggregate_value.SetField(index, element_value);
        }

        // Record modification for trigger system
        if (!is_local) {
          auto [target_symbol, target_instance] = resolve_binding(symbol);
          if (target_instance != nullptr) {
            effect.RecordVariableModification(target_symbol, target_instance);
          } else if (instance_context != nullptr) {
            effect.RecordVariableModification(symbol, instance_context);
          } else {
            effect.RecordVariableModification(symbol);
          }
        }
      } else {
        // Store to temp - no sensitivity tracking needed
        auto aggregate_value = get_temp(instr.operands[0]);

        if (aggregate_value.IsArray()) {
          auto actual_idx =
              ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
          aggregate_value.SetElement(actual_idx, element_value);
        } else {
          assert(
              aggregate_value.IsUnpackedStruct() ||
              aggregate_value.IsUnpackedUnion());
          aggregate_value.SetField(index, element_value);
        }
      }

      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElementNonBlocking: {
      // NBA to element: base[index] <= value
      // Always targets a variable (NBA must write to storage)
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      const auto* symbol = std::get<lir::SymbolRef>(instr.operands[0].value);
      auto aggregate_value = read_variable(instr.operands[0]);

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      // For arrays, compute actual index with bounds handling
      size_t actual_idx = index;
      if (aggregate_value.IsArray()) {
        actual_idx =
            ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
      }

      auto element_value = get_temp(instr.operands[2]);

      // Resolve binding for port outputs
      auto [target_symbol, target_instance] = resolve_binding(symbol);

      // Queue NBA action with element index
      if (target_instance != nullptr) {
        effect.RecordNbaAction(
            {.variable = target_symbol,
             .value = element_value,
             .instance = target_instance,
             .array_index = actual_idx});
      } else if (instance_context != nullptr) {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = element_value,
             .instance = instance_context,
             .array_index = actual_idx});
      } else {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = element_value,
             .instance = nullptr,
             .array_index = actual_idx});
      }

      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kCreateAggregate: {
      // Create default-initialized aggregate (struct or array)
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto aggregate_value =
          RuntimeValue::DefaultValueForType(*instr.result_type);
      temp_table.Write(instr.result.value(), std::move(aggregate_value));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kNewDynamicArray: {
      // Allocate/resize dynamic array: new[size] or new[size](init)
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      auto size_val = get_operand_value(instr.operands[0]);
      auto size = static_cast<size_t>(size_val.AsNarrow().AsInt64());

      const auto& dyn_data =
          std::get<common::DynamicArrayData>(instr.result_type->data);
      const auto& elem_type = *dyn_data.element_type;

      std::vector<RuntimeValue> elements;
      elements.reserve(size);

      if (instr.operands.size() > 1) {
        // Resize with init: deep-copy elements for value semantics
        const auto& init = get_operand_value(instr.operands[1]);
        const auto& init_arr = init.AsArray();
        for (size_t i = 0; i < size; ++i) {
          if (i < init_arr.size()) {
            elements.push_back(init_arr[i].DeepCopy());
          } else {
            elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
          }
        }
      } else {
        // Default init
        for (size_t i = 0; i < size; ++i) {
          elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
        }
      }

      temp_table.Write(
          instr.result.value(),
          RuntimeValue::Array(*instr.result_type, std::move(elements)));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadPackedBits: {
      // Load bits from packed vector: result = value[bit_offset +: width]
      // operands[0] = value (packed vector)
      // operands[1] = bit_offset (pre-computed)
      // result_type contains the width
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsTemp());
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = get_temp(instr.operands[0]);
      auto offset_value = get_temp(instr.operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      const auto& result_type = instr.result_type.value();
      // LoadPackedBits can extract integral types or packed aggregates
      // (struct/union)
      assert(
          result_type.kind == common::Type::Kind::kIntegral ||
          result_type.kind == common::Type::Kind::kPackedStruct);
      size_t width = result_type.GetBitWidth();
      bool is_signed = false;
      if (result_type.kind == common::Type::Kind::kIntegral) {
        is_signed = std::get<common::IntegralData>(result_type.data).is_signed;
      } else {
        is_signed =
            std::get<common::PackedStructData>(result_type.data).is_signed;
      }

      RuntimeValue result;
      if (width <= 64) {
        // Narrow - extract as uint64_t
        uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
        uint64_t extracted = 0;
        if (value.IsWide()) {
          auto shifted = value.AsWideBit().ShiftRightLogical(bit_offset);
          extracted = shifted.GetWord(0) & mask;
        } else {
          extracted = (value.AsNarrow().AsUInt64() >> bit_offset) & mask;
        }
        result = is_signed ? RuntimeValue::IntegralSigned(
                                 static_cast<int64_t>(extracted), width)
                           : RuntimeValue::IntegralUnsigned(extracted, width);
      } else {
        // Wide - extract as WideBit
        auto wide_value = value.IsWide() ? value.AsWideBit()
                                         : common::WideBit::FromUInt64(
                                               value.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(bit_offset, width);
        result =
            RuntimeValue::IntegralWide(std::move(extracted), width, is_signed);
      }
      temp_table.Write(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStorePackedBits: {
      // Store bits to packed vector: variable[bit_offset +: width] = value
      // operands[0] = variable (packed vector)
      // operands[1] = bit_offset (pre-computed)
      // operands[2] = value to store
      // result_type contains slice width
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());
      assert(instr.operands[1].IsTemp());
      assert(instr.operands[2].IsTemp());
      assert(instr.result_type.has_value());

      auto current = read_variable(instr.operands[0]);
      assert(current.IsTwoState());

      auto offset_value = get_temp(instr.operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      auto new_value = get_temp(instr.operands[2]);

      // Get slice width from result_type
      const auto& slice_type = instr.result_type.value();
      size_t slice_width = slice_type.GetBitWidth();

      // Get storage width and signedness
      const auto& current_data =
          std::get<common::IntegralData>(current.type.data);
      size_t storage_width = current_data.bit_width;
      bool storage_is_wide = current.IsWide();
      bool slice_is_wide = slice_width > 64;

      RuntimeValue result;
      if (!storage_is_wide && !slice_is_wide) {
        // Narrow storage, narrow slice - mask-and-merge
        uint64_t slice_mask =
            common::MakeBitMask(static_cast<uint32_t>(slice_width));
        uint64_t clear_mask = ~(slice_mask << bit_offset);
        uint64_t merged =
            (current.AsNarrow().AsUInt64() & clear_mask) |
            ((new_value.AsNarrow().AsUInt64() & slice_mask) << bit_offset);
        result = current_data.is_signed
                     ? RuntimeValue::IntegralSigned(
                           static_cast<int64_t>(merged), storage_width)
                     : RuntimeValue::IntegralUnsigned(merged, storage_width);
      } else {
        // Wide storage or wide slice - use WideBit InsertSlice
        size_t storage_words = common::wide_ops::WordsForBits(storage_width);
        auto current_wide =
            storage_is_wide ? current.AsWideBit()
                            : common::WideBit::FromUInt64(
                                  current.AsNarrow().AsUInt64(), storage_words);
        auto value_wide =
            slice_is_wide ? new_value.AsWideBit()
                          : common::WideBit::FromUInt64(
                                new_value.AsNarrow().AsUInt64(), storage_words);
        auto merged =
            current_wide.InsertSlice(value_wide, bit_offset, slice_width);
        result = RuntimeValue::IntegralWide(
            std::move(merged), storage_width, current_data.is_signed);
      }
      store_variable(instr.operands[0], result, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kMove: {
      assert(instr.operands.size() == 1);
      assert(instr.result.has_value());

      const auto value = get_temp(instr.operands[0]);
      temp_table.Write(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    // Unary operations
    case lir::InstructionKind::kUnaryPlus: {
      return eval_unary_op(instr.operands[0], UnaryPlus);
    }

    case lir::InstructionKind::kUnaryMinus: {
      return eval_unary_op(instr.operands[0], UnaryMinus);
    }

    case lir::InstructionKind::kUnaryLogicalNot: {
      return eval_unary_op(instr.operands[0], UnaryLogicalNot);
    }

    case lir::InstructionKind::kUnaryBitwiseNot: {
      return eval_unary_op(instr.operands[0], UnaryBitwiseNot);
    }

    // Reduction operations
    case lir::InstructionKind::kReductionAnd: {
      return eval_unary_op(instr.operands[0], ReductionAnd);
    }

    case lir::InstructionKind::kReductionNand: {
      return eval_unary_op(instr.operands[0], ReductionNand);
    }

    case lir::InstructionKind::kReductionOr: {
      return eval_unary_op(instr.operands[0], ReductionOr);
    }

    case lir::InstructionKind::kReductionNor: {
      return eval_unary_op(instr.operands[0], ReductionNor);
    }

    case lir::InstructionKind::kReductionXor: {
      return eval_unary_op(instr.operands[0], ReductionXor);
    }

    case lir::InstructionKind::kReductionXnor: {
      return eval_unary_op(instr.operands[0], ReductionXnor);
    }

    // Binary operations
    case lir::InstructionKind::kBinaryAdd: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryAdd);
    }

    case lir::InstructionKind::kBinarySubtract: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinarySubtract);
    }

    case lir::InstructionKind::kBinaryMultiply: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryMultiply);
    }

    case lir::InstructionKind::kBinaryDivide: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryDivide);
    }

    case lir::InstructionKind::kBinaryModulo: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryModulo);
    }

    case lir::InstructionKind::kBinaryEqual: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryEqual);
    }

    case lir::InstructionKind::kBinaryNotEqual: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryNotEqual);
    }

    case lir::InstructionKind::kBinaryLessThan: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLessThan);
    }

    case lir::InstructionKind::kBinaryLessThanEqual: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLessThanEqual);
    }

    case lir::InstructionKind::kBinaryGreaterThan: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryGreaterThan);
    }

    case lir::InstructionKind::kBinaryGreaterThanEqual: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryGreaterThanEqual);
    }

    case lir::InstructionKind::kBinaryPower: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryPower);
    }

    case lir::InstructionKind::kBinaryBitwiseAnd: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseAnd);
    }

    case lir::InstructionKind::kBinaryBitwiseOr: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseOr);
    }

    case lir::InstructionKind::kBinaryBitwiseXor: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseXor);
    }

    case lir::InstructionKind::kBinaryBitwiseXnor: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseXnor);
    }

    case lir::InstructionKind::kBinaryLogicalAnd: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalAnd);
    }

    case lir::InstructionKind::kBinaryLogicalOr: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalOr);
    }

    case lir::InstructionKind::kBinaryLogicalShiftLeft: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalShiftLeft);
    }

    case lir::InstructionKind::kBinaryLogicalShiftRight: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalShiftRight);
    }

    case lir::InstructionKind::kBinaryArithmeticShiftLeft: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryArithmeticShiftLeft);
    }

    case lir::InstructionKind::kBinaryArithmeticShiftRight: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryArithmeticShiftRight);
    }

    // Type operations
    case lir::InstructionKind::kConversion: {
      // Read the source value from temp table
      const auto& src = get_temp(instr.operands[0]);
      const auto& target_type = instr.result_type.value();

      // Handle string to string and real to real as no-op conversions.
      if ((src.type == common::Type::String() &&
           target_type == common::Type::String()) ||
          (src.type == common::Type::Real() &&
           target_type == common::Type::Real())) {
        temp_table.Write(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      // Helper to check if a type is integral-like (bitvector)
      auto is_integral_like = [](common::Type::Kind kind) {
        return kind == common::Type::Kind::kIntegral ||
               kind == common::Type::Kind::kPackedStruct;
      };

      if (is_integral_like(src.type.kind) &&
          is_integral_like(target_type.kind)) {
        // Get bit width and signedness for the target type
        size_t target_width = target_type.GetBitWidth();
        bool target_signed =
            target_type.IsPackedStruct()
                ? std::get<common::PackedStructData>(target_type.data).is_signed
                : std::get<common::IntegralData>(target_type.data).is_signed;

        // Get source signedness for sign extension
        bool src_signed =
            src.type.IsPackedStruct()
                ? std::get<common::PackedStructData>(src.type.data).is_signed
                : std::get<common::IntegralData>(src.type.data).is_signed;

        // Wide target type - create WideBit and sign-extend if needed
        if (target_width > 64) {
          common::WideBit wide = src.IsWide() ? src.AsWideBit() : [&]() {
            return CreateWideFromInt64(
                src.AsNarrow().AsInt64(), target_width, src_signed);
          }();

          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), target_width, target_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type - extract value (may be from wide source)
        int64_t raw_value = ExtractInt64FromSource(src);

        // Apply sign/bitwidth conversion
        RuntimeValue result =
            target_signed
                ? RuntimeValue::IntegralSigned(raw_value, target_width)
                : RuntimeValue::IntegralUnsigned(
                      static_cast<uint64_t>(raw_value), target_width);

        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kReal) {
        auto two_state_data = std::get<common::IntegralData>(src.type.data);
        double real_value = 0.0;
        if (src.IsWide()) {
          // Wide source - convert full value to double
          // Note: For signed wide, we'd need to handle two's complement
          // but typically unsigned for large values
          real_value = src.AsWideBit().ToDouble();
        } else {
          int64_t raw_value = src.AsNarrow().AsInt64();
          real_value =
              two_state_data.is_signed
                  ? static_cast<double>(raw_value)
                  : static_cast<double>(static_cast<uint64_t>(raw_value));
        }
        temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);
        auto raw_value = static_cast<int64_t>(src.AsDouble());

        // Wide target type
        if (two_state_data.bit_width > 64) {
          // Real-to-integral: sign-extend if negative
          common::WideBit wide = CreateWideFromInt64(
              raw_value, two_state_data.bit_width,
              /*sign_extend=*/true);
          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), two_state_data.bit_width,
              two_state_data.is_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type
        RuntimeValue result = two_state_data.is_signed
                                  ? RuntimeValue::IntegralSigned(
                                        raw_value, two_state_data.bit_width)
                                  : RuntimeValue::IntegralUnsigned(
                                        static_cast<uint64_t>(raw_value),
                                        two_state_data.bit_width);

        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      // Shortreal conversions
      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kShortReal) {
        auto two_state_data = std::get<common::IntegralData>(src.type.data);
        float float_value = 0.0F;
        if (src.IsWide()) {
          // Wide source - convert full value to float
          float_value = static_cast<float>(src.AsWideBit().ToDouble());
        } else {
          int64_t raw_value = src.AsNarrow().AsInt64();
          float_value =
              two_state_data.is_signed
                  ? static_cast<float>(raw_value)
                  : static_cast<float>(static_cast<uint64_t>(raw_value));
        }
        temp_table.Write(
            instr.result.value(), RuntimeValue::ShortReal(float_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);
        auto raw_value = static_cast<int64_t>(src.AsFloat());

        // Wide target type
        if (two_state_data.bit_width > 64) {
          // Shortreal-to-integral: sign-extend if negative
          common::WideBit wide = CreateWideFromInt64(
              raw_value, two_state_data.bit_width,
              /*sign_extend=*/true);
          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), two_state_data.bit_width,
              two_state_data.is_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type
        RuntimeValue result = two_state_data.is_signed
                                  ? RuntimeValue::IntegralSigned(
                                        raw_value, two_state_data.bit_width)
                                  : RuntimeValue::IntegralUnsigned(
                                        static_cast<uint64_t>(raw_value),
                                        two_state_data.bit_width);

        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.kind == common::Type::Kind::kShortReal) {
        // Precision loss: double -> float
        temp_table.Write(
            instr.result.value(),
            RuntimeValue::ShortReal(static_cast<float>(src.AsDouble())));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.kind == common::Type::Kind::kReal) {
        // Precision gain: float -> double
        temp_table.Write(
            instr.result.value(),
            RuntimeValue::Real(static_cast<double>(src.AsFloat())));
        return InstructionResult::Continue();
      }

      // Integral to string conversion (LRM 6.16)
      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kString) {
        temp_table.Write(
            instr.result.value(), RuntimeValue::String(IntegralToString(src)));
        return InstructionResult::Continue();
      }

      // Dynamic array to dynamic array: pass through unchanged
      // This handles cases where slang inserts a conversion expression for
      // type-compatible arrays (e.g., in new[size](init) expressions)
      if (src.type.IsDynamicArray() && target_type.IsDynamicArray()) {
        temp_table.Write(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      throw DiagnosticException(
          Diagnostic::Error(
              {},
              fmt::format(
                  "conversion only supports two-state/real/shortreal/string "
                  "types, got: {} -> {}",
                  src.type, target_type)));
    }

    // Concatenation: result = {op0, op1, ...}
    // Operands are ordered MSB to LSB (first operand is most significant)
    case lir::InstructionKind::kConcatenation: {
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      const auto& result_type = instr.result_type.value();

      // String concatenation: collect strings and join
      if (result_type.kind == common::Type::Kind::kString) {
        std::string result;
        for (const auto& operand : instr.operands) {
          const auto& val = get_temp(operand);
          if (val.IsString()) {
            result += val.AsString();
          } else {
            // Integral operand - convert per LRM (8 bits = 1 char)
            result += IntegralToString(val);
          }
        }
        temp_table.Write(instr.result.value(), RuntimeValue::String(result));
        return InstructionResult::Continue();
      }

      // Integral concatenation: existing path
      size_t result_width = result_type.GetBitWidth();

      // Collect operand values
      std::vector<RuntimeValue> operand_values;
      operand_values.reserve(instr.operands.size());
      for (const auto& operand : instr.operands) {
        operand_values.push_back(get_temp(operand));
      }

      // RuntimeValue::Concatenate handles narrow/wide dispatch internally
      temp_table.Write(
          instr.result.value(),
          RuntimeValue::Concatenate(operand_values, result_width));
      return InstructionResult::Continue();
    }

    // Control flow
    case lir::InstructionKind::kComplete: {
      return InstructionResult::Complete();
    }

    case lir::InstructionKind::kWaitEvent: {
      // Pass triggers through - resolution happens in simulation_runner
      return InstructionResult::WaitEvent(instr.wait_triggers);
    }

    case lir::InstructionKind::kDelay: {
      assert(instr.operands[0].IsLiteral());
      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
      // Delay amounts are never wide
      const auto delay_amount =
          RuntimeValue::FromLiteral(literal).AsNarrow().AsUInt64();
      return InstructionResult::Delay(delay_amount);
    }

    case lir::InstructionKind::kSystemCall: {
      return RunSystemCall(
          instr, simulation_context, frame, effect, temp_table,
          instance_context);
    }

    case lir::InstructionKind::kMethodCall: {
      // Generic method call - handles enum methods (next, prev, name) and
      // array methods (size, delete).
      //
      // Note: Unlike codegen which uses registry-driven dispatch, we use string
      // comparison here. This is acceptable because: (1) only 5 methods total,
      // (2) string comparison is fast for short strings, (3) simpler than
      // pulling in registry dependency for runtime dispatch.
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      // Array methods have empty enum_members
      if (instr.enum_members.empty()) {
        // RuntimeValue uses shared_ptr for array storage, so modifications
        // through this copy will affect the original variable
        auto receiver = get_temp(instr.operands[0]);

        if (instr.method_name == "size") {
          auto size = static_cast<int32_t>(receiver.AsArray().size());
          temp_table.Write(
              instr.result.value(), RuntimeValue::IntegralSigned(size, 32));
        } else if (instr.method_name == "delete") {
          // Shared ptr semantics: clearing temp clears original variable
          receiver.AsArray().clear();
          // delete() returns void, write a dummy value
          temp_table.Write(
              instr.result.value(),
              RuntimeValue::DefaultValueForType(*instr.result_type));
        } else {
          assert(false && "unsupported array method call");
        }
        return InstructionResult::Continue();
      }

      // Enum methods
      const auto& receiver = get_temp(instr.operands[0]);
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
          // next(N): move forward N positions with wrap-around
          auto step = static_cast<size_t>(instr.method_step);
          size_t target_pos = (current_pos + step) % members.size();
          result = RuntimeValue::IntegralSigned(
              members[target_pos].value, instr.result_type->GetBitWidth());
        } else {
          // Invalid value: return 0 (implementation-defined per IEEE 1800-2023)
          result =
              RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
        }
      } else if (instr.method_name == "prev") {
        if (found) {
          // prev(N): move backward N positions with wrap-around
          // Add members.size() before subtracting to avoid underflow
          size_t step = static_cast<size_t>(instr.method_step) % members.size();
          size_t target_pos =
              (current_pos + members.size() - step) % members.size();
          result = RuntimeValue::IntegralSigned(
              members[target_pos].value, instr.result_type->GetBitWidth());
        } else {
          // Invalid value: return 0 (implementation-defined per IEEE 1800-2023)
          result =
              RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
        }
      } else if (instr.method_name == "name") {
        if (found) {
          result = RuntimeValue::String(members[current_pos].name);
        } else {
          // Invalid value: return empty string (implementation-defined)
          result = RuntimeValue::String("");
        }
      } else {
        assert(false && "unsupported method call");
      }

      temp_table.Write(instr.result.value(), result);
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

      const auto& condition = get_temp(instr.operands[0]);
      const auto& true_target =
          std::get<lir::LabelRef>(instr.operands[1].value);
      const auto& false_target =
          std::get<lir::LabelRef>(instr.operands[2].value);

      return InstructionResult::Jump(
          IsTruthy(condition) ? true_target : false_target);
    }

    case lir::InstructionKind::kCall:
      return HandleCall(instr, temp_table);

    case lir::InstructionKind::kReturn:
      return HandleReturn(instr, frame, temp_table);

    case lir::InstructionKind::kLoadCapture: {
      // Load value from the closure's captures.
      // Used by closures (e.g., $monitor check processes) to load captured
      // values.
      if (!simulation_context.active_monitor.has_value()) {
        throw common::InternalError(
            "kLoadCapture", "no active monitor (closure context)");
      }
      auto& captures = simulation_context.active_monitor->closure.captures;

      auto it = captures.find(instr.capture_name);
      if (it == captures.end()) {
        throw common::InternalError(
            "kLoadCapture",
            std::format("capture '{}' not found", instr.capture_name));
      }

      temp_table.Write(instr.result.value(), it->second);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreCapture: {
      // Store value to the closure's captures.
      // Used by closures (e.g., $monitor check processes) to update captured
      // values.
      if (!simulation_context.active_monitor.has_value()) {
        throw common::InternalError(
            "kStoreCapture", "no active monitor (closure context)");
      }

      auto& captures = simulation_context.active_monitor->closure.captures;
      auto value = get_temp(instr.operands[0]);

      captures[instr.capture_name] = value;
      return InstructionResult::Continue();
    }
  }
}

}  // namespace lyra::interpreter
