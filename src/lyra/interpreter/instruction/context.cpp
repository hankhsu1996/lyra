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

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
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
    std::shared_ptr<HierarchyContext> hierarchy_context)
    : simulation_context_(&simulation_context),
      frame_(&frame),
      effect_(&effect),
      temp_table_(&temp_table),
      hierarchy_context_(std::move(hierarchy_context)) {
}

auto InstructionContext::ResolveBinding(common::SymbolId symbol) const
    -> std::pair<common::SymbolId, std::shared_ptr<HierarchyContext>> {
  if (hierarchy_context_ != nullptr) {
    return hierarchy_context_->ResolveBinding(symbol);
  }
  return {symbol, nullptr};
}

auto InstructionContext::GetTemp(lir::TempRef temp) const -> RuntimeValue {
  return temp_table_->Read(temp);
}

auto InstructionContext::ReadVariable(common::SymbolId symbol) const
    -> RuntimeValue {
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

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);

  // Read from flat storage (either resolved target or original symbol)
  auto actual_symbol = (target_instance != nullptr) ? target_symbol : symbol;
  return simulation_context_->variable_store.Read(actual_symbol);
}

void InstructionContext::StoreVariable(
    common::SymbolId symbol, const RuntimeValue& value, bool is_non_blocking) {
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

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);
  auto actual_symbol = (target_instance != nullptr) ? target_symbol : symbol;

  // Write to flat storage
  if (!is_non_blocking) {
    simulation_context_->variable_store.Write(actual_symbol, actual_value);
    effect_->RecordVariableModification(actual_symbol);
  } else {
    effect_->RecordNbaAction(
        {.symbol = actual_symbol,
         .value = actual_value,
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

  size_t container_size = array_value.IsQueue() ? array_value.AsQueue().size()
                                                : array_value.AsArray().size();
  if (actual_idx >= container_size) {
    throw std::runtime_error(
        fmt::format(
            "array index {} out of bounds (size {})", sv_index,
            container_size));
  }

  return actual_idx;
}

}  // namespace

auto InstructionContext::ReadPointer(const PointerValue& ptr) const
    -> RuntimeValue {
  if (ptr.IsAlloc()) {
    auto id = ptr.AsAlloc().allocation_id;
    return frame_->ReadAnonymous(id);
  }

  if (ptr.IsVar()) {
    auto symbol = ptr.AsVar().symbol;

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

    // Resolve through port bindings (output port -> target signal)
    auto [target_symbol, target_instance] = ResolveBinding(symbol);

    // Read from flat storage (either resolved target or original symbol)
    auto actual_symbol = (target_instance != nullptr) ? target_symbol : symbol;
    return simulation_context_->variable_store.Read(actual_symbol);
  }

  if (ptr.IsIndex()) {
    const auto& idx_ptr = ptr.AsIndex();

    // Read the container through the base pointer
    auto container = ReadPointer(*idx_ptr.base);

    // Compute actual index (adjust for lower bound)
    auto actual_idx =
        ComputeArrayIndex(container, static_cast<int64_t>(idx_ptr.index));

    // Return the element
    if (container.IsArray()) {
      return container.AsArray()[actual_idx].DeepCopy();
    }
    return container.AsQueue()[actual_idx].DeepCopy();
  }

  if (ptr.IsField()) {
    const auto& field_ptr = ptr.AsField();

    // Read the struct through the base pointer
    auto aggregate = ReadPointer(*field_ptr.base);

    // Return the field value
    return aggregate.GetField(field_ptr.field_id).DeepCopy();
  }

  if (ptr.IsSlice()) {
    const auto& slice_ptr = ptr.AsSlice();

    // Read the container through the base pointer
    auto container = ReadPointer(*slice_ptr.base);
    assert(container.IsTwoState());

    size_t offset = slice_ptr.offset;
    size_t width = slice_ptr.width;

    // Extract bits from container
    if (width <= 64) {
      uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
      uint64_t extracted = 0;
      if (container.IsWide()) {
        auto shifted = container.AsWideBit().ShiftRightLogical(offset);
        extracted = shifted.GetWord(0) & mask;
      } else {
        extracted = (container.AsNarrow().AsUInt64() >> offset) & mask;
      }
      return RuntimeValue::IntegralUnsigned(extracted, width);
    }
    // Wide extraction
    auto wide_value =
        container.IsWide()
            ? container.AsWideBit()
            : common::WideBit::FromUInt64(container.AsNarrow().AsUInt64(), 2);
    auto extracted = wide_value.ExtractSlice(offset, width);
    return RuntimeValue::IntegralWide(std::move(extracted), width, false);
  }

  throw common::InternalError("interpreter", "unsupported pointer kind");
}

void InstructionContext::WritePointer(
    const PointerValue& ptr, const RuntimeValue& value, bool is_non_blocking) {
  if (ptr.IsAlloc()) {
    if (is_non_blocking) {
      throw common::InternalError(
          "interpreter", "non-blocking assignment to anonymous storage");
    }
    auto id = ptr.AsAlloc().allocation_id;
    frame_->WriteAnonymous(id, value.DeepCopy());
    return;
  }

  if (ptr.IsVar()) {
    auto symbol = ptr.AsVar().symbol;

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

    // Resolve through port bindings (output port -> target signal)
    auto [target_symbol, target_instance] = ResolveBinding(symbol);
    auto actual_symbol = (target_instance != nullptr) ? target_symbol : symbol;

    // Write to flat storage
    if (!is_non_blocking) {
      simulation_context_->variable_store.Write(actual_symbol, actual_value);
      effect_->RecordVariableModification(actual_symbol);
    } else {
      effect_->RecordNbaAction(
          {.symbol = actual_symbol,
           .value = actual_value,
           .array_index = std::nullopt});
    }
    return;
  }

  if (ptr.IsIndex()) {
    const auto& idx_ptr = ptr.AsIndex();

    // Read the container through the base pointer
    auto container = ReadPointer(*idx_ptr.base);

    // Compute actual index
    auto actual_idx =
        ComputeArrayIndex(container, static_cast<int64_t>(idx_ptr.index));

    // For anonymous allocations, no sensitivity tracking needed
    if (!ptr.HasRootSymbol()) {
      if (is_non_blocking) {
        throw common::InternalError(
            "interpreter", "non-blocking assignment to anonymous storage");
      }
      // Blocking: modify element and write back through base pointer
      if (container.IsArray() || container.IsQueue()) {
        container.SetElement(actual_idx, value);
      }
      WritePointer(*idx_ptr.base, container, false);
      return;
    }

    // Get root symbol for sensitivity tracking
    auto root_symbol = ptr.GetRootSymbol();

    if (is_non_blocking) {
      // Resolve through port bindings for the root symbol
      auto [target_symbol, target_instance] = ResolveBinding(root_symbol);
      auto actual_symbol =
          (target_instance != nullptr) ? target_symbol : root_symbol;
      effect_->RecordNbaAction(
          {.symbol = actual_symbol,
           .value = value.DeepCopy(),
           .array_index = actual_idx});
      return;
    }

    // Blocking: modify element and write back through base pointer
    if (container.IsArray() || container.IsQueue()) {
      container.SetElement(actual_idx, value);
    }
    WritePointer(*idx_ptr.base, container, false);
    return;
  }

  if (ptr.IsField()) {
    const auto& field_ptr = ptr.AsField();

    // Read the struct through the base pointer
    auto aggregate = ReadPointer(*field_ptr.base);

    // For anonymous allocations, no sensitivity tracking needed
    if (!ptr.HasRootSymbol()) {
      if (is_non_blocking) {
        throw common::InternalError(
            "interpreter", "non-blocking assignment to anonymous storage");
      }
      // Blocking: modify field and write back through base pointer
      aggregate.SetField(field_ptr.field_id, value);
      WritePointer(*field_ptr.base, aggregate, false);
      return;
    }

    // Get root symbol for sensitivity tracking
    auto root_symbol = ptr.GetRootSymbol();

    if (is_non_blocking) {
      // Resolve through port bindings for the root symbol
      auto [target_symbol, target_instance] = ResolveBinding(root_symbol);
      auto actual_symbol =
          (target_instance != nullptr) ? target_symbol : root_symbol;
      // For struct fields, we record NBA without array_index
      // The whole struct will be replaced
      aggregate.SetField(field_ptr.field_id, value);
      effect_->RecordNbaAction(
          {.symbol = actual_symbol,
           .value = aggregate,
           .array_index = std::nullopt});
      return;
    }

    // Blocking: modify field and write back through base pointer
    aggregate.SetField(field_ptr.field_id, value);
    WritePointer(*field_ptr.base, aggregate, false);
    return;
  }

  if (ptr.IsSlice()) {
    const auto& slice_ptr = ptr.AsSlice();

    // Read the container through the base pointer
    auto container = ReadPointer(*slice_ptr.base);
    assert(container.IsTwoState());

    size_t offset = slice_ptr.offset;
    size_t width = slice_ptr.width;

    // Insert bits into container (read-modify-write)
    size_t storage_width = container.type.GetBitWidth();
    bool storage_is_signed = container.type.IsSigned();
    bool storage_is_wide = container.IsWide();
    bool slice_is_wide = width > 64;

    RuntimeValue modified;
    if (!storage_is_wide && !slice_is_wide) {
      uint64_t slice_mask = common::MakeBitMask(static_cast<uint32_t>(width));
      uint64_t clear_mask = ~(slice_mask << offset);
      uint64_t merged = (container.AsNarrow().AsUInt64() & clear_mask) |
                        ((value.AsNarrow().AsUInt64() & slice_mask) << offset);
      modified = storage_is_signed
                     ? RuntimeValue::IntegralSigned(
                           static_cast<int64_t>(merged), storage_width)
                     : RuntimeValue::IntegralUnsigned(merged, storage_width);
    } else {
      // Wide case - use WideBit operations
      size_t storage_words = common::wide_ops::WordsForBits(storage_width);
      auto current_wide =
          storage_is_wide ? container.AsWideBit()
                          : common::WideBit::FromUInt64(
                                container.AsNarrow().AsUInt64(), storage_words);
      auto value_wide = slice_is_wide
                            ? value.AsWideBit()
                            : common::WideBit::FromUInt64(
                                  value.AsNarrow().AsUInt64(), storage_words);
      auto merged = current_wide.InsertSlice(value_wide, offset, width);
      modified = RuntimeValue::IntegralWide(
          std::move(merged), storage_width, storage_is_signed);
    }

    // Write modified container back through base pointer
    WritePointer(*slice_ptr.base, modified, is_non_blocking);
    return;
  }

  throw common::InternalError("interpreter", "unsupported pointer kind");
}

auto InstructionContext::GetOperandValue(const lir::Operand& operand) const
    -> RuntimeValue {
  // Operands are always temps now (SSA form)
  return GetTemp(operand.AsTempRef());
}

auto InstructionContext::EvalUnaryOp(
    const lir::Operand& operand, lir::TempRef result,
    const std::function<RuntimeValue(RuntimeValue)>& op) -> InstructionResult {
  const auto result_value = op(GetTemp(operand.AsTempRef()));
  temp_table_->Write(result, result_value);
  return InstructionResult::Continue();
}

auto InstructionContext::EvalBinaryOp(
    const lir::Operand& lhs, const lir::Operand& rhs, lir::TempRef result,
    const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
    -> InstructionResult {
  const auto result_value =
      op(GetTemp(lhs.AsTempRef()), GetTemp(rhs.AsTempRef()));
  temp_table_->Write(result, result_value);
  return InstructionResult::Continue();
}

void InstructionContext::WriteTemp(lir::TempRef result, RuntimeValue value) {
  temp_table_->Write(result, std::move(value));
}

auto InstructionContext::AllocateAnonymous(RuntimeValue initial) -> uint64_t {
  return frame_->AllocateAnonymous(std::move(initial));
}

}  // namespace lyra::interpreter
