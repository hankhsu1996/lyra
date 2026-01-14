#include "lyra/interpreter/instruction/context.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <utility>

#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/address.hpp"
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

auto InstructionContext::GetTempType(lir::TempRef temp) const
    -> const common::Type& {
  // Look up type from per-unit metadata
  if (!frame_->call_stack.empty()) {
    // In function context - use function's temps
    const auto& func = *frame_->call_stack.back().function;
    return *func.temps[temp.id].type;
  }
  // In process context - use process's temps
  return *frame_->process->temps[temp.id].type;
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

/// Read address root and return a copy of the value.
auto ReadAddressRoot(
    const Address& addr, const ProcessFrame& frame,
    const InstructionContext& ctx, SimulationContext& sim_ctx) -> RuntimeValue {
  if (addr.IsAlloc()) {
    return frame.ReadAnonymous(addr.GetAllocationId()).DeepCopy();
  }

  auto symbol = addr.GetSymbol();

  // Check function-local variables first (parameters and locals)
  if (!frame.call_stack.empty()) {
    const auto& call_frame = frame.call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      return it->second.DeepCopy();
    }
  }

  // Check process-local next
  if (frame.variable_table.Exists(symbol)) {
    return frame.variable_table.Read(symbol);  // Already returns by value
  }

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ctx.ResolveBinding(symbol);

  // Read from flat storage (either resolved target or original symbol)
  auto actual_symbol = (target_instance != nullptr) ? target_symbol : symbol;
  return sim_ctx.variable_store.Read(actual_symbol).DeepCopy();
}

/// Compute array index with bounds checking.
/// Returns the actual index after adjusting for lower_bound.
/// Returns nullopt if out of bounds (SV semantics: OOB reads return X -> 0).
///
/// TODO(hankhsu): Add OOB counter/tracer for observability. Silent OOB can hide
/// real design bugs. Users should be able to detect when OOB access occurs.
auto ComputeArrayIndex(const RuntimeValue& container, uint32_t sv_index)
    -> std::optional<size_t> {
  int32_t lower_bound = 0;
  if (container.type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data =
        std::get<common::UnpackedArrayData>(container.type.data);
    lower_bound = array_data.lower_bound;
  }

  auto signed_idx = static_cast<int64_t>(sv_index) - lower_bound;
  if (signed_idx < 0) {
    return std::nullopt;
  }

  auto actual_idx = static_cast<size_t>(signed_idx);
  size_t container_size = container.IsQueue() ? container.AsQueue().size()
                                              : container.AsArray().size();

  if (actual_idx >= container_size) {
    return std::nullopt;
  }

  return actual_idx;
}

/// Walk the address path on a value and return the final element.
/// Returns default value if any index is out of bounds (SV: OOB -> X -> 0).
auto WalkAddressPath(RuntimeValue root, const Address& addr) -> RuntimeValue {
  RuntimeValue current = std::move(root);

  for (const auto& elem : addr.path) {
    switch (elem.kind) {
      case PathElement::Kind::kField:
        current = current.GetField(elem.value).DeepCopy();
        break;
      case PathElement::Kind::kIndex: {
        auto actual_idx = ComputeArrayIndex(current, elem.value);
        if (!actual_idx) {
          // OOB: return default value for element type
          return RuntimeValue::DefaultValueForType(
              current.type.GetElementType());
        }
        if (current.IsQueue()) {
          current = current.AsQueue()[*actual_idx].DeepCopy();
        } else {
          current = current.AsArray()[*actual_idx].DeepCopy();
        }
        break;
      }
    }
  }
  return current;
}

}  // namespace

auto InstructionContext::ResolveForRead(const Address& addr) const
    -> RuntimeValue {
  RuntimeValue root =
      ReadAddressRoot(addr, *frame_, *this, *simulation_context_);
  return WalkAddressPath(std::move(root), addr);
}

void InstructionContext::ResolveForWrite(
    const Address& addr, const RuntimeValue& value, bool is_non_blocking) {
  // For simple variable writes (empty path), write directly without reading
  // This handles the case where the variable doesn't exist yet
  RuntimeValue root_copy = value.DeepCopy();

  if (!addr.path.empty()) {
    // For writes through a path (field/index), we need to read the root first,
    // modify through the path, then write the whole thing back
    root_copy = ReadAddressRoot(
        Address{.root = addr.root, .path = {}}, *frame_, *this,
        *simulation_context_);

    // Walk path and modify in place
    RuntimeValue* current = &root_copy;
    for (size_t i = 0; i < addr.path.size(); ++i) {
      const auto& elem = addr.path[i];
      bool is_final = (i == addr.path.size() - 1);

      switch (elem.kind) {
        case PathElement::Kind::kField:
          if (is_final) {
            current->SetField(elem.value, value.DeepCopy());
          } else {
            current = &current->AsArray()[elem.value];
          }
          break;
        case PathElement::Kind::kIndex: {
          auto actual_idx = ComputeArrayIndex(*current, elem.value);
          if (!actual_idx) {
            // OOB write: silently ignore (SV semantics)
            return;
          }
          if (is_final) {
            current->SetElement(*actual_idx, value.DeepCopy());
          } else if (current->IsQueue()) {
            current = &current->AsQueue()[*actual_idx];
          } else {
            current = &current->AsArray()[*actual_idx];
          }
          break;
        }
      }
    }
  }

  // Now write the modified root back
  if (addr.IsAlloc()) {
    if (is_non_blocking) {
      throw common::InternalError(
          "interpreter", "non-blocking assignment to anonymous storage");
    }
    frame_->WriteAnonymous(addr.GetAllocationId(), std::move(root_copy));
    return;
  }

  auto symbol = addr.GetSymbol();

  // Check function-local variables first (parameters and locals)
  if (!frame_->call_stack.empty()) {
    auto& call_frame = frame_->call_stack.back();
    auto it = call_frame.local_variables.find(symbol);
    if (it != call_frame.local_variables.end()) {
      it->second = std::move(root_copy);
      return;
    }
  }

  // Check process-local next
  if (frame_->variable_table.Exists(symbol)) {
    frame_->variable_table.Write(symbol, std::move(root_copy));
    return;
  }

  // Resolve through port bindings (output port -> target signal)
  auto [target_symbol, target_instance] = ResolveBinding(symbol);
  auto actual_symbol = (target_instance != nullptr) ? target_symbol : symbol;

  // Write to flat storage
  if (!is_non_blocking) {
    simulation_context_->variable_store.Write(actual_symbol, root_copy);
    effect_->RecordVariableModification(actual_symbol);
  } else {
    // NBA limitation: We only support symbol-level NBA, not path-level NBA.
    // For `arr[i] <= v`, we do read-modify-write immediately and record the
    // whole modified array for deferred commit. True SV semantics would defer
    // the index evaluation and element write, but that requires storing the
    // full address path in NbaAction.
    effect_->RecordNbaAction(
        {.symbol = actual_symbol,
         .value = std::move(root_copy),
         .array_index = std::nullopt});
  }
}

void InstructionContext::ResolveForWriteSlice(
    const Address& addr, const RuntimeValue& value, size_t bit_offset,
    size_t width, bool is_non_blocking) {
  // Read the current value at the address
  RuntimeValue current = ResolveForRead(addr);
  assert(current.IsTwoState());

  // Insert bits into current (read-modify-write)
  size_t storage_width = current.type.GetBitWidth();
  bool storage_is_signed = current.type.IsSigned();
  bool storage_is_wide = current.IsWide();
  bool slice_is_wide = width > 64;

  RuntimeValue modified;
  if (!storage_is_wide && !slice_is_wide) {
    uint64_t slice_mask = common::MakeBitMask(static_cast<uint32_t>(width));
    uint64_t clear_mask = ~(slice_mask << bit_offset);
    uint64_t merged =
        (current.AsNarrow().AsUInt64() & clear_mask) |
        ((value.AsNarrow().AsUInt64() & slice_mask) << bit_offset);
    modified = storage_is_signed
                   ? RuntimeValue::IntegralSigned(
                         static_cast<int64_t>(merged), storage_width)
                   : RuntimeValue::IntegralUnsigned(merged, storage_width);
  } else {
    // Wide case - use WideBit operations
    size_t storage_words = common::wide_ops::WordsForBits(storage_width);
    auto current_wide = storage_is_wide
                            ? current.AsWideBit()
                            : common::WideBit::FromUInt64(
                                  current.AsNarrow().AsUInt64(), storage_words);
    auto value_wide = slice_is_wide
                          ? value.AsWideBit()
                          : common::WideBit::FromUInt64(
                                value.AsNarrow().AsUInt64(), storage_words);
    auto merged = current_wide.InsertSlice(value_wide, bit_offset, width);
    modified = RuntimeValue::IntegralWide(
        std::move(merged), storage_width, storage_is_signed);
  }

  // Write the modified value back
  ResolveForWrite(addr, modified, is_non_blocking);
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
