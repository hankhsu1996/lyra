#include "lyra/interpreter/instruction/memory.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <stdexcept>
#include <vector>

#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

namespace {

/// Compute the actual array index after adjusting for lower bound and checking
/// bounds. Returns the adjusted index or throws DiagnosticException if out of
/// bounds.
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
    throw std::runtime_error(
        fmt::format(
            "array index {} out of bounds (size {})", sv_index,
            array_value.AsArray().size()));
  }

  return actual_idx;
}

}  // namespace

auto HandleMemoryOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  switch (instr.kind) {
    case lir::InstructionKind::kLiteral: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLiteral());
      assert(instr.result.has_value());

      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
      RuntimeValue value = RuntimeValue::FromLiteral(literal);
      ctx.WriteTemp(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadVariable: {
      if (instr.target_symbol != nullptr) {
        const auto value =
            ctx.LoadHierarchical(instr.instance_path, instr.target_symbol);
        ctx.WriteTemp(instr.result.value(), value);
      } else {
        const auto& src_variable = ctx.ReadVariable(instr.operands[0]);
        ctx.WriteTemp(instr.result.value(), src_variable);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariable: {
      if (instr.target_symbol != nullptr) {
        const auto value = ctx.GetTemp(instr.operands[0]);
        ctx.StoreHierarchical(
            instr.instance_path, instr.target_symbol, value, false);
      } else {
        const auto variable = instr.operands[0];
        const auto value = ctx.GetTemp(instr.operands[1]);
        assert(variable.IsVariable());
        ctx.StoreVariable(variable, value, false);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariableNonBlocking: {
      if (instr.target_symbol != nullptr) {
        const auto value = ctx.GetTemp(instr.operands[0]);
        ctx.StoreHierarchical(
            instr.instance_path, instr.target_symbol, value, true);
      } else {
        const auto variable = instr.operands[0];
        const auto value = ctx.GetTemp(instr.operands[1]);
        assert(variable.IsVariable());
        ctx.StoreVariable(variable, value, true);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadElement: {
      assert(instr.operands.size() == 2);
      assert(instr.result.has_value());

      RuntimeValue base_value;
      if (instr.operands[0].IsVariable()) {
        base_value = ctx.ReadVariable(instr.operands[0]);
      } else {
        base_value = ctx.GetTemp(instr.operands[0]);
      }

      auto index_value = ctx.GetTemp(instr.operands[1]);
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      if (base_value.IsArray()) {
        auto actual_idx =
            ComputeArrayIndex(base_value, static_cast<int64_t>(index));
        ctx.WriteTemp(instr.result.value(), base_value.GetElement(actual_idx));
      } else {
        assert(base_value.IsUnpackedStruct() || base_value.IsUnpackedUnion());
        ctx.WriteTemp(instr.result.value(), base_value.GetField(index));
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElement: {
      assert(instr.operands.size() == 3);

      auto index_value = ctx.GetTemp(instr.operands[1]);
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());
      auto element_value = ctx.GetTemp(instr.operands[2]);

      if (instr.operands[0].IsVariable()) {
        const auto* symbol = std::get<lir::SymbolRef>(instr.operands[0].value);
        auto& frame = ctx.GetFrame();
        auto& effect = ctx.GetEffect();
        const auto& instance = ctx.GetInstanceContext();

        // Check if this is a local variable (no triggers needed)
        bool is_local = false;
        if (!frame.call_stack.empty()) {
          auto& call_frame = frame.call_stack.back();
          is_local = call_frame.local_variables.contains(symbol);
        }
        if (!is_local) {
          is_local = frame.variable_table.Exists(symbol);
        }

        auto aggregate_value = ctx.ReadVariable(instr.operands[0]);

        // Snapshot old value BEFORE modification for non-local variables
        if (!is_local) {
          auto [target_symbol, target_instance] = ctx.ResolveBinding(symbol);
          if (target_instance != nullptr) {
            target_instance->UpdatePrevious(target_symbol, aggregate_value);
          } else if (instance != nullptr) {
            instance->UpdatePrevious(symbol, aggregate_value);
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
          auto [target_symbol, target_instance] = ctx.ResolveBinding(symbol);
          if (target_instance != nullptr) {
            effect.RecordVariableModification(target_symbol, target_instance);
          } else if (instance != nullptr) {
            effect.RecordVariableModification(symbol, instance);
          } else {
            effect.RecordVariableModification(symbol);
          }
        }
      } else {
        auto aggregate_value = ctx.GetTemp(instr.operands[0]);

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
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      const auto* symbol = std::get<lir::SymbolRef>(instr.operands[0].value);
      auto aggregate_value = ctx.ReadVariable(instr.operands[0]);
      auto& effect = ctx.GetEffect();
      const auto& instance = ctx.GetInstanceContext();

      auto index_value = ctx.GetTemp(instr.operands[1]);
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      size_t actual_idx = index;
      if (aggregate_value.IsArray()) {
        actual_idx =
            ComputeArrayIndex(aggregate_value, static_cast<int64_t>(index));
      }

      auto element_value = ctx.GetTemp(instr.operands[2]);

      auto [target_symbol, target_instance] = ctx.ResolveBinding(symbol);

      if (target_instance != nullptr) {
        effect.RecordNbaAction(
            {.variable = target_symbol,
             .value = element_value,
             .instance = target_instance,
             .array_index = actual_idx});
      } else if (instance != nullptr) {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = element_value,
             .instance = instance,
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
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto aggregate_value =
          RuntimeValue::DefaultValueForType(*instr.result_type);
      ctx.WriteTemp(instr.result.value(), std::move(aggregate_value));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kNewDynamicArray: {
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      auto size_val = ctx.GetOperandValue(instr.operands[0]);
      auto size = static_cast<size_t>(size_val.AsNarrow().AsInt64());

      const auto& dyn_data =
          std::get<common::DynamicArrayData>(instr.result_type->data);
      const auto& elem_type = *dyn_data.element_type;

      std::vector<RuntimeValue> elements;
      elements.reserve(size);

      if (instr.operands.size() > 1) {
        const auto& init = ctx.GetOperandValue(instr.operands[1]);
        const auto& init_arr = init.AsArray();
        for (size_t i = 0; i < size; ++i) {
          if (i < init_arr.size()) {
            elements.push_back(init_arr[i].DeepCopy());
          } else {
            elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
          }
        }
      } else {
        for (size_t i = 0; i < size; ++i) {
          elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
        }
      }

      ctx.WriteTemp(
          instr.result.value(),
          RuntimeValue::Array(*instr.result_type, std::move(elements)));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadPackedBits: {
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsTemp());
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = ctx.GetTemp(instr.operands[0]);
      auto offset_value = ctx.GetTemp(instr.operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      const auto& result_type = instr.result_type.value();
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
        auto wide_value = value.IsWide() ? value.AsWideBit()
                                         : common::WideBit::FromUInt64(
                                               value.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(bit_offset, width);
        result =
            RuntimeValue::IntegralWide(std::move(extracted), width, is_signed);
      }
      ctx.WriteTemp(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStorePackedBits: {
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());
      assert(instr.operands[1].IsTemp());
      assert(instr.operands[2].IsTemp());
      assert(instr.result_type.has_value());

      auto current = ctx.ReadVariable(instr.operands[0]);
      assert(current.IsTwoState());

      auto offset_value = ctx.GetTemp(instr.operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      auto new_value = ctx.GetTemp(instr.operands[2]);

      const auto& slice_type = instr.result_type.value();
      size_t slice_width = slice_type.GetBitWidth();

      const auto& current_data =
          std::get<common::IntegralData>(current.type.data);
      size_t storage_width = current_data.bit_width;
      bool storage_is_wide = current.IsWide();
      bool slice_is_wide = slice_width > 64;

      RuntimeValue result;
      if (!storage_is_wide && !slice_is_wide) {
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
      ctx.StoreVariable(instr.operands[0], result, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kMove: {
      assert(instr.operands.size() == 1);
      assert(instr.result.has_value());

      const auto value = ctx.GetTemp(instr.operands[0]);
      ctx.WriteTemp(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in memory handler");
  }
}

}  // namespace lyra::interpreter
