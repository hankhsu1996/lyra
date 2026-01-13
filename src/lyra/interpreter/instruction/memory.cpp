#include "lyra/interpreter/instruction/memory.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

auto HandleMemoryOps(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  switch (instr.kind) {
    case lir::InstructionKind::kConstant: {
      assert(instr.constant.has_value());
      assert(instr.result.has_value());

      RuntimeValue value = RuntimeValue::FromConstant(*instr.constant);
      ctx.WriteTemp(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kResolveVar: {
      assert(instr.symbol.has_value());
      assert(instr.result.has_value());

      // Create a pointer value pointing to the variable
      auto ptr = PointerValue::Var(*instr.symbol);
      auto ptr_type = (*instr.result)->type;
      ctx.WriteTemp(instr.result.value(), RuntimeValue::Pointer(ptr_type, ptr));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoad: {
      assert(instr.result.has_value());
      assert(instr.temp_operands.size() == 1);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      assert(ptr_value.IsPointer());

      RuntimeValue loaded = ctx.ReadPointer(ptr_value.AsPointer());
      ctx.WriteTemp(instr.result.value(), std::move(loaded));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStore: {
      assert(instr.temp_operands.size() == 2);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(ptr_value.IsPointer());

      ctx.WritePointer(ptr_value.AsPointer(), value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreNBA: {
      assert(instr.temp_operands.size() == 2);

      const auto& ptr_value = ctx.GetTemp(instr.temp_operands[0]);
      const auto& value = ctx.GetTemp(instr.temp_operands[1]);
      assert(ptr_value.IsPointer());

      ctx.WritePointer(ptr_value.AsPointer(), value, true);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElement: {
      assert(instr.operands.size() == 3);

      auto index_value = ctx.GetTemp(instr.operands[1].AsTempRef());
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());
      auto element_value = ctx.GetTemp(instr.operands[2].AsTempRef());

      ctx.StoreElement(instr.operands[0], index, element_value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreElementNonBlocking: {
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      auto index_value = ctx.GetTemp(instr.operands[1].AsTempRef());
      assert(!index_value.IsWide() && "element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());
      auto element_value = ctx.GetTemp(instr.operands[2].AsTempRef());

      ctx.StoreElement(instr.operands[0], index, element_value, true);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kAllocate: {
      assert(instr.result.has_value());
      const auto& result_type = (*instr.result)->type;

      if (instr.operands.empty()) {
        // Fixed-size allocation: use DefaultValueForType
        auto value = RuntimeValue::DefaultValueForType(result_type);
        ctx.WriteTemp(instr.result.value(), std::move(value));
      } else {
        // Dynamic allocation with size (and optional init)
        auto size_val = ctx.GetOperandValue(instr.operands[0]);
        auto size = static_cast<size_t>(size_val.AsNarrow().AsInt64());

        const auto& dyn_data =
            std::get<common::DynamicArrayData>(result_type.data);
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
            RuntimeValue::Array(result_type, std::move(elements)));
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadPackedBits: {
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsTemp());
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = ctx.GetTemp(instr.operands[0].AsTempRef());
      auto offset_value = ctx.GetTemp(instr.operands[1].AsTempRef());
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      const auto& result_type = instr.result_type.value();
      assert(result_type.IsBitvector());
      size_t width = result_type.GetBitWidth();
      bool is_signed = result_type.IsSigned();

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

      auto offset_value = ctx.GetTemp(instr.operands[1].AsTempRef());
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      auto new_value = ctx.GetTemp(instr.operands[2].AsTempRef());

      const auto& slice_type = instr.result_type.value();
      size_t slice_width = slice_type.GetBitWidth();

      size_t storage_width = current.type.GetBitWidth();
      bool storage_is_signed = current.type.IsSigned();
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
        result = storage_is_signed
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
            std::move(merged), storage_width, storage_is_signed);
      }
      ctx.StoreVariable(instr.operands[0], result, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kMove: {
      assert(instr.operands.size() == 1);
      assert(instr.result.has_value());

      const auto value = ctx.GetTemp(instr.operands[0].AsTempRef());
      ctx.WriteTemp(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    default:
      throw common::InternalError(
          "interpreter", "unhandled instruction kind in memory handler");
  }
}

}  // namespace lyra::interpreter
